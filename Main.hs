{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import System.Environment
import Data.Conduit
import Data.Conduit.Process
import Data.Conduit.Combinators as DCC
import Data.Conduit.List as DCL
import Control.Monad.Trans.Resource
import System.Exit
import Data.Time
import System.IO
import Text.Printf
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import Data.Time.ISO8601
import System.Directory
import Control.Monad
import Network.Wreq
import Data.Aeson
import qualified System.Process as SP

main :: IO ()
main = do
  args <- getArgs
  onFailure <- getOnFailureHandler
  withFile "gradle-loop.log" AppendMode $ \hLog -> do
    hSetBuffering hLog LineBuffering
    runUntilFailure onFailure (logAndPrint hLog) args

getOnFailureHandler :: IO (B.ByteString -> B.ByteString -> IO ())
getOnFailureHandler = do
  maybeSlackWebhookUser <- lookupEnv "GRADLE_LOOP_SLACK_WEBHOOK_USER"
  maybeSlackWebhookUri  <- lookupEnv "GRADLE_LOOP_SLACK_WEBHOOK_URI"
  case (maybeSlackWebhookUser, maybeSlackWebhookUri) of
    (Just slackWebhookUser, Just slackWebhookUri) -> return $ \gitRevision bs -> void $ post slackWebhookUri $ object
      [ "text" .= String ("<" <> T.pack slackWebhookUser <> "> tests failed on `"
        <> T.decodeUtf8With T.lenientDecode gitRevision
        <> "`, use these args to reproduce:"
        <> T.decodeUtf8With T.lenientDecode bs)
      ]
    _ -> do
      putStrLn "GRADLE_LOOP_SLACK_WEBHOOK_USER or GRADLE_LOOP_SLACK_WEBHOOK_URI unset, notifications disabled"
      return $ \_ _ -> return ()

logAndPrint :: Handle -> String -> IO ()
logAndPrint h msg = do
  now <- getCurrentTime
  let timeStampedMsg = printf "[%s] %s" (formatISO8601Millis now) msg
  hPutStrLn h timeStampedMsg
  putStrLn    timeStampedMsg

getGitRevision :: IO B.ByteString
getGitRevision = do
  gitRevParseResult <- runResourceT $
      sourceProcessWithStreams
        (proc "git" ["rev-parse", "HEAD"])
        (return ())
        (DCC.linesUnboundedAscii .| DCC.last)
        (return ())

  return $ case gitRevParseResult of
    (ExitSuccess, Just gitRev, ()) -> gitRev
    _                              -> "unknown"

resetGitBranch :: String -> IO ()
resetGitBranch rev = do
  (exitCode, (), ()) <- runResourceT $
    sourceProcessWithStreams
        (proc "git" ["reset", "--hard", "--recurse-submodules", rev])
        (return ())
        (awaitForever $ const $ return ())
        (return ())
  case exitCode of
    ExitSuccess -> return ()
    _ -> error $ "failed: git reset --hard " ++ rev

runUntilFailure :: (B.ByteString -> B.ByteString -> IO ()) -> (String -> IO ()) -> [String] -> IO ()
runUntilFailure onFailure writeLog args = loop (0::Int) Nothing
  where
  loop iteration maybePreviousGitRevision = do
    maybeBranch <- lookupEnv "BRANCH"
    branchDescription <- case maybeBranch of
        Nothing -> return ""
        Just branch -> do
            let rev = "origin/" ++ branch
            resetGitBranch rev
            return $ show rev ++ " = "

    gitRevision <- getGitRevision
    writeLog $ printf "[%4d] starting on %s%s with args %s" iteration branchDescription (show gitRevision) (show args)

    {-
        If $GRADLE_LOOP_PRECLEAN command is present, run it first.
    -}
    maybe (return ()) SP.callCommand =<< lookupEnv "GRADLE_LOOP_PRECLEAN"

    {-
        If $GRADLE_LOOP_STRESSOR command is present, run it via "sh -c 'exec $GRADLE_LOOP_STRESSOR'".
        Only do this if the git revision is unchanged from the last time because if the git revision
        changes then we must recompile things.
        Use 'exec' to replace the sh process with the actual stressor so that it receives a SIGTERM
        when Gradle completes
    -}
    runWithStressor <- let getStressor = if maybePreviousGitRevision == Just gitRevision then lookupEnv "GRADLE_LOOP_STRESSOR" else return Nothing
      in maybe id (\stressor c -> withCreateProcess (shell $ "exec " ++ stressor) $ \_ _ _ _ -> c) <$> getStressor

    startTime <- getCurrentTime
    exitCode <- runWithStressor $ do
      (exitCode, (), ()) <- runResourceT $
        sourceProcessWithStreams
          (proc "./gradlew" $ ("-Dtests.gradle-loop-iteration=" ++ show iteration) : args)
            { new_session   = False
            , delegate_ctlc = True
            }
          (return ()) -- stdin
          (sinkFile "testoutput-stdout-wip.log")
          (DCC.linesUnboundedAscii
            .| collectReproduceWith
            .| DCC.unlinesAscii
            .| sinkFile "testoutput-stderr-wip.log")

      endTime <- getCurrentTime
      writeLog $ printf "[%4d] finished with %s in %s" iteration (show exitCode) (show $ diffUTCTime endTime startTime)
      return exitCode

    forM_ ["stdout", "stderr"] $ \fd -> renameFile ("testoutput-" ++ fd ++ "-wip.log") 
                                                   ("testoutput-" ++ fd ++ ".log")

    case exitCode of
      ExitSuccess   -> loop (iteration + 1) (Just gitRevision)
      ExitFailure c -> do
        writeLog $ printf "tar zcvf testoutput-%s.tar.gz --force-local --transform 's/^/testoutput-%s\\//' testoutput-std*.log"
                    (formatISO8601Millis startTime)
                    (formatISO8601Millis startTime)
        runResourceT $ runConduit
          $  sourceFile "testoutput-stderr.log"
          .| DCC.stdout

        when (c < 128) $ do
          notificationMessageLines <- runResourceT $ runConduit $
            do  sourceFile "testoutput-stderr.log"
                  .| DCC.linesUnboundedAscii
                  .| do DCC.dropWhile (/= "REPRODUCE WITH:")
                        DCC.drop 1
                        yield "```"
                        DCC.take 5
                        yield "```"
                        yield "Error output follows:"
                        yield "```"
                  .| DCC.unlinesAscii

                sourceFile "testoutput-stderr.log"
                  .| DCC.linesUnboundedAscii
                  .| DCC.take 30
                  .| DCC.unlinesAscii
                  .| DCC.takeE 3500

                yield "```"

            .| DCL.consume

          onFailure gitRevision $ B.concat notificationMessageLines

collectReproduceWith :: Monad m => ConduitT B.ByteString B.ByteString m ()
collectReproduceWith = go [] []
  where
    lineAfter :: B.ByteString -> B.ByteString -> Maybe B.ByteString
    lineAfter s = dropMatch . snd . B.breakSubstring s
      where
        dropMatch bs | B.null bs = Nothing
                     | otherwise = Just $ B.drop (B.length s) bs

    lineAfterReproduceWith :: B.ByteString -> Maybe B.ByteString
    lineAfterReproduceWith = lineAfter "REPRODUCE WITH: ./gradlew "

    lineAfterSeed :: B.ByteString -> Maybe B.ByteString
    lineAfterSeed = lineAfter "__randomizedtesting.SeedInfo.seed(["

    testsSeedArg :: B.ByteString
    testsSeedArg = " -Dtests.seed="

    breakAtTestsSeed :: B.ByteString -> (B.ByteString, B.ByteString)
    breakAtTestsSeed = B.breakSubstring testsSeedArg

    testMethodSeedArg :: B.ByteString
    testMethodSeedArg = " {seed=["

    breakAtTestMethodSeedArg :: B.ByteString -> (B.ByteString, B.ByteString)
    breakAtTestMethodSeedArg = B.breakSubstring testMethodSeedArg

    go fixedReproduceWithLines pendingReproduceWithLines = await >>= \case
      Nothing -> case fixedReproduceWithLines ++ pendingReproduceWithLines of
        [] -> return ()
        reproLines -> do
          yield ""
          yield "REPRODUCE WITH:"
          yieldMany reproLines
      Just l -> do
        yield l
        case (lineAfterReproduceWith l, lineAfterSeed l) of
          (Just r, _) -> go fixedReproduceWithLines (pendingReproduceWithLines ++ [r])
          (_, Just s) -> go (fixedReproduceWithLines ++ fmap (fixReproduceWithLine s) pendingReproduceWithLines) []
          _           -> go fixedReproduceWithLines pendingReproduceWithLines

    fixTestMethodName :: B.ByteString -> B.ByteString
    fixTestMethodName cmdLine = beforeMethodSeed <> B.drop 1 (B.dropWhile (/= 0x7d) methodSeedAndAfter)
      where
      (beforeMethodSeed, methodSeedAndAfter) = breakAtTestMethodSeedArg cmdLine

    fixReproduceWithLine :: B.ByteString -> B.ByteString -> B.ByteString
    fixReproduceWithLine seedAndJunk reproduceWithLine = fixTestMethodName $ beforeTestsSeed <> testsSeedArg <> seed <> after
      where
      (beforeTestsSeed, testsSeedAndAfter) = breakAtTestsSeed reproduceWithLine
      after = B.dropWhile (/= 0x20) $ B.drop (B.length testsSeedArg) testsSeedAndAfter
      seed = B.takeWhile (/= 0x5d) seedAndJunk
