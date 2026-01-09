{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad
import Control.Monad.Trans.Resource
import Data.Aeson
import Data.Array.IO
import Data.Array.Unboxed
import Data.Conduit
import Data.Conduit.Combinators as DCC (sourceFile, sinkFile, yieldMany)
import Data.Conduit.Process
import Data.IORef
import Data.Time
import Data.Time.ISO8601
import System.Directory
import System.Environment
import System.Exit
import System.IO
import Text.Printf

import qualified Data.Conduit.Combinators as DCC
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import qualified System.Process as SP

bisectCandidatesFile :: FilePath
bisectCandidatesFile = "gradle-loop-bisect-candidates.txt"

bisectHistoryFile :: FilePath
bisectHistoryFile = "gradle-loop-bisect-history.json"

data BisectHistoryEntry = BisectHistoryEntry
  { _bisectHistoryEntryCommit    :: String
  , _bisectHistoryEntrySuccesses :: Int
  , _bisectHistoryEntryFailures  :: Int
  } deriving (Show, Eq)

instance ToJSON BisectHistoryEntry where
  toJSON BisectHistoryEntry{..} = object
    [ "commit"    .= _bisectHistoryEntryCommit
    , "successes" .= _bisectHistoryEntrySuccesses
    , "failures"  .= _bisectHistoryEntryFailures
    ]

instance FromJSON BisectHistoryEntry where
   parseJSON = withObject "BisectHistoryEntry" $ \v -> BisectHistoryEntry
        <$> v .: "commit"
        <*> v .: "successes"
        <*> v .: "failures"

data BisectCommitState = BisectCommitState
  { _bisectCommitIndex     :: Int
  , _bisectCommit          :: String
  , _bisectCommitSuccesses :: Int
  , _bisectCommitFailures  :: Int
  } deriving (Show, Eq)

data BisectState = BisectState
  { _bisectStateCommits :: IOArray Int BisectCommitState
  , _bisectCurrentCommit :: IORef (Maybe BisectCommitState)
  }

getHistoryEntries :: BisectState -> IO [BisectHistoryEntry]
getHistoryEntries BisectState{..} = map historyEntryFromState <$> getElems _bisectStateCommits
  where
    historyEntryFromState BisectCommitState{..} = BisectHistoryEntry _bisectCommit _bisectCommitSuccesses _bisectCommitFailures

writeBisectState :: BisectState -> IO ()
writeBisectState bisectState = encodeFile bisectHistoryFile =<< getHistoryEntries bisectState

printBisectState :: BisectState -> IO ()
printBisectState bisectState = mapM_ (putStrLn . show) =<< getAssocs (_bisectStateCommits bisectState)

loadBisectState :: [String] -> IO BisectState
loadBisectState candidateCommits = do
  hasBisectState <- doesFileExist bisectHistoryFile
  historyCommitsMap <- if hasBisectState
    then do
      Just historyEntries <- decodeFileStrict bisectHistoryFile
      let historyCommitsSet   = S.fromList $ map _bisectHistoryEntryCommit historyEntries
          candidateCommitsSet = S.fromList candidateCommits
      unless (historyCommitsSet `S.isSubsetOf` candidateCommitsSet) $ error $ "mismatch between " ++ bisectHistoryFile ++ " and " ++ bisectCandidatesFile
      return $ M.fromList [(_bisectHistoryEntryCommit e, e) | e <- historyEntries]
    else return M.empty
  let historyEntries = map (\c -> M.findWithDefault (BisectHistoryEntry c 0 0) c historyCommitsMap) candidateCommits
  BisectState
    <$> newListArray (0, length candidateCommits - 1)
          [ BisectCommitState i _bisectHistoryEntryCommit _bisectHistoryEntrySuccesses _bisectHistoryEntryFailures
          | (i, BisectHistoryEntry{..}) <- zip [0..] historyEntries
          ]
    <*> newIORef Nothing

main :: IO ()
main = do
  args <- getArgs

  hasBisectCandidates <- doesFileExist bisectCandidatesFile
  if hasBisectCandidates
    then do
      putStrLn $ "running Bayesian bisection using " ++ bisectCandidatesFile ++ " and " ++ bisectHistoryFile
      commits <- runResourceT $ sourceToList
        $  sourceFile bisectCandidatesFile
        .| DCC.linesUnboundedAscii
        .| DCC.map (B.takeWhile (/= 0x20))
        .| DCC.filter ((== 40) . B.length)
        .| DCC.map (T.unpack . T.decodeUtf8)
      bisectState <- loadBisectState commits
      runBayesianBisection bisectState args
    else do
      commitSelector <- makeCommitSelector
      _ <- logRunUntilFailure commitSelector args
      return ()

-- implements https://davecturner.github.io/2024/11/11/bayesian-bisection.html except:
-- keeps trying known-bad commit to get a better sense of failure rate, but only if
-- the number of runs on that commit is less than the number of runs on the
-- believed-good commits (div 10, so we try each commit multiple times before switching)
runBayesianBisection :: BisectState -> [String] -> IO ()
runBayesianBisection bisectState args = do
  exitCode <- logRunUntilFailure commitSelector args
  if 128 <= exitCode
    then return () -- exit on a signal, not a failure
    else do
      maybeCurrentCommit <- readIORef (_bisectCurrentCommit bisectState)
      case maybeCurrentCommit of
        Nothing -> error "failed without setting current commit"
        Just currentCommit -> do
          writeIORef (_bisectCurrentCommit bisectState) Nothing
          modifyArray (_bisectStateCommits bisectState) (_bisectCommitIndex currentCommit) (\bcs@BisectCommitState{..} -> bcs {_bisectCommitFailures = _bisectCommitFailures + 1})
          writeBisectState bisectState
          runBayesianBisection bisectState args

  where
    commitSelector = do
      maybeCurrentCommit <- readIORef (_bisectCurrentCommit bisectState)
      case maybeCurrentCommit of
        Nothing -> return ()
        Just currentCommit -> do
          modifyArray (_bisectStateCommits bisectState) (_bisectCommitIndex currentCommit) (\bcs@BisectCommitState{..} -> bcs {_bisectCommitSuccesses = _bisectCommitSuccesses + 1})
          writeBisectState bisectState
      distribution <- getPosteriorDistribution bisectState
      let total = sum $ elems distribution
          cumulativeDistribution = scanl (+) 0.0 $ elems distribution
      (_,ub) <- getBounds (_bisectStateCommits bisectState)
      let target = total / 2.0
      let proposedCommitIndex = min ub $ length $ filter (<target) cumulativeDistribution
          proposedKnownBad = all (==0.0) $ take proposedCommitIndex cumulativeDistribution
      commitIndex <- if proposedKnownBad && proposedCommitIndex < ub
        then do
          proposedRuns <- (\BisectCommitState{..} -> _bisectCommitSuccesses + _bisectCommitFailures) <$> readArray (_bisectStateCommits bisectState) proposedCommitIndex
          otherRuns <- (sum . drop (proposedCommitIndex + 1) . map _bisectCommitSuccesses) <$> getElems (_bisectStateCommits bisectState)
          return $ if div otherRuns 10 <= div proposedRuns 10 then proposedCommitIndex + 1 else proposedCommitIndex
        else return proposedCommitIndex
      bcs@BisectCommitState{..} <- readArray (_bisectStateCommits bisectState) commitIndex
      resetGitBranch _bisectCommit
      writeIORef (_bisectCurrentCommit bisectState) $ Just bcs
      let knownBad = all (==0.0) $ take commitIndex cumulativeDistribution
      return $ "bisect index " ++ show commitIndex ++ (if knownBad then " (known-bad)" else "") ++ ": "

getPosteriorDistribution :: BisectState -> IO (UArray Int Double)
getPosteriorDistribution bisectState = do
  _elems <- getElems (_bisectStateCommits bisectState)
  let cumulativeFailures  = drop 1 (scanr (+) 0 $ map _bisectCommitFailures _elems) ++ [0]
      loop1 [] = []
      loop1 ((failuresAccr, _):es) = if failuresAccr > 0 then 0.0 : loop1 es else 1.0 : loop2 1.0 es
      loop2 _ [] = []
      loop2 p ((_,successes):es) = let p' = p * product (replicate successes 0.9) in p' : loop2 p' es
  (lb,ub) <- getBounds (_bisectStateCommits bisectState)
  return $ array (lb,ub) [ (ix, p) | (ix, p) <- zip [lb..ub] $ loop1 $ zip cumulativeFailures $ map _bisectCommitSuccesses _elems ]

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

makeCommitSelector :: IO (IO String)
makeCommitSelector = do
  maybeBranch <- lookupEnv "BRANCH"
  case maybeBranch of
    Nothing -> return $ return ""
    Just branch -> do
      let rev = "origin/" ++ branch
          description = show rev ++ " = "
      return $ do
        resetGitBranch rev
        return description

logRunUntilFailure :: IO String -> [String] -> IO Int
logRunUntilFailure commitSelector args = withFile "gradle-loop.log" AppendMode $ \hLog -> do
  hSetBuffering hLog LineBuffering
  runUntilFailure commitSelector (logAndPrint hLog) args

runUntilFailure :: IO String -> (String -> IO ()) -> [String] -> IO Int
runUntilFailure commitSelector writeLog args = loop (0::Int) Nothing
  where
  loop iteration maybePreviousGitRevision = do

    commitDescription <- commitSelector
    gitRevision <- getGitRevision
    writeLog $ printf "[%4d] starting on %s%s with args %s" iteration commitDescription (show gitRevision) (show args)

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
        return c

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
