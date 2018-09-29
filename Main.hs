{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment
import Data.Conduit
import Data.Conduit.Process
import Data.Conduit.Combinators as DCC
import Control.Monad.Trans.Resource
import System.Exit
import Data.Time
import System.IO
import Text.Printf
import qualified Data.ByteString as B
import Data.Time.ISO8601

main :: IO ()
main = do
  args <- getArgs
  withFile "gradle-loop.log" AppendMode $ \hLog -> do
    logAndPrint hLog $ "starting with " ++ show args
    runUntilFailure (logAndPrint hLog) args

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

runUntilFailure :: (String -> IO ()) -> [String] -> IO ()
runUntilFailure writeLog args = loop
  where
  loop = do
    gitRevision <- getGitRevision
    writeLog $ printf "starting on revision %s with args %s" (show gitRevision) (show args)

    startTime <- getCurrentTime
    (exitCode, (), ()) <- runResourceT $
      sourceProcessWithStreams
        (proc "./gradlew" args)
          { new_session   = False
          , delegate_ctlc = True
          }
        (return ()) -- stdin
        (sinkFile "testoutput-stdout.log")
        (sinkFile "testoutput-stderr.log")

    endTime <- getCurrentTime
    writeLog $ printf "finished with %s in %s" (show exitCode) (show $ diffUTCTime endTime startTime)
    case exitCode of
      ExitSuccess -> loop
      _           -> do
        runResourceT $ runConduit
          $  sourceFile "testoutput-stderr.log"
          .| DCC.linesUnboundedAscii
          .| DCC.unlinesAscii
          .| DCC.stdout
