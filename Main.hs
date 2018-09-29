module Main where

import System.Environment
import Data.Conduit
import Data.Conduit.Process
import Data.Conduit.Combinators
import Control.Monad.Trans.Resource
import System.Exit

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ "starting with " ++ show args
  loop args

loop :: [String] -> IO ()
loop args = do
  (exitCode, (), ()) <- runResourceT $
    sourceProcessWithStreams
      (proc "./gradlew" args)
        { new_session   = True
        , delegate_ctlc = True
        }
      (return ()) -- stdin
      (sinkFile "testoutput-stdout.log")
      (sinkFile "testoutput-stderr.log")

  putStrLn $ "finished with " ++ show exitCode
  case exitCode of
    ExitSuccess -> loop args
    _           -> return ()
