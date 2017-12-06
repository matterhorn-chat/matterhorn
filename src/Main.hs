module Main where

import           Prelude ()
import           Prelude.Compat

import           Data.Monoid ((<>))
import           Lens.Micro.Platform
import           System.Exit (exitFailure)

import           Config
import           Options
import           Types
import           InputHistory
import           RunState
import           App

main :: IO ()
main = do
    opts <- grabOptions
    configResult <- findConfig (optConfLocation opts)
    config <- case configResult of
        Left err -> do
            putStrLn $ "Error loading config: " <> err
            exitFailure
        Right c -> return c

    finalSt <- runMatterhorn opts config

    writeHistory (finalSt^.csEditState.cedInputHistory)

    -- Try to write the run state to a file. If it fails, just print the error
    -- and do not exit with a failure status because the run state file is optional.
    done <- writeRunState finalSt
    case done of
      Left err -> putStrLn $ "Error in writing last run state: " <> err
      Right _  -> return ()
