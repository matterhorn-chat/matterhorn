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
