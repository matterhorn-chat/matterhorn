module Main where

import Prelude ()
import Prelude.MH

import System.Exit ( exitFailure )

import Config
import Options
import App
import Events ( ensureKeybindingConsistency )


main :: IO ()
main = do
    opts <- grabOptions
    configResult <- findConfig (optConfLocation opts)
    config <- case configResult of
        Left err -> do
            putStrLn $ "Error loading config: " <> err
            exitFailure
        Right c -> return c

    case ensureKeybindingConsistency (configUserKeys config) of
        Right () -> return ()
        Left err -> do
            putStrLn $ "Configuration error: " <> err
            exitFailure

    finalSt <- runMatterhorn opts config
    closeMatterhorn finalSt
