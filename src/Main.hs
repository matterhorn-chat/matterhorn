module Main where

import           Prelude ()
import           Prelude.Compat

import           Data.Monoid ((<>))
import           System.Exit (exitFailure)

import           Config
import           Options
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
    closeMatterhorn finalSt
