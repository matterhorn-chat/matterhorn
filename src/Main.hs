module Main where

import Prelude ()
import Prelude.MH
import qualified Data.Text.IO as T

import System.Exit ( exitFailure, exitSuccess )

import Config
import Options
import App
import Events.Keybindings ( ensureKeybindingConsistency )
import KeyMap ( keybindingModeMap )
import Draw.ShowHelp ( keybindingMarkdownTable, keybindingTextTable
                     , commandMarkdownTable, commandTextTable )


main :: IO ()
main = do
    opts <- grabOptions

    configResult <- if optIgnoreConfig opts
                    then return $ Right defaultConfig
                    else findConfig (optConfLocation opts)

    config <- case configResult of
        Left err -> do
            putStrLn $ "Error loading config: " <> err
            exitFailure
        Right c -> return c

    let keyConfig = configUserKeys config

    printedCommands <- case optPrintCommands opts of
        Nothing -> return False
        Just ty -> do
            case ty of
                Markdown -> T.putStrLn commandMarkdownTable
                Plain -> T.putStrLn commandTextTable
            return True

    printedKeybindings <- case optPrintKeybindings opts of
        Nothing -> return False
        Just ty -> do
            case ty of
                Markdown -> T.putStrLn $ keybindingMarkdownTable keyConfig
                Plain -> T.putStrLn $ keybindingTextTable keyConfig
            return True

    when (printedKeybindings || printedCommands) exitSuccess

    case ensureKeybindingConsistency keyConfig keybindingModeMap of
        Right () -> return ()
        Left err -> do
            putStrLn $ "Configuration error: " <> err
            exitFailure

    finalSt <- runMatterhorn opts config
    closeMatterhorn finalSt
