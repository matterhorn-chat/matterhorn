module Main where

import Prelude ()
import qualified Data.Text.IO as T

import System.Exit ( exitFailure, exitSuccess )

import Matterhorn.Prelude
import Matterhorn.Config
import Matterhorn.Options
import Matterhorn.App
import Matterhorn.KeybindingConsistency
import Brick.Keybindings.Pretty ( keybindingTextTable, keybindingMarkdownTable )
import Matterhorn.Draw.ShowHelp ( commandMarkdownTable, commandTextTable
                                , keybindSections )


main :: IO ()
main = do
    opts <- grabOptions

    configResult <- findConfig $ if optIgnoreConfig opts
                                 then Nothing
                                 else optConfLocation opts

    config <- case configResult of
        Left err -> do
            putStrLn $ "Error loading config: " <> err
            exitFailure
        Right (_, c) -> return c

    let keyConfig = configUserKeys config
        format = optPrintFormat opts

    printedCommands <- case optPrintCommands opts of
        False -> return False
        True -> do
            case format of
                Markdown -> T.putStrLn commandMarkdownTable
                Plain -> T.putStrLn commandTextTable
            return True

    printedKeybindings <- case optPrintKeybindings opts of
        False -> return False
        True -> do
            case format of
                Markdown -> T.putStrLn $ keybindingMarkdownTable keyConfig keybindSections
                Plain -> T.putStrLn $ keybindingTextTable keyConfig keybindSections
            return True

    when (printedKeybindings || printedCommands) exitSuccess

    case ensureKeybindingConsistency keyConfig keybindSections of
        Right () -> return ()
        Left err -> do
            putStrLn $ "Configuration error: " <> err
            exitFailure

    finalSt <- runMatterhorn opts config
    closeMatterhorn finalSt
