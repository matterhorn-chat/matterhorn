{-# LANGUAGE RecordWildCards #-}

module Main where

import           Prelude ()
import           Prelude.Compat

import           Brick
import           Brick.BChan
import qualified Control.Concurrent.STM as STM
import           Data.Monoid ((<>))
import qualified Graphics.Vty as Vty
import           Lens.Micro.Platform
import           System.Exit (exitFailure)
import           System.IO (IOMode(WriteMode), openFile, hClose)
import           Text.Aspell (stopAspell)

import           Config
import           Options
import           State.Setup
import           Events
import           Draw
import           Types
import           InputHistory
import           AsyncThread

main :: IO ()
main = do
  opts <- grabOptions
  configResult <- findConfig (optConfLocation opts)
  config <- case configResult of
      Left err -> do
          putStrLn $ "Error loading config: " <> err
          exitFailure
      Right c -> return c

  eventChan <- newBChan 25
  writeBChan eventChan RefreshWebsocketEvent

  requestChan <- STM.atomically STM.newTChan

  startAsyncWorkerThread config requestChan eventChan

  logFile <- case optLogLocation opts of
    Just path -> Just `fmap` openFile path WriteMode
    Nothing   -> return Nothing

  st <- setupState logFile config requestChan eventChan

  let mkVty = do
        vty <- Vty.mkVty Vty.defaultConfig
        let output = Vty.outputIface vty
        Vty.setMode output Vty.BracketedPaste True
        return vty

  finalSt <- customMain mkVty (Just eventChan) app st

  case finalSt^.csEditState.cedSpellChecker of
      Nothing -> return ()
      Just (s, _) -> stopAspell s

  case logFile of
    Nothing -> return ()
    Just h -> hClose h
  writeHistory (finalSt^.csEditState.cedInputHistory)

app :: App ChatState MHEvent Name
app = App
  { appDraw         = draw
  , appChooseCursor = showFirstCursor
  , appHandleEvent  = onEvent
  , appStartEvent   = return
  , appAttrMap      = (^.csResources.crTheme)
  }
