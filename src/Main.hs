{-# LANGUAGE RecordWildCards #-}

module Main where

import           Brick
import           Brick.BChan
import           Control.Concurrent (forkIO)
import qualified Control.Concurrent.Chan as Chan
import           Control.Exception (try)
import           Control.Monad (forever)
import           Data.Monoid ((<>))
import qualified Graphics.Vty as Vty
import           Lens.Micro.Platform
import           System.Exit (exitFailure)
import           System.IO (IOMode(WriteMode), openFile, hClose)

import           Config
import           Options
import           State.Setup
import           Events
import           Draw
import           Types
import           InputHistory

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

  requestChan <- Chan.newChan
  _ <- forkIO $ forever $ do
    req <- Chan.readChan requestChan
    res <- try req
    case res of
      Left e    -> writeBChan eventChan (AsyncErrEvent e)
      Right upd -> writeBChan eventChan (RespEvent upd)

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
  case logFile of
    Nothing -> return ()
    Just h -> hClose h
  writeHistory (finalSt^.csInputHistory)

app :: App ChatState MHEvent Name
app = App
  { appDraw         = draw
  , appChooseCursor = showFirstCursor
  , appHandleEvent  = onEvent
  , appStartEvent   = return
  , appAttrMap      = (^.csTheme)
  }
