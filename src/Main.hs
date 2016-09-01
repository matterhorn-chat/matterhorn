{-# LANGUAGE RecordWildCards #-}

module Main where

import           Brick
import           Control.Concurrent (forkIO)
import qualified Control.Concurrent.Chan as Chan
import           Control.Monad (forever)
import           Data.Default (def)
import           Data.Monoid ((<>))
import qualified Graphics.Vty as Vty
import           Lens.Micro.Platform
import           System.Exit (exitFailure)

import           Network.Mattermost.WebSocket

import           Config
import           State
import           Events
import           Draw
import           Types
import           InputHistory

main :: IO ()
main = do
  configResult <- findConfig
  config <- case configResult of
      Left err -> do
          putStrLn $ "Error loading config: " <> err
          exitFailure
      Right c -> return c

  eventChan <- Chan.newChan
  let shunt e = Chan.writeChan eventChan (WSEvent e)

  requestChan <- Chan.newChan
  _ <- forkIO $ forever $ do
    req <- Chan.readChan requestChan
    upd <- req
    Chan.writeChan eventChan (RespEvent upd)

  st <- setupState config requestChan

  mmWithWebSocket (st^.csConn) (st^.csTok) shunt $ \_ -> do
    finalSt <- customMain (Vty.mkVty def) eventChan app st
    writeHistory (finalSt^.csInputHistory)

app :: App ChatState Event Name
app = App
  { appDraw         = chatDraw
  , appChooseCursor = \ _ (l:_) -> Just l
  , appHandleEvent  = onEvent
  , appStartEvent   = \ s -> return s
  , appAttrMap      = (^.csTheme)
  , appLiftVtyEvent = VtyEvent
  }
