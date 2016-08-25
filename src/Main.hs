{-# LANGUAGE RecordWildCards #-}

module Main where

import           Brick
import qualified Control.Concurrent.Chan as Chan
import           Control.Monad (void)
import           Data.Default (def)
import qualified Graphics.Vty as Vty
import           Lens.Micro.Platform

import           Network.Mattermost.WebSocket

import           Config
import           State
import           Themes
import           Events
import           Draw
import           Types

main :: IO ()
main = do
  config <- findConfig
  st <- setupState config

  eventChan <- Chan.newChan
  let shunt e = Chan.writeChan eventChan (WSEvent e)

  mmWithWebSocket (st^.csConn) (st^.csTok) shunt $ \_ -> do
    void $ customMain (Vty.mkVty def) eventChan app st

app :: App ChatState Event Name
app = App
  { appDraw         = chatDraw
  , appChooseCursor = \ _ (l:_) -> Just l
  , appHandleEvent  = onEvent
  , appStartEvent   = \ s -> return s
  , appAttrMap      = const colorTheme
  , appLiftVtyEvent = VtyEvent
  }
