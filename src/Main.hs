{-# LANGUAGE RecordWildCards #-}

module Main where

import           Brick
import           Brick.Widgets.Border
import           Brick.Widgets.Edit (editor, renderEditor)
import qualified Control.Concurrent.Chan as Chan
import           Control.Monad (void)
import           Control.Monad.IO.Class (liftIO)
import           Data.Default (def)
import qualified Graphics.Vty as Vty
import           Lens.Micro.Platform

import           Network.Mattermost
-- import           Network.Mattermost.Lenses
import           Network.Mattermost.WebSocket
import           Network.Mattermost.WebSocket.Types

import           Config
import           State

data Event
  = VtyEvent Vty.Event
  | WSEvent WebsocketEvent

main :: IO ()
main = do
  config <- getConfig
  st <- setupState config

  eventChan <- Chan.newChan
  let shunt e = Chan.writeChan eventChan (WSEvent e)

  mmWithWebSocket (st^.csConn) (st^.csTok) shunt $ \c -> do
    void $ customMain (Vty.mkVty def) eventChan app st

app :: App ChatState Event Int
app = App
  { appDraw = chatDraw
  , appChooseCursor = \ _ _ -> Nothing
  , appHandleEvent = onEvent
  , appStartEvent = \ s -> return (nextChannel id s)
  , appAttrMap = \ _ -> def
  , appLiftVtyEvent = VtyEvent
  }

chatDraw :: ChatState -> [Widget Int]
chatDraw st
  | Just cId <- st^.csFocus =
      let chnName = getChannelName cId st
          msgs = getMessageListing cId st
          chatText = vBox [ str (u ++ ": " ++ m)
                          | (u, m) <- msgs
                          ]
          userCmd  = renderEditor False (editor 1 (vBox . map str) (Just 1) "> ")
      in [ border (padRight Max (str ("#" ++ chnName))) <=>
           border (viewport 0 Vertical chatText) <=>
           border userCmd ]
  | otherwise = [ str "whoo" ]

onEvent :: ChatState -> Event -> EventM Int (Next ChatState)
onEvent st (VtyEvent (Vty.EvKey Vty.KEsc [])) = halt st
onEvent st (VtyEvent (Vty.EvKey Vty.KRight [])) =
  continue (nextChannel (+1) st)
onEvent st (VtyEvent (Vty.EvKey Vty.KLeft [])) =
  continue (nextChannel (\ x -> x - 1) st)
onEvent st (VtyEvent e) = do
  continue st
onEvent st (WSEvent we) = do
  case weAction we of
    WMPosted -> case wepPost (weProps we) of
      Just p  -> continue $ addMessage p st
      Nothing -> continue st
    WMPostEdited -> case wepPost (weProps we) of
      Just p  -> continue $ editMessage p st
      Nothing -> continue st
    WMPostDeleted -> case wepPost (weProps we) of
      Just p  -> continue $ editMessage p { postMessage = "[deleted]" } st
      Nothing -> continue st
    _ -> continue st

{-
handleInput :: StateRef -> MMWebSocket -> IO ()
handleInput st ws = do
  ln <- getLine
  case words ln of
    ["show", chan] -> do
      ChatState { _chnMap = cs } <- readIORef st
      case [ c | c <- HM.elems cs, channelName c == chan ] of
        c:_ -> do
          ms <- getMessageListing (channelId c) st
          forM_ ms $ \ (u, m) -> do
            putStrLn ("@" ++ u ++ ":  " ++ m)
          handleInput st ws
        _ -> do
          putStrLn ("cannot find " ++ chan)
          handleInput st ws
    ["quit"] -> do
      mmCloseWebSocket ws
    cmd -> do
      putStrLn ("I don't know how to " ++ unwords cmd)
      handleInput st ws
-}
