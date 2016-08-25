{-# LANGUAGE RecordWildCards #-}

module Main where

import           Brick
import           Brick.Widgets.Border
import           Brick.Widgets.Edit ( renderEditor
                                    , getEditContents
                                    , handleEditorEvent
                                    , applyEdit
                                    )
import qualified Control.Concurrent.Chan as Chan
import           Control.Monad (void)
import           Control.Monad.IO.Class (liftIO)
import           Data.Default (def)
import           Data.Text.Zipper (clearZipper)
import           Data.Time.Clock (UTCTime)
import           Data.Time.Format ( formatTime
                                  , defaultTimeLocale )
import           Data.Time.LocalTime ( TimeZone, utcToLocalTime )
import           Data.Monoid ((<>))
import qualified Graphics.Vty as Vty
import           Lens.Micro.Platform
import           Text.LineBreak (breakString, BreakFormat(..))

import           Network.Mattermost
import           Network.Mattermost.Lenses
import           Network.Mattermost.WebSocket
import           Network.Mattermost.WebSocket.Types

import           Command
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

  mmWithWebSocket (st^.csConn) (st^.csTok) shunt $ \_ -> do
    void $ customMain (Vty.mkVty def) eventChan app st

app :: App ChatState Event Int
app = App
  { appDraw         = chatDraw
  , appChooseCursor = \ _ (l:_) -> Just l
  , appHandleEvent  = onEvent
  , appStartEvent   = \ s -> return s
  , appAttrMap      = \ _ -> def
  , appLiftVtyEvent = VtyEvent
  }

wrappedText :: String -> Widget Int
wrappedText msg = Widget Fixed Fixed $ do
  ctx <- getContext
  let w = ctx ^. availWidthL
  render (str (breakString (BreakFormat w 8 '-' Nothing) msg))

renderTime :: TimeZone -> UTCTime -> String
renderTime tz t =
    -- %R gives HH:MM in 24 hour time
    let timeStr = formatTime defaultTimeLocale "%R" (utcToLocalTime tz t)
    in "[" ++ timeStr ++ "]"

renderChatMessage :: TimeZone -> (UTCTime, String, String) -> Widget Int
renderChatMessage tz (t, u, m) =
    str (renderTime tz t ++ " ") <+> wrappedText (u ++ ": " ++ m)

renderChannelList :: ChatState -> Widget Int
renderChannelList st = vBox $ channelNames <> dmChannelNames
    where
    cId = currChannel st
    currentChannelName = getChannelName cId st
    channelNames = [ str (i ++ "#" ++ n)
                   | n <- (st ^. csNames . cnChans)
                   , let i = if n == currentChannelName then "+" else " "
                   ]
    dmChannelNames = [ str (" @" ++ n)
                     | n <- (st ^. csNames . cnUsers)
                     ]

chatDraw :: ChatState -> [Widget Int]
chatDraw st =
  let cId      = currChannel st
      chnName  = getChannelName    cId st
      msgs     = getMessageListing cId st
      chatText = vBox $ renderChatMessage (st ^. timeZone) <$> msgs
      prompt = str "> "
      userCmd  = (prompt <+> renderEditor True (st^.cmdLine))
  in [ (renderChannelList st <+> vBorder <+>
         (padRight Max (str ("#" ++ chnName))
           <=> hBorder
           <=> viewport 0 Vertical chatText <+> str " "))
       <=> hBorder
       <=> userCmd
     ]

onEvent :: ChatState -> Event -> EventM Int (Next ChatState)
onEvent st (VtyEvent (Vty.EvKey Vty.KEsc [])) = halt st
onEvent st (VtyEvent (Vty.EvKey Vty.KRight [])) =
  continue (nextChannel st)
onEvent st (VtyEvent (Vty.EvKey Vty.KLeft [])) =
  continue (prevChannel st)
onEvent st (VtyEvent (Vty.EvKey Vty.KEnter [])) = do
  let (line:_) = getEditContents (st^.cmdLine)
  let st' = st & cmdLine %~ applyEdit clearZipper
  case line of
    ('/':cmd) -> dispatchCommand cmd st'
    _         -> do
      liftIO (sendMessage st' line)
      continue st'
onEvent st (VtyEvent e) = do
  editor <- handleEditorEvent e (st^.cmdLine)
  continue (st & cmdLine .~ editor)
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

sendMessage :: ChatState -> String -> IO ()
sendMessage st msg = do
  let myId   = st^.csMe.userIdL
      chanId = currChannel st
      teamId = st^.csMyTeam.teamIdL
  pendingPost <- mkPendingPost msg myId chanId
  _ <- mmPost (st^.csConn) (st^.csTok) teamId pendingPost
  return ()
