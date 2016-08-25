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
import           Themes

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

app :: App ChatState Event Name
app = App
  { appDraw         = chatDraw
  , appChooseCursor = \ _ (l:_) -> Just l
  , appHandleEvent  = onEvent
  , appStartEvent   = \ s -> return s
  , appAttrMap      = const colorTheme
  , appLiftVtyEvent = VtyEvent
  }

wrappedText :: String -> Widget Name
wrappedText msg = Widget Fixed Fixed $ do
  ctx <- getContext
  let w = ctx ^. availWidthL
  render (str (breakString (BreakFormat w 8 '-' Nothing) msg))

renderTime :: TimeZone -> UTCTime -> Widget Name
renderTime tz t =
    -- %R gives HH:MM in 24 hour time
    let timeStr = formatTime defaultTimeLocale "%R" (utcToLocalTime tz t)
    in str "[" <+> withDefAttr timeAttr (str timeStr) <+> str "]"

renderChatMessage :: TimeZone -> (UTCTime, String, String) -> Widget Name
renderChatMessage tz (t, u, m) =
    renderTime tz t <+> str " " <+> wrappedText (u ++ ": " ++ m)

mkChannelName :: String -> String
mkChannelName = ('#':)

mkDMChannelName :: String -> String
mkDMChannelName = ('@':)

renderChannelList :: ChatState -> Widget Name
renderChannelList st = hLimit channelListWidth $
                       vBox $ header "Channels" : channelNames <>
                              (header "Users" : dmChannelNames)
    where
    channelListWidth = 20
    cId = currentChannelId st
    currentChannelName = getChannelName cId st
    header label = hBorderWithLabel $
                   withDefAttr channelListHeaderAttr $
                   str label
    channelNames = [ attr $ str (indicator ++ mkChannelName n)
                   | n <- (st ^. csNames . cnChans)
                   , let indicator = if current then "+" else " "
                         attr = if current
                                then withDefAttr currentChannelNameAttr
                                else id
                         current = n == currentChannelName
                   ]
    dmChannelNames = [ str (" " ++ mkDMChannelName n)
                     | n <- (st ^. csNames . cnUsers)
                     ]

renderUserCommandBox :: ChatState -> Widget Name
renderUserCommandBox st = prompt <+> inputBox
    where
    prompt = str "> "
    inputBox = renderEditor True (st^.cmdLine)

renderCurrentChannelDisplay :: ChatState -> Widget Name
renderCurrentChannelDisplay st = header <=> hBorder <=> messages
    where
    header = padRight Max $
             withDefAttr channelHeaderAttr $
             case null purposeStr of
                 True -> str $ mkChannelName chnName
                 False -> wrappedText $ mkChannelName chnName <> " - " <> purposeStr
    messages = viewport ChannelMessages Vertical chatText <+> str " "
    chatText = vBox $ renderChatMessage (st ^. timeZone) <$> channelMessages
    channelMessages = getMessageListing cId st
    cId = currentChannelId st
    Just chan = getChannel cId st
    chnName = chan^.channelNameL
    purposeStr = chan^.channelPurposeL

chatDraw :: ChatState -> [Widget Name]
chatDraw st =
    [ (renderChannelList st <+> vBorder <+> renderCurrentChannelDisplay st)
      <=> hBorder
      <=> renderUserCommandBox st
    ]

onEvent :: ChatState -> Event -> EventM Name (Next ChatState)
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
      chanId = currentChannelId st
      teamId = st^.csMyTeam.teamIdL
  pendingPost <- mkPendingPost msg myId chanId
  _ <- mmPost (st^.csConn) (st^.csTok) teamId pendingPost
  return ()
