module Events where

import           Brick
import           Brick.Widgets.Edit ( getEditContents
                                    , handleEditorEvent
                                    , applyEdit
                                    )
import           Control.Monad.IO.Class (liftIO)
import           Data.Text.Zipper (clearZipper)
import qualified Graphics.Vty as Vty
import           Lens.Micro.Platform

import           Network.Mattermost
import           Network.Mattermost.Lenses
import           Network.Mattermost.WebSocket.Types

import           Command
import           State
import           Types

onEvent :: ChatState -> Event -> EventM Name (Next ChatState)
onEvent st (VtyEvent (Vty.EvResize _ _)) = do
  -- On resize we need to update the current channel message area so
  -- that the most recent message is at the bottom. We have to do this
  -- on a resize because brick only guarantees that the message is
  -- visible, not that it is at the bottom, so after a resize we can end
  -- up with lots of whitespace at the bottom of the message area. This
  -- whitespace is created when the window gets bigger. We only need to
  -- worry about the current channel's viewport because that's the one
  -- that is about to be redrawn.
  continue =<< updateChannelScrollState st
onEvent st (VtyEvent (Vty.EvKey Vty.KEsc [])) =
  halt st
onEvent st (VtyEvent (Vty.EvKey (Vty.KChar 'n') [Vty.MCtrl])) =
  continue =<< updateChannelScrollState =<< nextChannel st
onEvent st (VtyEvent (Vty.EvKey (Vty.KChar 'p') [Vty.MCtrl])) =
  continue =<< updateChannelScrollState =<< prevChannel st
onEvent st (VtyEvent (Vty.EvKey Vty.KEnter [])) =
  handleInputSubmission st
onEvent st (VtyEvent e) =
  continue =<< handleEventLensed st cmdLine handleEditorEvent e
onEvent st (WSEvent we) =
  handleWSEvent st we

handleInputSubmission :: ChatState -> EventM Name (Next ChatState)
handleInputSubmission st = do
  let (line:_) = getEditContents (st^.cmdLine)
  let st' = st & cmdLine %~ applyEdit clearZipper
  case line of
    ('/':cmd) -> dispatchCommand cmd st'
    _         -> do
      liftIO (sendMessage st' line)
      continue st'

handleWSEvent :: ChatState -> WebsocketEvent -> EventM Name (Next ChatState)
handleWSEvent st we =
  case weEvent we of
    WMPosted -> case wepPost (weData we) of
      Just p  -> addMessage p st >>= continue
      Nothing -> continue st
    WMPostEdited -> case wepPost (weData we) of
      Just p  -> editMessage p st >>= continue
      Nothing -> continue st
    WMPostDeleted -> case wepPost (weData we) of
      Just p  -> editMessage p { postMessage = "[deleted]" } st >>= continue
      Nothing -> continue st
    _ -> continue st

sendMessage :: ChatState -> String -> IO ()
sendMessage st msg = do
  let myId   = st^.csMe.userIdL
      chanId = currentChannelId st
      theTeamId = st^.csMyTeam.teamIdL
  pendingPost <- mkPendingPost msg myId chanId
  _ <- mmPost (st^.csConn) (st^.csTok) theTeamId pendingPost
  return ()
