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
onEvent st (VtyEvent (Vty.EvKey Vty.KEsc [])) =
  halt st
onEvent st (VtyEvent (Vty.EvKey Vty.KRight [Vty.MCtrl])) =
  continue (nextChannel st)
onEvent st (VtyEvent (Vty.EvKey Vty.KLeft [Vty.MCtrl])) =
  continue (prevChannel st)
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
      theTeamId = st^.csMyTeam.teamIdL
  pendingPost <- mkPendingPost msg myId chanId
  _ <- mmPost (st^.csConn) (st^.csTok) theTeamId pendingPost
  return ()
