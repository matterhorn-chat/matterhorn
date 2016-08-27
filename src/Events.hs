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
onEvent st (VtyEvent (Vty.EvKey (Vty.KChar 'n') [Vty.MCtrl])) =
  continue =<< nextChannel st
onEvent st (VtyEvent (Vty.EvKey (Vty.KChar 'p') [Vty.MCtrl])) =
  continue =<< prevChannel st
onEvent st (VtyEvent (Vty.EvKey Vty.KEnter [])) =
  handleInputSubmission st
onEvent st (VtyEvent e) =
  continue =<< handleEventLensed st cmdLine handleEditorEvent e
onEvent st (WSEvent we) =
  handleWSEvent st we
onEvent st (RespEvent f) =
  continue (f st)

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
  doAsync st $ do
    pendingPost <- mkPendingPost msg myId chanId
    doAsync st $ do
      _ <- mmPost (st^.csConn) (st^.csTok) theTeamId pendingPost
      return ()
