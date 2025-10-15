module Matterhorn.Events.ChannelTopicWindow
  ( onEventChannelTopicWindow
  , channelTopicWindowMouseHandler
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick ( BrickEvent(VtyEvent, MouseDown) )
import           Brick.Focus
import           Brick.Widgets.Edit ( handleEditorEvent, getEditContents )
import qualified Data.Text as T
import           Lens.Micro.Platform ( (%=) )
import qualified Graphics.Vty as Vty

import           Network.Mattermost.Types ( TeamId, ChannelId )

import           Matterhorn.Types
import           Matterhorn.State.Channels ( setChannelTopic )


onEventChannelTopicWindow :: TeamId -> ChannelId -> Vty.Event -> MH ()
onEventChannelTopicWindow tId _ (Vty.EvKey (Vty.KChar '\t') []) =
    csTeam(tId).tsChannelTopicDialog.channelTopicDialogFocus %= focusNext
onEventChannelTopicWindow tId _ (Vty.EvKey Vty.KBackTab []) =
    csTeam(tId).tsChannelTopicDialog.channelTopicDialogFocus %= focusPrev
onEventChannelTopicWindow tId cId e@(Vty.EvKey Vty.KEnter []) = do
    f <- use (csTeam(tId).tsChannelTopicDialog.channelTopicDialogFocus)
    case focusGetCurrent f of
        Just (ChannelTopicSaveButton {}) -> do
            doSaveTopic tId cId
        Just (ChannelTopicEditor {}) ->
            mhZoom (csTeam(tId).tsChannelTopicDialog.channelTopicDialogEditor)
                                handleEditorEvent (VtyEvent e)
        Just (ChannelTopicCancelButton {}) ->
            doCancelTopicEdit tId
        _ ->
            popMode tId
onEventChannelTopicWindow tId _ (Vty.EvKey Vty.KEsc []) = do
    popMode tId
onEventChannelTopicWindow tId _ e = do
    f <- use (csTeam(tId).tsChannelTopicDialog.channelTopicDialogFocus)
    case focusGetCurrent f of
        Just (ChannelTopicEditor {}) ->
            mhZoom (csTeam(tId).tsChannelTopicDialog.channelTopicDialogEditor)
                                handleEditorEvent (VtyEvent e)
        _ ->
            return ()

channelTopicWindowMouseHandler :: TeamId -> ChannelId -> BrickEvent Name MHEvent -> MH ()
channelTopicWindowMouseHandler tId cId (MouseDown (ChannelTopicSaveButton {}) _ _ _) = doSaveTopic tId cId
channelTopicWindowMouseHandler tId _ (MouseDown (ChannelTopicCancelButton {}) _ _ _) = doCancelTopicEdit tId
channelTopicWindowMouseHandler _ _ _ = return ()

doSaveTopic :: TeamId -> ChannelId -> MH ()
doSaveTopic tId cId = do
    ed <- use (csTeam(tId).tsChannelTopicDialog.channelTopicDialogEditor)
    let topic = T.unlines $ getEditContents ed
    setChannelTopic cId topic
    popMode tId

doCancelTopicEdit :: TeamId -> MH ()
doCancelTopicEdit tId = popMode tId
