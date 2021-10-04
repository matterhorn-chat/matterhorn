module Matterhorn.Events.ChannelTopicWindow
  ( onEventChannelTopicWindow
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick.Focus
import           Brick.Widgets.Edit ( handleEditorEvent, getEditContents )
import qualified Data.Text as T
import           Lens.Micro.Platform ( (%=) )
import qualified Graphics.Vty as Vty

import           Network.Mattermost.Types ( TeamId )

import           Matterhorn.Types
import           Matterhorn.State.Channels ( setChannelTopic )


onEventChannelTopicWindow :: TeamId -> Vty.Event -> MH ()
onEventChannelTopicWindow tId (Vty.EvKey (Vty.KChar '\t') []) =
    csTeam(tId).tsChannelTopicDialog.channelTopicDialogFocus %= focusNext
onEventChannelTopicWindow tId (Vty.EvKey Vty.KBackTab []) =
    csTeam(tId).tsChannelTopicDialog.channelTopicDialogFocus %= focusPrev
onEventChannelTopicWindow tId e@(Vty.EvKey Vty.KEnter []) = do
    f <- use (csTeam(tId).tsChannelTopicDialog.channelTopicDialogFocus)
    case focusGetCurrent f of
        Just (ChannelTopicSaveButton {}) -> do
            ed <- use (csTeam(tId).tsChannelTopicDialog.channelTopicDialogEditor)
            let topic = T.unlines $ getEditContents ed
            setChannelTopic topic
            setMode tId Main
        Just (ChannelTopicEditor {}) ->
            mhHandleEventLensed (csTeam(tId).tsChannelTopicDialog.channelTopicDialogEditor)
                                handleEditorEvent e
        Just (ChannelTopicCancelButton {}) ->
            setMode tId Main
        _ ->
            setMode tId Main
onEventChannelTopicWindow tId (Vty.EvKey Vty.KEsc []) = do
    setMode tId Main
onEventChannelTopicWindow tId e = do
    f <- use (csTeam(tId).tsChannelTopicDialog.channelTopicDialogFocus)
    case focusGetCurrent f of
        Just (ChannelTopicEditor {}) ->
            mhHandleEventLensed (csTeam(tId).tsChannelTopicDialog.channelTopicDialogEditor)
                                handleEditorEvent e
        _ ->
            return ()
