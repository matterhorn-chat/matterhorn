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

import           Matterhorn.Types
import           Matterhorn.State.Channels ( setChannelTopic )


onEventChannelTopicWindow :: Vty.Event -> MH ()
onEventChannelTopicWindow (Vty.EvKey (Vty.KChar '\t') []) =
    csCurrentTeam.tsChannelTopicDialog.channelTopicDialogFocus %= focusNext
onEventChannelTopicWindow (Vty.EvKey Vty.KBackTab []) =
    csCurrentTeam.tsChannelTopicDialog.channelTopicDialogFocus %= focusPrev
onEventChannelTopicWindow e@(Vty.EvKey Vty.KEnter []) = do
    tId <- use csCurrentTeamId
    f <- use (csTeam(tId).tsChannelTopicDialog.channelTopicDialogFocus)
    case focusGetCurrent f of
        Just (ChannelTopicSaveButton {}) -> do
            ed <- use (csTeam(tId).tsChannelTopicDialog.channelTopicDialogEditor)
            let topic = T.unlines $ getEditContents ed
            setChannelTopic topic
            setMode tId Main
        Just (ChannelTopicEditor {}) ->
            mhHandleEventLensed (csCurrentTeam.tsChannelTopicDialog.channelTopicDialogEditor)
                                handleEditorEvent e
        Just (ChannelTopicCancelButton {}) ->
            setMode tId Main
        _ ->
            setMode tId Main
onEventChannelTopicWindow (Vty.EvKey Vty.KEsc []) = do
    tId <- use csCurrentTeamId
    setMode tId Main
onEventChannelTopicWindow e = do
    f <- use (csCurrentTeam.tsChannelTopicDialog.channelTopicDialogFocus)
    case focusGetCurrent f of
        Just (ChannelTopicEditor {}) ->
            mhHandleEventLensed (csCurrentTeam.tsChannelTopicDialog.channelTopicDialogEditor)
                                handleEditorEvent e
        _ ->
            return ()
