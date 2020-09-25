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
    csChannelTopicDialog.channelTopicDialogFocus %= focusNext
onEventChannelTopicWindow (Vty.EvKey Vty.KBackTab []) =
    csChannelTopicDialog.channelTopicDialogFocus %= focusPrev
onEventChannelTopicWindow e@(Vty.EvKey Vty.KEnter []) = do
    f <- use (csChannelTopicDialog.channelTopicDialogFocus)
    case focusGetCurrent f of
        Just ChannelTopicSaveButton -> do
            ed <- use (csChannelTopicDialog.channelTopicDialogEditor)
            let topic = T.unlines $ getEditContents ed
            setChannelTopic topic
            setMode Main
        Just ChannelTopicEditor ->
            mhHandleEventLensed (csChannelTopicDialog.channelTopicDialogEditor)
                                handleEditorEvent e
        Just ChannelTopicCancelButton ->
            setMode Main
        _ ->
            setMode Main
onEventChannelTopicWindow (Vty.EvKey Vty.KEsc []) = do
    setMode Main
onEventChannelTopicWindow e = do
    f <- use (csChannelTopicDialog.channelTopicDialogFocus)
    case focusGetCurrent f of
        Just ChannelTopicEditor ->
            mhHandleEventLensed (csChannelTopicDialog.channelTopicDialogEditor)
                                handleEditorEvent e
        _ ->
            return ()
