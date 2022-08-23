{-# LANGUAGE RankNTypes #-}
module Matterhorn.Events.SaveAttachmentWindow
  ( onEventSaveAttachmentWindow
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import qualified Control.Exception as E

import           Brick ( BrickEvent(VtyEvent) )
import           Brick.Focus
import           Brick.Widgets.Edit ( handleEditorEvent, getEditContents )
import qualified Data.Text as T
import           Lens.Micro.Platform ( (%=), Lens' )
import qualified Graphics.Vty as Vty

import           Matterhorn.Types
import           Matterhorn.State.SaveAttachmentWindow
import           Matterhorn.State.Common ( postInfoMessage, fetchFileAtPath
                                         , doAsyncWith, AsyncPriority(Normal)
                                         , postErrorMessage'
                                         )


onEventSaveAttachmentWindow :: Lens' ChatState (MessageInterface Name i) -> Vty.Event -> MH Bool
onEventSaveAttachmentWindow which (Vty.EvKey (Vty.KChar '\t') []) = do
    which.miSaveAttachmentDialog.attachmentPathDialogFocus %= focusNext
    return True
onEventSaveAttachmentWindow which (Vty.EvKey Vty.KBackTab []) = do
    which.miSaveAttachmentDialog.attachmentPathDialogFocus %= focusPrev
    return True
onEventSaveAttachmentWindow which (Vty.EvKey Vty.KEnter []) = do
    f <- use (which.miSaveAttachmentDialog.attachmentPathDialogFocus)
    session <- getSession
    mode <- use (which.miMode)

    let link = case mode of
            SaveAttachment l -> l
            _ -> error $ "BUG: invalid mode " <> show mode <> " in onEventSaveAttachmentWindow"
        fId = case link^.linkTarget of
            LinkFileId i -> i
            _ -> error $ "BUG: invalid link target " <> show (link^.linkTarget) <> " in onEventSaveAttachmentWindow"
        save = do
            ed <- use (which.miSaveAttachmentDialog.attachmentPathEditor)
            let path = T.unpack $ T.strip $ T.concat $ getEditContents ed

            when (not $ null path) $ do
                doAsyncWith Normal $ do
                    result <- E.try $ fetchFileAtPath fId session path
                    return $ Just $ do
                        case result of
                            Left (e::E.SomeException) ->
                                postErrorMessage' $ T.pack $ "Error saving to " <> path <> ": " <> show e
                            Right () ->
                                postInfoMessage $ T.pack $ "Attachment saved to " <> path
                closeSaveAttachmentWindow which

    case focusGetCurrent f of
        Just (AttachmentPathSaveButton {})   -> save
        Just (AttachmentPathEditor {})       -> save
        Just (AttachmentPathCancelButton {}) -> closeSaveAttachmentWindow which
        _                                    -> closeSaveAttachmentWindow which

    return True
onEventSaveAttachmentWindow which (Vty.EvKey Vty.KEsc []) = do
    closeSaveAttachmentWindow which
    return True
onEventSaveAttachmentWindow which e = do
    f <- use (which.miSaveAttachmentDialog.attachmentPathDialogFocus)
    case focusGetCurrent f of
        Just (AttachmentPathEditor {}) -> do
            mhZoom (which.miSaveAttachmentDialog.attachmentPathEditor)
                                handleEditorEvent (VtyEvent e)
            return True
        _ ->
            return False
