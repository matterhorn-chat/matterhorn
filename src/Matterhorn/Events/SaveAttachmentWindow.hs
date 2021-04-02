module Matterhorn.Events.SaveAttachmentWindow
  ( onEventSaveAttachmentWindow
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import qualified Control.Exception as E

import           Brick.Focus
import           Brick.Widgets.Edit ( handleEditorEvent, getEditContents )
import qualified Data.Text as T
import           Lens.Micro.Platform ( (%=) )
import qualified Graphics.Vty as Vty

import           Matterhorn.Types
import           Matterhorn.State.Common ( postInfoMessage, fetchFileAtPath
                                         , doAsyncWith, AsyncPriority(Normal)
                                         , postErrorMessage'
                                         )


onEventSaveAttachmentWindow :: Vty.Event -> MH ()
onEventSaveAttachmentWindow (Vty.EvKey (Vty.KChar '\t') []) =
    csCurrentTeam.tsSaveAttachmentDialog.attachmentPathDialogFocus %= focusNext
onEventSaveAttachmentWindow (Vty.EvKey Vty.KBackTab []) =
    csCurrentTeam.tsSaveAttachmentDialog.attachmentPathDialogFocus %= focusPrev
onEventSaveAttachmentWindow (Vty.EvKey Vty.KEnter []) = do
    f <- use (csCurrentTeam.tsSaveAttachmentDialog.attachmentPathDialogFocus)
    session <- getSession
    mode <- use (csCurrentTeam.tsMode)

    let SaveAttachmentWindow link = mode
        LinkFileId fId = link^.linkTarget
        save = do
            ed <- use (csCurrentTeam.tsSaveAttachmentDialog.attachmentPathEditor)
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
                setMode UrlSelect

    case focusGetCurrent f of
        Just (AttachmentPathSaveButton {})   -> save
        Just (AttachmentPathEditor {})       -> save
        Just (AttachmentPathCancelButton {}) -> setMode UrlSelect
        _                                    -> setMode UrlSelect
onEventSaveAttachmentWindow (Vty.EvKey Vty.KEsc []) = do
    setMode UrlSelect
onEventSaveAttachmentWindow e = do
    f <- use (csCurrentTeam.tsSaveAttachmentDialog.attachmentPathDialogFocus)
    case focusGetCurrent f of
        Just (AttachmentPathEditor {}) ->
            mhHandleEventLensed (csCurrentTeam.tsSaveAttachmentDialog.attachmentPathEditor)
                                handleEditorEvent e
        _ ->
            return ()
