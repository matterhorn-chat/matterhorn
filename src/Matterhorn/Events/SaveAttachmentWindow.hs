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

import           Network.Mattermost.Types ( TeamId )

import           Matterhorn.Types
import           Matterhorn.State.Common ( postInfoMessage, fetchFileAtPath
                                         , doAsyncWith, AsyncPriority(Normal)
                                         , postErrorMessage'
                                         )


onEventSaveAttachmentWindow :: TeamId -> Vty.Event -> MH ()
onEventSaveAttachmentWindow tId (Vty.EvKey (Vty.KChar '\t') []) =
    csTeam(tId).tsSaveAttachmentDialog.attachmentPathDialogFocus %= focusNext
onEventSaveAttachmentWindow tId (Vty.EvKey Vty.KBackTab []) =
    csTeam(tId).tsSaveAttachmentDialog.attachmentPathDialogFocus %= focusPrev
onEventSaveAttachmentWindow tId (Vty.EvKey Vty.KEnter []) = do
    f <- use (csTeam(tId).tsSaveAttachmentDialog.attachmentPathDialogFocus)
    session <- getSession
    mode <- use (csTeam(tId).tsMode)

    let SaveAttachmentWindow link = mode
        LinkFileId fId = link^.linkTarget
        save = do
            ed <- use (csTeam(tId).tsSaveAttachmentDialog.attachmentPathEditor)
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
                setMode tId UrlSelect

    case focusGetCurrent f of
        Just (AttachmentPathSaveButton {})   -> save
        Just (AttachmentPathEditor {})       -> save
        Just (AttachmentPathCancelButton {}) -> setMode tId UrlSelect
        _                                    -> setMode tId UrlSelect
onEventSaveAttachmentWindow tId (Vty.EvKey Vty.KEsc []) = do
    setMode tId UrlSelect
onEventSaveAttachmentWindow tId e = do
    f <- use (csTeam(tId).tsSaveAttachmentDialog.attachmentPathDialogFocus)
    case focusGetCurrent f of
        Just (AttachmentPathEditor {}) ->
            mhHandleEventLensed (csTeam(tId).tsSaveAttachmentDialog.attachmentPathEditor)
                                handleEditorEvent e
        _ ->
            return ()
