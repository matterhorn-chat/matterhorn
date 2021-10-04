{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Matterhorn.Events.ManageAttachments
  ( onEventManageAttachments
  , attachmentListKeybindings
  , attachmentBrowseKeyHandlers
  , attachmentBrowseKeybindings
  , attachmentListKeyHandlers
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import qualified Brick.Widgets.FileBrowser as FB
import qualified Brick.Widgets.List as L
import qualified Data.Text as T
import qualified Data.Vector as Vector
import qualified Graphics.Vty as V
import           Lens.Micro.Platform ( (?=), (%=), to )

import           Network.Mattermost.Types ( TeamId )

import           Matterhorn.Types
import           Matterhorn.Types.KeyEvents
import           Matterhorn.Events.Keybindings
import           Matterhorn.State.Attachments
import           Matterhorn.State.Common


onEventManageAttachments :: TeamId -> V.Event -> MH ()
onEventManageAttachments tId e = do
    mode <- use (csTeam(tId).tsMode)
    case mode of
        ManageAttachments -> void $ onEventAttachmentList tId e
        ManageAttachmentsBrowseFiles -> onEventBrowseFile tId e
        _ -> error "BUG: onEventManageAttachments called in invalid mode"

onEventAttachmentList :: TeamId -> V.Event -> MH Bool
onEventAttachmentList tId =
    handleKeyboardEvent (attachmentListKeybindings tId) $
        mhHandleEventLensed (csTeam(tId).tsEditState.cedAttachmentList) L.handleListEvent

attachmentListKeybindings :: TeamId -> KeyConfig -> KeyHandlerMap
attachmentListKeybindings tId = mkKeybindings (attachmentListKeyHandlers tId)

attachmentListKeyHandlers :: TeamId -> [KeyEventHandler]
attachmentListKeyHandlers tId =
    [ mkKb CancelEvent "Close attachment list" $
          setMode tId Main
    , mkKb SelectUpEvent "Move cursor up" $
          mhHandleEventLensed (csTeam(tId).tsEditState.cedAttachmentList) L.handleListEvent (V.EvKey V.KUp [])
    , mkKb SelectDownEvent "Move cursor down" $
          mhHandleEventLensed (csTeam(tId).tsEditState.cedAttachmentList) L.handleListEvent (V.EvKey V.KDown [])
    , mkKb AttachmentListAddEvent "Add a new attachment to the attachment list" $
          showAttachmentFileBrowser tId
    , mkKb AttachmentOpenEvent "Open the selected attachment using the URL open command" $
          openSelectedAttachment tId
    , mkKb AttachmentListDeleteEvent "Delete the selected attachment from the attachment list" $
          deleteSelectedAttachment tId
    ]

attachmentBrowseKeybindings :: TeamId -> KeyConfig -> KeyHandlerMap
attachmentBrowseKeybindings tId = mkKeybindings (attachmentBrowseKeyHandlers tId)

attachmentBrowseKeyHandlers :: TeamId -> [KeyEventHandler]
attachmentBrowseKeyHandlers tId =
    [ mkKb CancelEvent "Cancel attachment file browse" $
      cancelAttachmentBrowse tId
    , mkKb AttachmentOpenEvent "Open the selected file using the URL open command" $
      openSelectedBrowserEntry tId
    , mkKb FileBrowserBeginSearchEvent "Begin search for name in list" $
      mhHandleEventLensed' (csTeam(tId).tsEditState.unsafeCedFileBrowser)
        FB.actionFileBrowserBeginSearch
    , mkKb FileBrowserSelectEnterEvent "Select file or enter directory" $ do
      mhHandleEventLensed' (csTeam(tId).tsEditState.unsafeCedFileBrowser)
        FB.actionFileBrowserSelectEnter
      withFileBrowser tId (tryAddAttachment . FB.fileBrowserSelection)
    , mkKb FileBrowserSelectCurrentEvent "Select file" $
      mhHandleEventLensed' (csTeam(tId).tsEditState.unsafeCedFileBrowser)
        FB.actionFileBrowserSelectCurrent
    , mkKb FileBrowserListPageUpEvent "Move cursor one page up" $
      mhHandleEventLensed' (csTeam(tId).tsEditState.unsafeCedFileBrowser)
        FB.actionFileBrowserListPageUp
    , mkKb FileBrowserListPageDownEvent "Move cursor one page down" $
      mhHandleEventLensed' (csTeam(tId).tsEditState.unsafeCedFileBrowser)
        FB.actionFileBrowserListPageDown
    , mkKb FileBrowserListHalfPageUpEvent "Move cursor one-half page up" $
      mhHandleEventLensed' (csTeam(tId).tsEditState.unsafeCedFileBrowser)
        FB.actionFileBrowserListHalfPageUp
    , mkKb FileBrowserListHalfPageDownEvent "Move cursor one-half page down" $
      mhHandleEventLensed' (csTeam(tId).tsEditState.unsafeCedFileBrowser)
        FB.actionFileBrowserListHalfPageDown
    , mkKb FileBrowserListTopEvent "Move cursor to top of list" $
      mhHandleEventLensed' (csTeam(tId).tsEditState.unsafeCedFileBrowser)
        FB.actionFileBrowserListTop
    , mkKb FileBrowserListBottomEvent "Move cursor to bottom of list" $
      mhHandleEventLensed' (csTeam(tId).tsEditState.unsafeCedFileBrowser)
        FB.actionFileBrowserListBottom
    , mkKb FileBrowserListNextEvent "Move cursor down" $
      mhHandleEventLensed' (csTeam(tId).tsEditState.unsafeCedFileBrowser)
        FB.actionFileBrowserListNext
    , mkKb FileBrowserListPrevEvent "Move cursor up" $
      mhHandleEventLensed' (csTeam(tId).tsEditState.unsafeCedFileBrowser)
        FB.actionFileBrowserListPrev
    ]

withFileBrowser :: TeamId -> ((FB.FileBrowser Name) -> MH ()) -> MH ()
withFileBrowser tId f = do
    use (csTeam(tId).tsEditState.cedFileBrowser) >>= \case
        Nothing -> do
            -- The widget has not been created yet.  This should
            -- normally not occur, because the ManageAttachments
            -- events should not fire when there is no FileBrowser
            -- Widget active to cause Brick to generate these events.
            -- This could therefore be implemented as an `error "BUG:
            -- ..."` handler, but the more benign approach is to
            -- simply create an available FileBrowser at this stage.
            new_b <- liftIO $ FB.newFileBrowser FB.selectNonDirectories (AttachmentFileBrowser tId) Nothing
            csTeam(tId).tsEditState.cedFileBrowser ?= new_b
            f new_b
        Just b -> f b

openSelectedAttachment :: TeamId -> MH ()
openSelectedAttachment tId = do
    cur <- use (csTeam(tId).tsEditState.cedAttachmentList.to L.listSelectedElement)
    case cur of
        Nothing -> return ()
        Just (_, entry) -> void $ openFilePath (FB.fileInfoFilePath $
                                                attachmentDataFileInfo entry)

openSelectedBrowserEntry :: TeamId -> MH ()
openSelectedBrowserEntry tId = withFileBrowser tId $ \b ->
    case FB.fileBrowserCursor b of
        Nothing -> return ()
        Just entry -> void $ openFilePath (FB.fileInfoFilePath entry)

onEventBrowseFile :: TeamId -> V.Event -> MH ()
onEventBrowseFile tId e = do
    withFileBrowser tId $ \b -> do
        case FB.fileBrowserIsSearching b of
            False ->
                void $ handleKeyboardEvent (attachmentBrowseKeybindings tId) (handleFileBrowserEvent tId) e
            True ->
                handleFileBrowserEvent tId e

    -- n.b. the FileBrowser may have been updated above, so re-acquire it
    withFileBrowser tId $ \b -> do
        case FB.fileBrowserException b of
            Nothing -> return ()
            Just ex -> do
                mhLog LogError $ T.pack $ "FileBrowser exception: " <> show ex

cancelAttachmentBrowse :: TeamId -> MH ()
cancelAttachmentBrowse tId = do
    es <- use (csTeam(tId).tsEditState.cedAttachmentList.L.listElementsL)
    case length es of
        0 -> setMode tId Main
        _ -> setMode tId ManageAttachments

handleFileBrowserEvent :: TeamId -> V.Event -> MH ()
handleFileBrowserEvent tId e = do
    let fbHandle ev = sequence . (fmap (FB.handleFileBrowserEvent ev))
    mhHandleEventLensed (csTeam(tId).tsEditState.cedFileBrowser) fbHandle e
    -- TODO: Check file browser exception state
    withFileBrowser tId $ \b ->
        tryAddAttachment $ FB.fileBrowserSelection b

deleteSelectedAttachment :: TeamId -> MH ()
deleteSelectedAttachment tId = do
    es <- use (csTeam(tId).tsEditState.cedAttachmentList.L.listElementsL)
    mSel <- use (csTeam(tId).tsEditState.cedAttachmentList.to L.listSelectedElement)
    case mSel of
        Nothing ->
            return ()
        Just (pos, _) -> do
            oldIdx <- use (csTeam(tId).tsEditState.cedAttachmentList.L.listSelectedL)
            let idx = if Vector.length es == 1
                      then Nothing
                      else case oldIdx of
                          Nothing -> Just 0
                          Just old -> if pos >= old
                                      then Just $ pos - 1
                                      else Just pos
            csTeam(tId).tsEditState.cedAttachmentList %= L.listReplace (deleteAt pos es) idx

deleteAt :: Int -> Vector.Vector a -> Vector.Vector a
deleteAt p as | p < 0 || p >= length as = as
              | otherwise = Vector.take p as <> Vector.drop (p + 1) as
