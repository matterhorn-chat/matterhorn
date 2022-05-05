{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
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
import           Lens.Micro.Platform ( (?=), (%=), to, Lens' )

import           Network.Mattermost.Types ( TeamId )

import           Matterhorn.Types
import           Matterhorn.Types.KeyEvents
import           Matterhorn.Events.Keybindings
import           Matterhorn.State.Attachments
import           Matterhorn.State.Common


onEventManageAttachments :: TeamId -> Lens' ChatState EditState -> V.Event -> MH ()
onEventManageAttachments tId which e = do
    mode <- use (csTeam(tId).tsMode)
    case mode of
        ManageAttachments -> void $ onEventAttachmentList tId which e
        ManageAttachmentsBrowseFiles -> onEventBrowseFile tId which e
        _ -> error "BUG: onEventManageAttachments called in invalid mode"

onEventAttachmentList :: TeamId -> Lens' ChatState EditState -> V.Event -> MH Bool
onEventAttachmentList tId which =
    handleKeyboardEvent (attachmentListKeybindings tId which) $
        mhHandleEventLensed (which.cedAttachmentList) L.handleListEvent

attachmentListKeybindings :: TeamId -> Lens' ChatState EditState -> KeyConfig -> KeyHandlerMap
attachmentListKeybindings tId which = mkKeybindings (attachmentListKeyHandlers tId which)

attachmentListKeyHandlers :: TeamId -> Lens' ChatState EditState -> [KeyEventHandler]
attachmentListKeyHandlers tId which =
    [ mkKb CancelEvent "Close attachment list" $
          popMode tId
    , mkKb SelectUpEvent "Move cursor up" $
          mhHandleEventLensed (which.cedAttachmentList) L.handleListEvent (V.EvKey V.KUp [])
    , mkKb SelectDownEvent "Move cursor down" $
          mhHandleEventLensed (which.cedAttachmentList) L.handleListEvent (V.EvKey V.KDown [])
    , mkKb AttachmentListAddEvent "Add a new attachment to the attachment list" $
          showAttachmentFileBrowser tId which
    , mkKb AttachmentOpenEvent "Open the selected attachment using the URL open command" $
          openSelectedAttachment which
    , mkKb AttachmentListDeleteEvent "Delete the selected attachment from the attachment list" $
          deleteSelectedAttachment which
    ]

attachmentBrowseKeybindings :: TeamId -> Lens' ChatState EditState -> KeyConfig -> KeyHandlerMap
attachmentBrowseKeybindings tId which = mkKeybindings (attachmentBrowseKeyHandlers tId which)

attachmentBrowseKeyHandlers :: TeamId -> Lens' ChatState EditState -> [KeyEventHandler]
attachmentBrowseKeyHandlers tId which =
    [ mkKb CancelEvent "Cancel attachment file browse" $
      cancelAttachmentBrowse tId which
    , mkKb AttachmentOpenEvent "Open the selected file using the URL open command" $
      openSelectedBrowserEntry tId which
    , mkKb FileBrowserBeginSearchEvent "Begin search for name in list" $
      mhHandleEventLensed' (which.unsafeCedFileBrowser)
        FB.actionFileBrowserBeginSearch
    , mkKb FileBrowserSelectEnterEvent "Select file or enter directory" $ do
      mhHandleEventLensed' (which.unsafeCedFileBrowser)
        FB.actionFileBrowserSelectEnter
      withFileBrowser tId which (tryAddAttachment tId which . FB.fileBrowserSelection)
    , mkKb FileBrowserSelectCurrentEvent "Select file" $
      mhHandleEventLensed' (which.unsafeCedFileBrowser)
        FB.actionFileBrowserSelectCurrent
    , mkKb FileBrowserListPageUpEvent "Move cursor one page up" $
      mhHandleEventLensed' (which.unsafeCedFileBrowser)
        FB.actionFileBrowserListPageUp
    , mkKb FileBrowserListPageDownEvent "Move cursor one page down" $
      mhHandleEventLensed' (which.unsafeCedFileBrowser)
        FB.actionFileBrowserListPageDown
    , mkKb FileBrowserListHalfPageUpEvent "Move cursor one-half page up" $
      mhHandleEventLensed' (which.unsafeCedFileBrowser)
        FB.actionFileBrowserListHalfPageUp
    , mkKb FileBrowserListHalfPageDownEvent "Move cursor one-half page down" $
      mhHandleEventLensed' (which.unsafeCedFileBrowser)
        FB.actionFileBrowserListHalfPageDown
    , mkKb FileBrowserListTopEvent "Move cursor to top of list" $
      mhHandleEventLensed' (which.unsafeCedFileBrowser)
        FB.actionFileBrowserListTop
    , mkKb FileBrowserListBottomEvent "Move cursor to bottom of list" $
      mhHandleEventLensed' (which.unsafeCedFileBrowser)
        FB.actionFileBrowserListBottom
    , mkKb FileBrowserListNextEvent "Move cursor down" $
      mhHandleEventLensed' (which.unsafeCedFileBrowser)
        FB.actionFileBrowserListNext
    , mkKb FileBrowserListPrevEvent "Move cursor up" $
      mhHandleEventLensed' (which.unsafeCedFileBrowser)
        FB.actionFileBrowserListPrev
    ]

withFileBrowser :: TeamId -> Lens' ChatState EditState -> ((FB.FileBrowser Name) -> MH ()) -> MH ()
withFileBrowser tId which f = do
    use (which.cedFileBrowser) >>= \case
        Nothing -> do
            -- The widget has not been created yet.  This should
            -- normally not occur, because the ManageAttachments
            -- events should not fire when there is no FileBrowser
            -- Widget active to cause Brick to generate these events.
            -- This could therefore be implemented as an `error "BUG:
            -- ..."` handler, but the more benign approach is to
            -- simply create an available FileBrowser at this stage.
            new_b <- liftIO $ FB.newFileBrowser FB.selectNonDirectories (AttachmentFileBrowser tId) Nothing
            which.cedFileBrowser ?= new_b
            f new_b
        Just b -> f b

openSelectedAttachment :: Lens' ChatState EditState -> MH ()
openSelectedAttachment which = do
    cur <- use (which.cedAttachmentList.to L.listSelectedElement)
    case cur of
        Nothing -> return ()
        Just (_, entry) -> void $ openFilePath (FB.fileInfoFilePath $
                                                attachmentDataFileInfo entry)

openSelectedBrowserEntry :: TeamId -> Lens' ChatState EditState -> MH ()
openSelectedBrowserEntry tId which = withFileBrowser tId which $ \b ->
    case FB.fileBrowserCursor b of
        Nothing -> return ()
        Just entry -> void $ openFilePath (FB.fileInfoFilePath entry)

onEventBrowseFile :: TeamId -> Lens' ChatState EditState -> V.Event -> MH ()
onEventBrowseFile tId which e = do
    withFileBrowser tId which $ \b -> do
        case FB.fileBrowserIsSearching b of
            False ->
                void $ handleKeyboardEvent (attachmentBrowseKeybindings tId which) (handleFileBrowserEvent tId which) e
            True ->
                handleFileBrowserEvent tId which e

    -- n.b. the FileBrowser may have been updated above, so re-acquire it
    withFileBrowser tId which $ \b -> do
        case FB.fileBrowserException b of
            Nothing -> return ()
            Just ex -> do
                mhLog LogError $ T.pack $ "FileBrowser exception: " <> show ex

cancelAttachmentBrowse :: TeamId -> Lens' ChatState EditState -> MH ()
cancelAttachmentBrowse tId which = do
    es <- use (which.cedAttachmentList.L.listElementsL)
    case length es of
        0 -> popMode tId
        _ -> replaceMode tId ManageAttachments

handleFileBrowserEvent :: TeamId -> Lens' ChatState EditState -> V.Event -> MH ()
handleFileBrowserEvent tId which e = do
    let fbHandle ev = sequence . (fmap (FB.handleFileBrowserEvent ev))
    mhHandleEventLensed (which.cedFileBrowser) fbHandle e
    -- TODO: Check file browser exception state
    withFileBrowser tId which $ \b ->
        tryAddAttachment tId which (FB.fileBrowserSelection b)

deleteSelectedAttachment :: Lens' ChatState EditState -> MH ()
deleteSelectedAttachment which = do
    es <- use (which.cedAttachmentList.L.listElementsL)
    mSel <- use (which.cedAttachmentList.to L.listSelectedElement)
    case mSel of
        Nothing ->
            return ()
        Just (pos, _) -> do
            oldIdx <- use (which.cedAttachmentList.L.listSelectedL)
            let idx = if Vector.length es == 1
                      then Nothing
                      else case oldIdx of
                          Nothing -> Just 0
                          Just old -> if pos >= old
                                      then Just $ pos - 1
                                      else Just pos
            which.cedAttachmentList %= L.listReplace (deleteAt pos es) idx

deleteAt :: Int -> Vector.Vector a -> Vector.Vector a
deleteAt p as | p < 0 || p >= length as = as
              | otherwise = Vector.take p as <> Vector.drop (p + 1) as
