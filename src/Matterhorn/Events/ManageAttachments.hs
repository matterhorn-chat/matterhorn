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

import           Matterhorn.Types
import           Matterhorn.Types.KeyEvents
import           Matterhorn.Events.Keybindings
import           Matterhorn.State.Attachments
import           Matterhorn.State.Common


onEventManageAttachments :: V.Event -> MH ()
onEventManageAttachments e = do
    mode <- use (csCurrentTeam.tsMode)
    case mode of
        ManageAttachments -> void $ onEventAttachmentList e
        ManageAttachmentsBrowseFiles -> onEventBrowseFile e
        _ -> error "BUG: onEventManageAttachments called in invalid mode"

onEventAttachmentList :: V.Event -> MH Bool
onEventAttachmentList =
    handleKeyboardEvent attachmentListKeybindings $
        mhHandleEventLensed (csCurrentTeam.tsEditState.cedAttachmentList) L.handleListEvent

attachmentListKeybindings :: KeyConfig -> KeyHandlerMap
attachmentListKeybindings = mkKeybindings attachmentListKeyHandlers

attachmentListKeyHandlers :: [KeyEventHandler]
attachmentListKeyHandlers =
    [ mkKb CancelEvent "Close attachment list"
          (setMode Main)
    , mkKb SelectUpEvent "Move cursor up" $
          mhHandleEventLensed (csCurrentTeam.tsEditState.cedAttachmentList) L.handleListEvent (V.EvKey V.KUp [])
    , mkKb SelectDownEvent "Move cursor down" $
          mhHandleEventLensed (csCurrentTeam.tsEditState.cedAttachmentList) L.handleListEvent (V.EvKey V.KDown [])
    , mkKb AttachmentListAddEvent "Add a new attachment to the attachment list"
          showAttachmentFileBrowser
    , mkKb AttachmentOpenEvent "Open the selected attachment using the URL open command"
          openSelectedAttachment
    , mkKb AttachmentListDeleteEvent "Delete the selected attachment from the attachment list"
          deleteSelectedAttachment
    ]

attachmentBrowseKeybindings :: KeyConfig -> KeyHandlerMap
attachmentBrowseKeybindings = mkKeybindings attachmentBrowseKeyHandlers

attachmentBrowseKeyHandlers :: [KeyEventHandler]
attachmentBrowseKeyHandlers =
    [ mkKb CancelEvent "Cancel attachment file browse"
      cancelAttachmentBrowse
    , mkKb AttachmentOpenEvent "Open the selected file using the URL open command"
      openSelectedBrowserEntry
    , mkKb FileBrowserBeginSearchEvent "Begin search for name in list" $
      mhHandleEventLensed' (csCurrentTeam.tsEditState.unsafeCedFileBrowser)
        FB.actionFileBrowserBeginSearch
    , mkKb FileBrowserSelectEnterEvent "Select file or enter directory" $ do
      mhHandleEventLensed' (csCurrentTeam.tsEditState.unsafeCedFileBrowser)
        FB.actionFileBrowserSelectEnter
      withFileBrowser (tryAddAttachment . FB.fileBrowserSelection)
    , mkKb FileBrowserSelectCurrentEvent "Select file" $
      mhHandleEventLensed' (csCurrentTeam.tsEditState.unsafeCedFileBrowser)
        FB.actionFileBrowserSelectCurrent
    , mkKb FileBrowserListPageUpEvent "Move cursor one page up" $
      mhHandleEventLensed' (csCurrentTeam.tsEditState.unsafeCedFileBrowser)
        FB.actionFileBrowserListPageUp
    , mkKb FileBrowserListPageDownEvent "Move cursor one page down" $
      mhHandleEventLensed' (csCurrentTeam.tsEditState.unsafeCedFileBrowser)
        FB.actionFileBrowserListPageDown
    , mkKb FileBrowserListHalfPageUpEvent "Move cursor one-half page up" $
      mhHandleEventLensed' (csCurrentTeam.tsEditState.unsafeCedFileBrowser)
        FB.actionFileBrowserListHalfPageUp
    , mkKb FileBrowserListHalfPageDownEvent "Move cursor one-half page down" $
      mhHandleEventLensed' (csCurrentTeam.tsEditState.unsafeCedFileBrowser)
        FB.actionFileBrowserListHalfPageDown
    , mkKb FileBrowserListTopEvent "Move cursor to top of list" $
      mhHandleEventLensed' (csCurrentTeam.tsEditState.unsafeCedFileBrowser)
        FB.actionFileBrowserListTop
    , mkKb FileBrowserListBottomEvent "Move cursor to bottom of list" $
      mhHandleEventLensed' (csCurrentTeam.tsEditState.unsafeCedFileBrowser)
        FB.actionFileBrowserListBottom
    , mkKb FileBrowserListNextEvent "Move cursor down" $
      mhHandleEventLensed' (csCurrentTeam.tsEditState.unsafeCedFileBrowser)
        FB.actionFileBrowserListNext
    , mkKb FileBrowserListPrevEvent "Move cursor up" $
      mhHandleEventLensed' (csCurrentTeam.tsEditState.unsafeCedFileBrowser)
        FB.actionFileBrowserListPrev
    ]

withFileBrowser :: ((FB.FileBrowser Name) -> MH ()) -> MH ()
withFileBrowser f = do
    use (csCurrentTeam.tsEditState.cedFileBrowser) >>= \case
        Nothing -> do
            -- The widget has not been created yet.  This should
            -- normally not occur, because the ManageAttachments
            -- events should not fire when there is no FileBrowser
            -- Widget active to cause Brick to generate these events.
            -- This could therefore be implemented as an `error "BUG:
            -- ..."` handler, but the more benign approach is to
            -- simply create an available FileBrowser at this stage.
            tId <- use csCurrentTeamId
            new_b <- liftIO $ FB.newFileBrowser FB.selectNonDirectories (AttachmentFileBrowser tId) Nothing
            csCurrentTeam.tsEditState.cedFileBrowser ?= new_b
            f new_b
        Just b -> f b

openSelectedAttachment :: MH ()
openSelectedAttachment = do
    cur <- use (csCurrentTeam.tsEditState.cedAttachmentList.to L.listSelectedElement)
    case cur of
        Nothing -> return ()
        Just (_, entry) -> void $ openFilePath (FB.fileInfoFilePath $
                                                attachmentDataFileInfo entry)

openSelectedBrowserEntry :: MH ()
openSelectedBrowserEntry = withFileBrowser $ \b ->
    case FB.fileBrowserCursor b of
        Nothing -> return ()
        Just entry -> void $ openFilePath (FB.fileInfoFilePath entry)

onEventBrowseFile :: V.Event -> MH ()
onEventBrowseFile e = do
    withFileBrowser $ \b -> do
        case FB.fileBrowserIsSearching b of
            False ->
                void $ handleKeyboardEvent attachmentBrowseKeybindings handleFileBrowserEvent e
            True ->
                handleFileBrowserEvent e

    -- n.b. the FileBrowser may have been updated above, so re-acquire it
    withFileBrowser $ \b -> do
        case FB.fileBrowserException b of
            Nothing -> return ()
            Just ex -> do
                mhLog LogError $ T.pack $ "FileBrowser exception: " <> show ex

cancelAttachmentBrowse :: MH ()
cancelAttachmentBrowse = do
    es <- use (csCurrentTeam.tsEditState.cedAttachmentList.L.listElementsL)
    case length es of
        0 -> setMode Main
        _ -> setMode ManageAttachments

handleFileBrowserEvent :: V.Event -> MH ()
handleFileBrowserEvent e = do
    let fbHandle ev = sequence . (fmap (FB.handleFileBrowserEvent ev))
    mhHandleEventLensed (csCurrentTeam.tsEditState.cedFileBrowser) fbHandle e
    -- TODO: Check file browser exception state
    withFileBrowser $ \b ->
        tryAddAttachment $ FB.fileBrowserSelection b

deleteSelectedAttachment :: MH ()
deleteSelectedAttachment = do
    es <- use (csCurrentTeam.tsEditState.cedAttachmentList.L.listElementsL)
    mSel <- use (csCurrentTeam.tsEditState.cedAttachmentList.to L.listSelectedElement)
    case mSel of
        Nothing ->
            return ()
        Just (pos, _) -> do
            oldIdx <- use (csCurrentTeam.tsEditState.cedAttachmentList.L.listSelectedL)
            let idx = if Vector.length es == 1
                      then Nothing
                      else case oldIdx of
                          Nothing -> Just 0
                          Just old -> if pos >= old
                                      then Just $ pos - 1
                                      else Just pos
            csCurrentTeam.tsEditState.cedAttachmentList %= L.listReplace (deleteAt pos es) idx

deleteAt :: Int -> Vector.Vector a -> Vector.Vector a
deleteAt p as | p < 0 || p >= length as = as
              | otherwise = Vector.take p as <> Vector.drop (p + 1) as
