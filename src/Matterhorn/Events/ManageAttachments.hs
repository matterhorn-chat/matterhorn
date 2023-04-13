{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module Matterhorn.Events.ManageAttachments
  ( onEventAttachmentList
  , onEventBrowseFile
  , attachmentListKeybindings
  , attachmentBrowseKeyHandlers
  , attachmentBrowseKeybindings
  , attachmentListKeyHandlers
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick.Keybindings
import qualified Brick.Widgets.FileBrowser as FB
import qualified Brick.Widgets.List as L
import qualified Data.Text as T
import qualified Data.Vector as Vector
import qualified Graphics.Vty as V
import           Lens.Micro.Platform ( (?=), (%=), to, Lens', (.=) )

import           Matterhorn.Types
import           Matterhorn.State.Attachments
import           Matterhorn.State.Common


onEventAttachmentList :: Lens' ChatState (MessageInterface Name i)
                      -> V.Event
                      -> MH Bool
onEventAttachmentList which =
    handleEventWith [ mhHandleKeyboardEvent (attachmentListKeybindings which)
                    , \e -> mhZoom (which.miEditor.esAttachmentList) L.handleListEvent e >> return True
                    ]

attachmentListKeybindings :: Lens' ChatState (MessageInterface Name i)
                          -> KeyConfig KeyEvent
                          -> KeyDispatcher KeyEvent MH
attachmentListKeybindings which kc = unsafeKeyDispatcher kc (attachmentListKeyHandlers which)

attachmentListKeyHandlers :: Lens' ChatState (MessageInterface Name i)
                          -> [MHKeyEventHandler]
attachmentListKeyHandlers which =
    [ onEvent CancelEvent "Close attachment list" $
          which.miMode .= Compose
    , onEvent SelectUpEvent "Move cursor up" $
          mhZoom (which.miEditor.esAttachmentList) L.handleListEvent (V.EvKey V.KUp [])
    , onEvent SelectDownEvent "Move cursor down" $
          mhZoom (which.miEditor.esAttachmentList) L.handleListEvent (V.EvKey V.KDown [])
    , onEvent AttachmentListAddEvent "Add a new attachment to the attachment list" $
          showAttachmentFileBrowser which
    , onEvent AttachmentOpenEvent "Open the selected attachment using the URL open command" $
          openSelectedAttachment which
    , onEvent AttachmentListDeleteEvent "Delete the selected attachment from the attachment list" $
          deleteSelectedAttachment which
    ]

attachmentBrowseKeybindings :: Lens' ChatState (MessageInterface Name i)
                            -> KeyConfig KeyEvent
                            -> KeyDispatcher KeyEvent MH
attachmentBrowseKeybindings which kc =
    unsafeKeyDispatcher kc (attachmentBrowseKeyHandlers which)

attachmentBrowseKeyHandlers :: Lens' ChatState (MessageInterface Name i)
                            -> [MHKeyEventHandler]
attachmentBrowseKeyHandlers which =
    [ onEvent CancelEvent "Cancel attachment file browse" $
      cancelAttachmentBrowse which
    , onEvent AttachmentOpenEvent "Open the selected file using the URL open command" $
      openSelectedBrowserEntry which
    , onEvent FileBrowserBeginSearchEvent "Begin search for name in list" $
      mhZoom' (which.miEditor.unsafeEsFileBrowser)
        FB.actionFileBrowserBeginSearch
    , onEvent FileBrowserSelectEnterEvent "Select file or enter directory" $ do
      mhZoom' (which.miEditor.unsafeEsFileBrowser)
        FB.actionFileBrowserSelectEnter
      withFileBrowser which (tryAddAttachment which . FB.fileBrowserSelection)
    , onEvent FileBrowserSelectCurrentEvent "Select file" $
      mhZoom' (which.miEditor.unsafeEsFileBrowser)
        FB.actionFileBrowserSelectCurrent
    , onEvent FileBrowserListPageUpEvent "Move cursor one page up" $
      mhZoom' (which.miEditor.unsafeEsFileBrowser)
        FB.actionFileBrowserListPageUp
    , onEvent FileBrowserListPageDownEvent "Move cursor one page down" $
      mhZoom' (which.miEditor.unsafeEsFileBrowser)
        FB.actionFileBrowserListPageDown
    , onEvent FileBrowserListHalfPageUpEvent "Move cursor one-half page up" $
      mhZoom' (which.miEditor.unsafeEsFileBrowser)
        FB.actionFileBrowserListHalfPageUp
    , onEvent FileBrowserListHalfPageDownEvent "Move cursor one-half page down" $
      mhZoom' (which.miEditor.unsafeEsFileBrowser)
        FB.actionFileBrowserListHalfPageDown
    , onEvent FileBrowserListTopEvent "Move cursor to top of list" $
      mhZoom' (which.miEditor.unsafeEsFileBrowser)
        FB.actionFileBrowserListTop
    , onEvent FileBrowserListBottomEvent "Move cursor to bottom of list" $
      mhZoom' (which.miEditor.unsafeEsFileBrowser)
        FB.actionFileBrowserListBottom
    , onEvent FileBrowserListNextEvent "Move cursor down" $
      mhZoom' (which.miEditor.unsafeEsFileBrowser)
        FB.actionFileBrowserListNext
    , onEvent FileBrowserListPrevEvent "Move cursor up" $
      mhZoom' (which.miEditor.unsafeEsFileBrowser)
        FB.actionFileBrowserListPrev
    ]

withFileBrowser :: Lens' ChatState (MessageInterface Name i)
                -> ((FB.FileBrowser Name) -> MH ())
                -> MH ()
withFileBrowser which f = do
    use (which.miEditor.esFileBrowser) >>= \case
        Nothing -> do
            -- The widget has not been created yet.  This should
            -- normally not occur, because the ManageAttachments
            -- events should not fire when there is no FileBrowser
            -- Widget active to cause Brick to generate these events.
            -- This could therefore be implemented as an `error "BUG:
            -- ..."` handler, but the more benign approach is to
            -- simply create an available FileBrowser at this stage.
            cId <- use (which.miEditor.esChannelId)
            new_b <- liftIO $ FB.newFileBrowser FB.selectNonDirectories (AttachmentFileBrowser cId) Nothing
            which.miEditor.esFileBrowser ?= new_b
            f new_b
        Just b -> f b

openSelectedAttachment :: Lens' ChatState (MessageInterface Name i) -> MH ()
openSelectedAttachment which = do
    cur <- use (which.miEditor.esAttachmentList.to L.listSelectedElement)
    case cur of
        Nothing -> return ()
        Just (_, entry) -> void $ openFilePath (FB.fileInfoFilePath $
                                                attachmentDataFileInfo entry)

openSelectedBrowserEntry :: Lens' ChatState (MessageInterface Name i) -> MH ()
openSelectedBrowserEntry which = withFileBrowser which $ \b ->
    case FB.fileBrowserCursor b of
        Nothing -> return ()
        Just entry -> void $ openFilePath (FB.fileInfoFilePath entry)

onEventBrowseFile :: Lens' ChatState (MessageInterface Name i) -> V.Event -> MH Bool
onEventBrowseFile which e = do
    withFileBrowser which $ \b -> do
        case FB.fileBrowserIsSearching b of
            False ->
                void $ handleEventWith [ mhHandleKeyboardEvent (attachmentBrowseKeybindings which)
                                       , \_ -> handleFileBrowserEvent which e >> return True
                                       ] e
            True ->
                handleFileBrowserEvent which e

    -- n.b. the FileBrowser may have been updated above, so re-acquire it
    withFileBrowser which $ \b -> do
        case FB.fileBrowserException b of
            Nothing -> return ()
            Just ex -> do
                mhLog LogError $ T.pack $ "FileBrowser exception: " <> show ex

    return True

cancelAttachmentBrowse :: Lens' ChatState (MessageInterface Name i) -> MH ()
cancelAttachmentBrowse which = do
    es <- use (which.miEditor.esAttachmentList.L.listElementsL)
    which.miMode .= case length es of
        0 -> Compose
        _ -> ManageAttachments

handleFileBrowserEvent :: Lens' ChatState (MessageInterface Name i) -> V.Event -> MH ()
handleFileBrowserEvent which e = do
    mhZoom (which.miEditor.unsafeEsFileBrowser) FB.handleFileBrowserEvent e
    -- TODO: Check file browser exception state
    withFileBrowser which $ \b ->
        tryAddAttachment which (FB.fileBrowserSelection b)

deleteSelectedAttachment :: Lens' ChatState (MessageInterface Name i) -> MH ()
deleteSelectedAttachment which = do
    es <- use (which.miEditor.esAttachmentList.L.listElementsL)
    mSel <- use (which.miEditor.esAttachmentList.to L.listSelectedElement)
    case mSel of
        Nothing ->
            return ()
        Just (pos, _) -> do
            oldIdx <- use (which.miEditor.esAttachmentList.L.listSelectedL)
            let idx = if Vector.length es == 1
                      then Nothing
                      else case oldIdx of
                          Nothing -> Just 0
                          Just old -> if pos >= old
                                      then Just $ pos - 1
                                      else Just pos
            which.miEditor.esAttachmentList %= L.listReplace (deleteAt pos es) idx

deleteAt :: Int -> Vector.Vector a -> Vector.Vector a
deleteAt p as | p < 0 || p >= length as = as
              | otherwise = Vector.take p as <> Vector.drop (p + 1) as
