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

import qualified Brick.Widgets.FileBrowser as FB
import qualified Brick.Widgets.List as L
import qualified Data.Text as T
import qualified Data.Vector as Vector
import qualified Graphics.Vty as V
import           Lens.Micro.Platform ( (?=), (%=), to, Lens', (.=) )

import           Matterhorn.Types
import           Matterhorn.Types.KeyEvents
import           Matterhorn.State.Attachments
import           Matterhorn.State.Common


onEventAttachmentList :: Lens' ChatState (MessageInterface Name i)
                      -> V.Event
                      -> MH Bool
onEventAttachmentList which =
    handleEventWith [ mhHandleKeyboardEvent (attachmentListKeybindings which)
                    , \e -> mhHandleEventLensed (which.miEditor.esAttachmentList) L.handleListEvent e >> return True
                    ]

attachmentListKeybindings :: Lens' ChatState (MessageInterface Name i)
                          -> KeyConfig KeyEvent
                          -> KeyHandlerMap KeyEvent MH
attachmentListKeybindings which = mkKeybindings (attachmentListKeyHandlers which)

attachmentListKeyHandlers :: Lens' ChatState (MessageInterface Name i)
                          -> [MHKeyEventHandler]
attachmentListKeyHandlers which =
    [ mkKb CancelEvent "Close attachment list" $
          which.miMode .= Compose
    , mkKb SelectUpEvent "Move cursor up" $
          mhHandleEventLensed (which.miEditor.esAttachmentList) L.handleListEvent (V.EvKey V.KUp [])
    , mkKb SelectDownEvent "Move cursor down" $
          mhHandleEventLensed (which.miEditor.esAttachmentList) L.handleListEvent (V.EvKey V.KDown [])
    , mkKb AttachmentListAddEvent "Add a new attachment to the attachment list" $
          showAttachmentFileBrowser which
    , mkKb AttachmentOpenEvent "Open the selected attachment using the URL open command" $
          openSelectedAttachment which
    , mkKb AttachmentListDeleteEvent "Delete the selected attachment from the attachment list" $
          deleteSelectedAttachment which
    ]

attachmentBrowseKeybindings :: Lens' ChatState (MessageInterface Name i)
                            -> KeyConfig KeyEvent
                            -> KeyHandlerMap KeyEvent MH
attachmentBrowseKeybindings which =
    mkKeybindings (attachmentBrowseKeyHandlers which)

attachmentBrowseKeyHandlers :: Lens' ChatState (MessageInterface Name i)
                            -> [MHKeyEventHandler]
attachmentBrowseKeyHandlers which =
    [ mkKb CancelEvent "Cancel attachment file browse" $
      cancelAttachmentBrowse which
    , mkKb AttachmentOpenEvent "Open the selected file using the URL open command" $
      openSelectedBrowserEntry which
    , mkKb FileBrowserBeginSearchEvent "Begin search for name in list" $
      mhHandleEventLensed' (which.miEditor.unsafeEsFileBrowser)
        FB.actionFileBrowserBeginSearch
    , mkKb FileBrowserSelectEnterEvent "Select file or enter directory" $ do
      mhHandleEventLensed' (which.miEditor.unsafeEsFileBrowser)
        FB.actionFileBrowserSelectEnter
      withFileBrowser which (tryAddAttachment which . FB.fileBrowserSelection)
    , mkKb FileBrowserSelectCurrentEvent "Select file" $
      mhHandleEventLensed' (which.miEditor.unsafeEsFileBrowser)
        FB.actionFileBrowserSelectCurrent
    , mkKb FileBrowserListPageUpEvent "Move cursor one page up" $
      mhHandleEventLensed' (which.miEditor.unsafeEsFileBrowser)
        FB.actionFileBrowserListPageUp
    , mkKb FileBrowserListPageDownEvent "Move cursor one page down" $
      mhHandleEventLensed' (which.miEditor.unsafeEsFileBrowser)
        FB.actionFileBrowserListPageDown
    , mkKb FileBrowserListHalfPageUpEvent "Move cursor one-half page up" $
      mhHandleEventLensed' (which.miEditor.unsafeEsFileBrowser)
        FB.actionFileBrowserListHalfPageUp
    , mkKb FileBrowserListHalfPageDownEvent "Move cursor one-half page down" $
      mhHandleEventLensed' (which.miEditor.unsafeEsFileBrowser)
        FB.actionFileBrowserListHalfPageDown
    , mkKb FileBrowserListTopEvent "Move cursor to top of list" $
      mhHandleEventLensed' (which.miEditor.unsafeEsFileBrowser)
        FB.actionFileBrowserListTop
    , mkKb FileBrowserListBottomEvent "Move cursor to bottom of list" $
      mhHandleEventLensed' (which.miEditor.unsafeEsFileBrowser)
        FB.actionFileBrowserListBottom
    , mkKb FileBrowserListNextEvent "Move cursor down" $
      mhHandleEventLensed' (which.miEditor.unsafeEsFileBrowser)
        FB.actionFileBrowserListNext
    , mkKb FileBrowserListPrevEvent "Move cursor up" $
      mhHandleEventLensed' (which.miEditor.unsafeEsFileBrowser)
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
    let fbHandle ev = sequence . (fmap (FB.handleFileBrowserEvent ev))
    mhHandleEventLensed (which.miEditor.esFileBrowser) fbHandle e
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
