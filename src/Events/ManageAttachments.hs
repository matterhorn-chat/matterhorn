{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Events.ManageAttachments
  ( onEventManageAttachments
  , attachmentListKeybindings
  , attachmentBrowseKeybindings
  )
where

import           Prelude ()
import           Prelude.MH

import qualified Control.Exception as E
import           Control.Monad ( void )
import qualified Brick.Widgets.FileBrowser as FB
import qualified Brick.Widgets.List as L
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Vector as Vector
import qualified Graphics.Vty as V
import           Lens.Micro.Platform ( (?=), (%=), to )

import           Types
import           Types.KeyEvents
import           Events.Keybindings
import           State.Attachments
import           State.Common


onEventManageAttachments :: V.Event -> MH ()
onEventManageAttachments e = do
    mode <- gets appMode
    case mode of
        ManageAttachments -> void $ onEventAttachmentList e
        ManageAttachmentsBrowseFiles -> onEventBrowseFile e
        _ -> error "BUG: onEventManageAttachments called in invalid mode"

onEventAttachmentList :: V.Event -> MH Bool
onEventAttachmentList =
    handleKeyboardEvent attachmentListKeybindings $
        mhHandleEventLensed (csEditState.cedAttachmentList) L.handleListEvent

attachmentListKeybindings :: KeyConfig -> [Keybinding]
attachmentListKeybindings = mkKeybindings
    [ mkKb CancelEvent "Close attachment list"
          (setMode Main)
    , mkKb SelectUpEvent "Move cursor up" $
          mhHandleEventLensed (csEditState.cedAttachmentList) L.handleListEvent (V.EvKey V.KUp [])
    , mkKb SelectDownEvent "Move cursor down" $
          mhHandleEventLensed (csEditState.cedAttachmentList) L.handleListEvent (V.EvKey V.KDown [])
    , mkKb AttachmentListAddEvent "Add a new attachment to the attachment list"
          showAttachmentFileBrowser
    , mkKb AttachmentOpenEvent "Open the selected attachment using the URL open command"
          openSelectedAttachment
    , mkKb AttachmentListDeleteEvent "Delete the selected attachment from the attachment list"
          deleteSelectedAttachment
    ]

attachmentBrowseKeybindings :: KeyConfig -> [Keybinding]
attachmentBrowseKeybindings = mkKeybindings
    [ mkKb CancelEvent "Cancel attachment file browse"
      cancelAttachmentBrowse
    , mkKb AttachmentOpenEvent "Open the selected file using the URL open command"
          openSelectedBrowserEntry
    ]

withFileBrowser :: ((FB.FileBrowser Name) -> MH ()) -> MH ()
withFileBrowser f = do
    use (csEditState.cedFileBrowser) >>= \case
        Nothing -> do
            -- The widget has not been created yet.  This should
            -- normally not occur, because the ManageAttachments
            -- events should not fire when there is no FileBrowser
            -- Widget active to cause Brick to generate these events.
            -- This could therefore be implemented as an `error "BUG:
            -- ..."` handler, but the more benign approach is to
            -- simply create an available FileBrowser at this stage.
            new_b <- liftIO $ FB.newFileBrowser FB.selectNonDirectories AttachmentFileBrowser Nothing
            csEditState.cedFileBrowser ?= new_b
            f new_b
        Just b -> f b

openSelectedAttachment :: MH ()
openSelectedAttachment = do
    cur <- use (csEditState.cedAttachmentList.to L.listSelectedElement)
    case cur of
        Nothing -> return ()
        Just (_, entry) -> void $ openURL (OpenLocalFile $ FB.fileInfoFilePath $
                                           attachmentDataFileInfo entry)

openSelectedBrowserEntry :: MH ()
openSelectedBrowserEntry = withFileBrowser $ \b ->
    case FB.fileBrowserCursor b of
        Nothing -> return ()
        Just entry -> void $ openURL (OpenLocalFile $ FB.fileInfoFilePath entry)

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
    es <- use (csEditState.cedAttachmentList.L.listElementsL)
    case length es of
        0 -> setMode Main
        _ -> setMode ManageAttachments

handleFileBrowserEvent :: V.Event -> MH ()
handleFileBrowserEvent e = do
  let fbHandle ev = sequence . (fmap (FB.handleFileBrowserEvent ev))
  mhHandleEventLensed (csEditState.cedFileBrowser) fbHandle e

  withFileBrowser $ \b -> do
    -- TODO: Check file browser exception state
    let entries = FB.fileBrowserSelection b
    forM_ entries $ \entry -> do
        -- Is the entry already present? If so, ignore the selection.
        es <- use (csEditState.cedAttachmentList.L.listElementsL)
        let matches = (== (FB.fileInfoFilePath entry)) .
                      FB.fileInfoFilePath .
                      attachmentDataFileInfo
        case Vector.find matches es of
            Just _ -> return ()
            Nothing -> do
                let path = FB.fileInfoFilePath entry
                readResult <- liftIO $ E.try $ BS.readFile path
                case readResult of
                    Left (_::E.SomeException) ->
                        -- TODO: report the error
                        return ()
                    Right bytes -> do
                        let a = AttachmentData { attachmentDataFileInfo = entry
                                               , attachmentDataBytes = bytes
                                               }
                        oldIdx <- use (csEditState.cedAttachmentList.L.listSelectedL)
                        let newIdx = if Vector.null es
                                     then Just 0
                                     else oldIdx
                        csEditState.cedAttachmentList %= L.listReplace (Vector.snoc es a) newIdx

    when (not $ null entries) $ setMode Main

deleteSelectedAttachment :: MH ()
deleteSelectedAttachment = do
    es <- use (csEditState.cedAttachmentList.L.listElementsL)
    mSel <- use (csEditState.cedAttachmentList.to L.listSelectedElement)
    case mSel of
        Nothing ->
            return ()
        Just (pos, _) -> do
            oldIdx <- use (csEditState.cedAttachmentList.L.listSelectedL)
            let idx = if Vector.length es == 1
                      then Nothing
                      else case oldIdx of
                          Nothing -> Just 0
                          Just old -> if pos >= old
                                      then Just $ pos - 1
                                      else Just pos
            csEditState.cedAttachmentList %= L.listReplace (deleteAt pos es) idx

deleteAt :: Int -> Vector.Vector a -> Vector.Vector a
deleteAt p as | p < 0 || p >= length as = as
              | otherwise = Vector.take p as <> Vector.drop (p + 1) as
