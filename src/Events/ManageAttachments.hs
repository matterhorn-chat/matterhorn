{-# LANGUAGE ScopedTypeVariables #-}
module Events.ManageAttachments
  ( onEventManageAttachments
  )
where

import           Prelude ()
import           Prelude.MH

import qualified Control.Exception as E
import qualified Brick.Widgets.FileBrowser as FB
import qualified Brick.Widgets.List as L
import qualified Data.ByteString as BS
import qualified Data.Vector as Vector
import qualified Graphics.Vty as V
import           Lens.Micro.Platform ( (%=), (.=), to )

import           Types
import           State.Attachments


onEventManageAttachments :: V.Event -> MH ()
onEventManageAttachments e = do
    mode <- gets appMode
    case mode of
        ManageAttachments -> onEventAttachmentList e
        ManageAttachmentsBrowseFiles -> onEventBrowseFile e
        _ -> error "BUG: onEventManageAttachments called in invalid mode"

onEventAttachmentList :: V.Event -> MH ()
onEventAttachmentList e =
    case e of
        V.EvKey V.KEsc [] ->
            setMode Main
        V.EvKey (V.KChar 'c') [V.MCtrl] ->
            setMode Main
        V.EvKey (V.KChar 'a') [] -> do
            showAttachmentFileBrowser
        V.EvKey (V.KChar 'd') [] -> do
            -- Delete the selected element and update the list
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
        _ ->
            mhHandleEventLensed (csEditState.cedAttachmentList) L.handleListEvent e

deleteAt :: Int -> Vector.Vector a -> Vector.Vector a
deleteAt p as | p < 0 || p >= length as = as
              | otherwise = Vector.take p as <> Vector.drop (p + 1) as

onEventBrowseFile :: V.Event -> MH ()
onEventBrowseFile e = do
    b <- use (csEditState.cedFileBrowser)
    case e of
        V.EvKey (V.KChar 'c') [V.MCtrl] | not (FB.fileBrowserIsSearching b) -> do
            es <- use (csEditState.cedAttachmentList.L.listElementsL)
            case length es of
                0 -> setMode Main
                _ -> setMode ManageAttachments
        V.EvKey V.KEsc [] | not (FB.fileBrowserIsSearching b) -> do
            es <- use (csEditState.cedAttachmentList.L.listElementsL)
            case length es of
                0 -> setMode Main
                _ -> setMode ManageAttachments
        _ -> do
            b' <- mh $ FB.handleFileBrowserEvent e b
            -- TODO: Check file browser exception state
            csEditState.cedFileBrowser .= b'
            case FB.fileBrowserSelection b' of
                Nothing -> return ()
                Just entry -> do
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
                            setMode ManageAttachments
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
                                    setMode Main
