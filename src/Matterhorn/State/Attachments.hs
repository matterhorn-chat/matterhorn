{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
module Matterhorn.State.Attachments
  ( showAttachmentList
  , resetAttachmentList
  , showAttachmentFileBrowser
  , attachFileByPath
  , tryAddAttachment
  , tryReadAttachment
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick ( vScrollToBeginning, viewportScroll )
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.FileBrowser as FB
import qualified Control.Exception as E
import           Data.Bool ( bool )
import qualified Data.ByteString as BS
import           Data.Either ( isRight )
import           Data.Text ( unpack )
import qualified Data.Vector as Vector
import           GHC.Exception ( toException )
import           Lens.Micro.Platform ( (.=), (%=), Lens' )
import           System.Directory ( doesDirectoryExist, doesFileExist, getDirectoryContents )

import           Network.Mattermost.Types ( TeamId )

import           Matterhorn.Types

validateAttachmentPath :: FilePath -> IO (Maybe FilePath)
validateAttachmentPath path = bool Nothing (Just path) <$> do
    ex <- doesDirectoryExist path
    case ex of
        False -> return False
        True -> do
            result :: Either E.SomeException [FilePath]
                   <- E.try $ getDirectoryContents path
            return $ isRight result

defaultAttachmentsPath :: Config -> IO (Maybe FilePath)
defaultAttachmentsPath = maybe (return Nothing) validateAttachmentPath . configDefaultAttachmentPath

showAttachmentList :: TeamId -> Lens' ChatState EditState -> Mode -> MH ()
showAttachmentList tId which m = do
    lst <- use (which.cedAttachmentList)
    case length (L.listElements lst) of
        0 -> showAttachmentFileBrowser tId which m
        _ -> pushMode tId ManageAttachments

resetAttachmentList :: TeamId -> Lens' ChatState EditState -> MH ()
resetAttachmentList tId which = do
    let listName = AttachmentList tId
    which.cedAttachmentList .= L.list listName mempty 1
    mh $ vScrollToBeginning $ viewportScroll listName

showAttachmentFileBrowser :: TeamId -> Lens' ChatState EditState -> Mode -> MH ()
showAttachmentFileBrowser tId which m = do
    config <- use (csResources.crConfiguration)
    filePath <- liftIO $ defaultAttachmentsPath config
    browser <- liftIO $ Just <$> FB.newFileBrowser FB.selectNonDirectories (AttachmentFileBrowser tId) filePath
    which.cedFileBrowser .= browser
    pushMode tId m

attachFileByPath :: TeamId -> Lens' ChatState EditState -> Text -> MH ()
attachFileByPath tId which txtPath = do
    let strPath = unpack txtPath
    fileInfo <- liftIO $ FB.getFileInfo strPath strPath
    case FB.fileInfoFileStatus fileInfo of
        Left e -> do
            mhError $ AttachmentException (toException e)
        Right _ -> tryAddAttachment tId which [fileInfo]

checkPathIsFile :: FB.FileInfo -> MH Bool
checkPathIsFile = liftIO . doesFileExist . FB.fileInfoFilePath

tryAddAttachment :: TeamId -> Lens' ChatState EditState -> [FB.FileInfo] -> MH ()
tryAddAttachment tId which entries = do
    forM_ entries $ \entry -> do
        isFile <- checkPathIsFile entry
        if not isFile
        then mhError (BadAttachmentPath
            "Error attaching file. It either doesn't exist or is a directory, which is not supported.")
        else do
            -- Is the entry already present? If so, ignore the selection.
            es <- use (which.cedAttachmentList.L.listElementsL)
            let matches = (== FB.fileInfoFilePath entry) .
                              FB.fileInfoFilePath .
                              attachmentDataFileInfo
            case Vector.find matches es of
                Just _ -> return ()
                Nothing -> do
                    tryReadAttachment entry >>= \case
                        Right a -> do
                            oldIdx <- use (which.cedAttachmentList.L.listSelectedL)
                            let newIdx = if Vector.null es
                                         then Just 0
                                         else oldIdx
                            which.cedAttachmentList %= L.listReplace (Vector.snoc es a) newIdx
                        Left e -> mhError $ AttachmentException e

    when (not $ null entries) $ popMode tId

tryReadAttachment :: FB.FileInfo -> MH (Either E.SomeException AttachmentData)
tryReadAttachment fi = do
    let path = FB.fileInfoFilePath fi
    readResult <- liftIO $ E.try $ BS.readFile path
    case readResult of
        Right bytes -> do
            return $ Right $
                AttachmentData { attachmentDataFileInfo = fi
                               , attachmentDataBytes = bytes
                               }
        Left e -> return $ Left e
