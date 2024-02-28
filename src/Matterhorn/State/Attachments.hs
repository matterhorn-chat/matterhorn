{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
module Matterhorn.State.Attachments
  ( showAttachmentList
  , showAttachmentFileBrowser
  , attachFileByPath
  , tryAddAttachment
  , tryReadAttachment
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.FileBrowser as FB
import qualified Control.Exception as E
import           Data.Bool ( bool )
import qualified Data.ByteString as BS
import           Data.Either ( isRight )
import           Data.Text ( unpack, replace, pack )
import qualified Data.Vector as Vector
import           GHC.Exception ( toException )
import           Lens.Micro.Platform ( (.=), (%=), Lens' )
import           System.Directory ( doesDirectoryExist, doesFileExist, getDirectoryContents )
import           System.Environment ( lookupEnv )

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

showAttachmentList :: Lens' ChatState (MessageInterface Name i) -> MH ()
showAttachmentList which = do
    lst <- use (which.miEditor.esAttachmentList)
    case length (L.listElements lst) of
        0 -> showAttachmentFileBrowser which
        _ -> which.miMode .= ManageAttachments

showAttachmentFileBrowser :: Lens' ChatState (MessageInterface Name i) -> MH ()
showAttachmentFileBrowser which = do
    cId <- use (which.miEditor.esChannelId)
    config <- use (csResources.crConfiguration)
    filePath <- liftIO $ defaultAttachmentsPath config
    browser <- liftIO $ Just <$> FB.newFileBrowser FB.selectNonDirectories (AttachmentFileBrowser cId) filePath
    which.miEditor.esFileBrowser .= browser
    which.miMode .= BrowseFiles

getHomeDir :: IO (Maybe String)
getHomeDir = lookupEnv "HOME"

replaceHome :: Text -> IO Text
replaceHome t = do
    home <- getHomeDir

    let maybeReplace = do
            h <- home
            return $ replace "~" (pack h) t

    return $ fromMaybe t maybeReplace

attachFileByPath :: Lens' ChatState (MessageInterface Name i) -> Text -> MH ()
attachFileByPath which txtPath = do
    strPath <- unpack <$> (liftIO $ replaceHome txtPath)

    fileInfo <- liftIO $ FB.getFileInfo strPath strPath
    case FB.fileInfoFileStatus fileInfo of
        Left e -> do
            mhError $ AttachmentException (toException e)
        Right _ -> tryAddAttachment which [fileInfo]

checkPathIsFile :: FB.FileInfo -> MH Bool
checkPathIsFile = liftIO . doesFileExist . FB.fileInfoFilePath

tryAddAttachment :: Lens' ChatState (MessageInterface Name i) -> [FB.FileInfo] -> MH ()
tryAddAttachment which entries = do
    forM_ entries $ \entry -> do
        isFile <- checkPathIsFile entry
        if not isFile
        then mhError (BadAttachmentPath
            "Error attaching file. It either doesn't exist or is a directory, which is not supported.")
        else do
            -- Is the entry already present? If so, ignore the selection.
            es <- use (which.miEditor.esAttachmentList.L.listElementsL)
            let matches = (== FB.fileInfoFilePath entry) .
                              FB.fileInfoFilePath .
                              attachmentDataFileInfo
            case Vector.find matches es of
                Just _ -> return ()
                Nothing -> do
                    tryReadAttachment entry >>= \case
                        Right a -> do
                            oldIdx <- use (which.miEditor.esAttachmentList.L.listSelectedL)
                            let newIdx = if Vector.null es
                                         then Just 0
                                         else oldIdx
                            which.miEditor.esAttachmentList %= L.listReplace (Vector.snoc es a) newIdx
                        Left e -> mhError $ AttachmentException e

    when (not $ null entries) $
        which.miMode .= Compose

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
