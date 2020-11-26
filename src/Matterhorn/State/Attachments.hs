module Matterhorn.State.Attachments
  ( showAttachmentList
  , resetAttachmentList
  , showAttachmentFileBrowser
  )
where

import           Prelude ()
import           Matterhorn.Prelude
import qualified Control.Exception as E
import           Data.Either ( isRight )
import           System.Directory ( doesDirectoryExist, getDirectoryContents )
import           Data.Bool ( bool )

import           Brick ( vScrollToBeginning, viewportScroll )
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.FileBrowser as FB
import           Lens.Micro.Platform ( (.=) )

import           Network.Mattermost.Types ( teamId )

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

showAttachmentList :: MH ()
showAttachmentList = do
    lst <- use (csCurrentTeam.tsEditState.cedAttachmentList)
    case length (L.listElements lst) of
        0 -> showAttachmentFileBrowser
        _ -> setMode ManageAttachments

resetAttachmentList :: MH ()
resetAttachmentList = do
    tId <- teamId <$> use (csCurrentTeam.tsTeam)
    let listName = AttachmentList tId
    csCurrentTeam.tsEditState.cedAttachmentList .= L.list listName mempty 1
    mh $ vScrollToBeginning $ viewportScroll listName

showAttachmentFileBrowser :: MH ()
showAttachmentFileBrowser = do
    config <- use (csResources.crConfiguration)
    tId <- teamId <$> use (csCurrentTeam.tsTeam)
    filePath <- liftIO $ defaultAttachmentsPath config
    browser <- liftIO $ Just <$> FB.newFileBrowser FB.selectNonDirectories (AttachmentFileBrowser tId) filePath
    csCurrentTeam.tsEditState.cedFileBrowser .= browser
    setMode ManageAttachmentsBrowseFiles
