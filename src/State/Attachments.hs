module State.Attachments
  ( showAttachmentList
  , resetAttachmentList
  , showAttachmentFileBrowser
  )
where

import           Prelude ()
import           Prelude.MH

import           Brick ( vScrollToBeginning, viewportScroll )
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.FileBrowser as FB
import           Lens.Micro.Platform ( (.=) )

import           Types


showAttachmentList :: MH ()
showAttachmentList = do
    lst <- use (csEditState.cedAttachmentList)
    case length (L.listElements lst) of
        0 -> showAttachmentFileBrowser
        _ -> setMode ManageAttachments

resetAttachmentList :: MH ()
resetAttachmentList = do
    csEditState.cedAttachmentList .= L.list AttachmentList mempty 1
    mh $ vScrollToBeginning $ viewportScroll AttachmentList

showAttachmentFileBrowser :: MH ()
showAttachmentFileBrowser = do
    browser <- liftIO $ FB.newFileBrowser FB.selectNonDirectories AttachmentFileBrowser Nothing
    csEditState.cedFileBrowser .= browser
    setMode ManageAttachmentsBrowseFiles
