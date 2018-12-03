module State.Attachments
  ( showAttachmentList
  , resetAttachmentList
  )
where

import           Prelude ()
import           Prelude.MH

import           Brick ( vScrollToBeginning, viewportScroll )
import qualified Brick.Widgets.List as L
import           Lens.Micro.Platform ( (.=) )

import           Types


showAttachmentList :: MH ()
showAttachmentList = setMode ManageAttachments

resetAttachmentList :: MH ()
resetAttachmentList = do
  csEditState.cedAttachmentList .= L.list AttachmentList mempty 1
  mh $ vScrollToBeginning $ viewportScroll AttachmentList
