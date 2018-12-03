module Draw.ManageAttachments
  ( drawManageAttachments
  )
where

import           Prelude ()
import           Prelude.MH

import           Brick
import           Brick.Widgets.List
import           Brick.Widgets.Center
import           Brick.Widgets.Border
import           Lens.Micro.Platform ( (^.) )

import           Types
import           Draw.Main ( drawMain )


drawManageAttachments :: ChatState -> [Widget Name]
drawManageAttachments st =
    drawAttachmentList st : drawMain st

drawAttachmentList :: ChatState -> Widget Name
drawAttachmentList st =
    centerLayer $
    hLimit 60 $
    vLimit 10 $
    borderWithLabel (txt "Attachments") $
    renderList renderAttachmentItem True (st^.csEditState.cedAttachmentList)

renderAttachmentItem :: Bool -> AttachmentData -> Widget Name
renderAttachmentItem _ d =
    padRight Max $ str $ attachmentDataFilename d
