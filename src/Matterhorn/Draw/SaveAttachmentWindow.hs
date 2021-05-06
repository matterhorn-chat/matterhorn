module Matterhorn.Draw.SaveAttachmentWindow
  ( drawSaveAttachmentWindow
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick
import           Brick.Focus
import           Brick.Widgets.Border
import           Brick.Widgets.Center
import           Brick.Widgets.Edit

import qualified Data.Text as T

import           Matterhorn.Types
import           Matterhorn.Draw.Buttons
import           Matterhorn.Themes


drawSaveAttachmentWindow :: ChatState -> Widget Name
drawSaveAttachmentWindow st =
    centerLayer $
    hLimit maxWindowWidth $
    joinBorders $
    borderWithLabel (withDefAttr clientEmphAttr $ txt "Save Attachment") $
    vBox [ padAll 1 $
           txt "Path: " <+>
           (vLimit editorHeight $
            withFocusRing foc (renderEditor drawEditorTxt) ed)
         , hBox [ padRight Max $
                  padLeft (Pad 1) $
                  drawButton foc (AttachmentPathSaveButton tId) "Save"
                , padRight (Pad 1) $
                  drawButton foc (AttachmentPathCancelButton tId) "Cancel"
                ]
         ]
    where
        tId = st^.csCurrentTeamId
        editorHeight = 1
        maxWindowWidth = 50
        foc = st^.csCurrentTeam.tsSaveAttachmentDialog.attachmentPathDialogFocus
        ed = st^.csCurrentTeam.tsSaveAttachmentDialog.attachmentPathEditor
        drawEditorTxt = txt . T.unlines
