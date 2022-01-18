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

import           Network.Mattermost.Types ( TeamId )

import           Matterhorn.Types
import           Matterhorn.Draw.Buttons
import           Matterhorn.Themes


drawSaveAttachmentWindow :: ChatState -> TeamId -> Widget Name
drawSaveAttachmentWindow st tId =
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
        editorHeight = 1
        maxWindowWidth = 50
        foc = st^.csTeam(tId).tsSaveAttachmentDialog.attachmentPathDialogFocus
        ed = st^.csTeam(tId).tsSaveAttachmentDialog.attachmentPathEditor
        drawEditorTxt = txt . T.unlines
