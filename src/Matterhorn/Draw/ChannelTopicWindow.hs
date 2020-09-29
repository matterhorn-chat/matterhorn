module Matterhorn.Draw.ChannelTopicWindow
  ( drawChannelTopicWindow
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick
import           Brick.Focus
import           Brick.Widgets.Border
import           Brick.Widgets.Center
import           Brick.Widgets.Edit

import           Control.Arrow ( (>>>) )
import qualified Data.Text as T
import           Data.Text.Zipper ( insertChar, gotoEOL )

import           Matterhorn.Types
import           Matterhorn.Draw.Buttons
import           Matterhorn.Draw.RichText
import           Matterhorn.Themes


drawChannelTopicWindow :: ChatState -> Widget Name
drawChannelTopicWindow st =
    centerLayer $
    hLimit maxWindowWidth $
    joinBorders $
    borderWithLabel (withDefAttr clientEmphAttr $ txt "Edit Channel Topic") $
    vBox [ vLimit editorHeight $
           withFocusRing foc (renderEditor drawTopicEditorTxt) ed
         , hBorderWithLabel (withDefAttr clientEmphAttr $ txt "Preview")
         , vLimit previewHeight $
           viewport ChannelTopicEditorPreview Vertical $
           renderText' "" hSet topicTxtWithCursor
         , hBorder
         , hBox [ padRight Max $
                  padLeft (Pad 1) $
                  drawButton foc ChannelTopicSaveButton "Save"
                , padRight (Pad 1) $
                  drawButton foc ChannelTopicCancelButton "Cancel"
                ]
         ]
    where
        editorHeight = 5
        previewHeight = 5
        maxWindowWidth = 70
        foc = st^.csChannelTopicDialog.channelTopicDialogFocus
        ed = st^.csChannelTopicDialog.channelTopicDialogEditor
        hSet = getHighlightSet st
        topicTxtWithCursor = T.unlines $
                             getEditContents $
                             applyEdit (gotoEOL >>> insertChar cursorSentinel) ed
        drawTopicEditorTxt = txt . T.unlines
