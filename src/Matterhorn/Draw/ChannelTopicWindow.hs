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

import           Network.Mattermost.Types ( TeamId )

import           Matterhorn.Types
import           Matterhorn.Draw.Buttons
import           Matterhorn.Draw.RichText
import           Matterhorn.Themes


drawChannelTopicWindow :: ChatState -> TeamId -> Widget Name
drawChannelTopicWindow st tId =
    centerLayer $
    hLimit maxWindowWidth $
    joinBorders $
    borderWithLabel (withDefAttr clientEmphAttr $ txt "Edit Channel Topic") $
    vBox [ vLimit editorHeight $
           withFocusRing foc (renderEditor drawTopicEditorTxt) ed
         , hBorderWithLabel (withDefAttr clientEmphAttr $ txt "Preview")
         , vLimit previewHeight $
           viewport (ChannelTopicEditorPreview tId) Vertical $
           renderText' (Just baseUrl) "" hSet Nothing topicTxtWithCursor
         , hBorder
         , hCenter $
           renderText' Nothing "" hSet Nothing
           "Press `Tab` to change focus to buttons."
         , hBox [ padRight Max $
                  padLeft (Pad 1) $
                  drawButton foc (ChannelTopicSaveButton tId) "Save"
                , padRight (Pad 1) $
                  drawButton foc (ChannelTopicCancelButton tId) "Cancel"
                ]
         ]
    where
        baseUrl = serverBaseUrl st tId
        editorHeight = 5
        previewHeight = 5
        maxWindowWidth = 70
        foc = st^.csTeam(tId).tsChannelTopicDialog.channelTopicDialogFocus
        ed = st^.csTeam(tId).tsChannelTopicDialog.channelTopicDialogEditor
        hSet = getHighlightSet st tId
        topicTxtWithCursor = T.unlines $
                             getEditContents $
                             applyEdit (gotoEOL >>> insertChar cursorSentinel) ed
        drawTopicEditorTxt = txt . T.unlines
