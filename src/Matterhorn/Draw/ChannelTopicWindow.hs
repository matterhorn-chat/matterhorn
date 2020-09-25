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
import           Matterhorn.Draw.RichText
import           Matterhorn.Themes


drawChannelTopicWindow :: ChatState -> Widget Name
drawChannelTopicWindow st =
    centerLayer $
    hLimit 70 $
    joinBorders $
    borderWithLabel (withDefAttr clientEmphAttr $ txt "Edit Channel Topic") $
    vBox [ vLimit 5 $
           withFocusRing foc (renderEditor (txt . T.unlines))
                             (st^.csChannelTopicDialog.channelTopicDialogEditor)
         , hBorderWithLabel (withDefAttr clientEmphAttr $ txt "Preview")
         , vLimit 5 $
           viewport ChannelTopicEditorPreview Vertical $
           renderText' ""
                       (getHighlightSet st)
                       (T.unlines $
                        getEditContents $
                        (applyEdit (gotoEOL >>> insertChar cursorSentinel)
                         (st^.csChannelTopicDialog.channelTopicDialogEditor)))
         , hBorder
         , hBox [ padRight Max $
                  padLeft (Pad 1) $
                  drawButton foc ChannelTopicSaveButton "Save"
                , padRight (Pad 1) $
                  drawButton foc ChannelTopicCancelButton "Cancel"
                ]
         ]
    where
        foc = st^.csChannelTopicDialog.channelTopicDialogFocus

buttonWidth :: Int
buttonWidth = 10

drawButton :: (Eq n) => FocusRing n -> n -> T.Text -> Widget n
drawButton f n label =
    let attr = if focusGetCurrent f == Just n
               then buttonFocusedAttr
               else buttonAttr
    in withDefAttr attr $
       hLimit buttonWidth $
       hCenter $
       txt label
