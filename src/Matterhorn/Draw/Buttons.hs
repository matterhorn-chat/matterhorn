module Matterhorn.Draw.Buttons
  ( drawButton
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick
import           Brick.Focus
import           Brick.Widgets.Center

import qualified Data.Text as T

import           Matterhorn.Themes


buttonWidth :: Int
buttonWidth = 10

drawButton :: (Eq n, Ord n) => FocusRing n -> n -> T.Text -> Widget n
drawButton f n label =
    let attr = if focusGetCurrent f == Just n
               then buttonFocusedAttr
               else buttonAttr
    in withDefAttr attr $
       clickable n $
       hLimit buttonWidth $
       hCenter $
       txt label
