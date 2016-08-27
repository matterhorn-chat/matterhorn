module DrawUtil
  ( wrappedText
  ) where

import Brick
import Lens.Micro.Platform
import Text.LineBreak (breakString, BreakFormat(..))

wrappedText :: String -> Widget a
wrappedText msg = Widget Fixed Fixed $ do
  ctx <- getContext
  let w = ctx ^. availWidthL
  render (str (breakString (BreakFormat w 8 '-' Nothing) msg))
