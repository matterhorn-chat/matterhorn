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
      s = breakString (BreakFormat w 8 '-' Nothing) trimmed
      -- Some messages end in '\n' or '\8203' but we don't want those to
      -- end up affecting rendering.
      bad :: String
      bad = "\n\8203"
      trimmed = case last msg `elem` bad of
          True -> init msg
          False -> msg
  render (str s)
