module Draw.Util
  ( wrappedText
  ) where

import Brick
import Lens.Micro.Platform
import Text.LineBreak (breakString, BreakFormat(..))
import qualified Data.Text as T

wrappedText :: (T.Text -> Widget a) -> T.Text -> Widget a
wrappedText mkWidget msg = Widget Fixed Fixed $ do
  ctx <- getContext
  let w = ctx ^. availWidthL
      s = breakString (BreakFormat w 8 '-' Nothing) (T.unpack trimmed)
      -- Some messages end in '\n' or '\8203' but we don't want those to
      -- end up affecting rendering.
      bad :: String
      bad = "\n\8203"
      trimmed = case T.last msg `elem` bad of
          True -> T.init msg
          False -> msg
  render (mkWidget $ T.pack s)
