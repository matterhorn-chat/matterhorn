module Matterhorn.Util
  ( nubOn
  , trimToOneLine
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import qualified Data.Set as Set
import qualified Data.Text as Text

-- | The 'nubOn' function removes duplicate elements from a list. In
-- particular, it keeps only the /last/ occurrence of each
-- element. The equality of two elements in a call to @nub f@ is
-- determined using @f x == f y@, and the resulting elements must have
-- an 'Ord' instance in order to make this function more efficient.
nubOn :: (Ord b) => (a -> b) -> [a] -> [a]
nubOn f = snd . go Set.empty
  where go before [] = (before, [])
        go before (x:xs) =
          let (before', xs') = go before xs
              key = f x in
          if key `Set.member` before'
            then (before', xs')
            else (Set.insert key before', x : xs')


-- | In some instances, such as when rendering the channel topics in the
-- autocomplete window, we just want to show the first line as a preview.
trimToOneLine :: Text -> Text
trimToOneLine = Text.takeWhile (/= '\n')

