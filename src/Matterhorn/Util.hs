module Matterhorn.Util
  ( nubOn
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import qualified Data.Set as Set

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
