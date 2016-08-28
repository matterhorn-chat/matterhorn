-- Heavily inspired by tab completion from glirc:
-- https://github.com/glguy/irc-core/blob/v2/src/Client/Commands/WordCompletion.hs
module Completion where

import           Control.Applicative ( (<|>) )
import           Control.Monad ( guard )
import           Data.List ( find
                           , isPrefixOf )
import qualified Data.Set as Set
import           Data.Set ( Set )

data Direction = Forwards | Backwards
  deriving (Read, Show, Eq, Ord)

search :: Direction
       -> String       -- ^ prefix
       -> String       -- ^ current match
       -> Set String   -- ^ potential completions
       -> Maybe String
search direction prefix current options
  | Just next <- advanceFun direction current options
  , prefix `isPrefixOf` next
  = Just next

  | otherwise = case direction of
    Backwards -> find (prefix `isPrefixOf`)
                      (Set.toDescList options)
    Forwards  -> do x <- Set.lookupGE prefix options
                    guard (prefix `isPrefixOf` x)
                    Just x
  where
  advanceFun Forwards  = Set.lookupGT
  advanceFun Backwards = Set.lookupLT

wordComplete :: Direction
             -> [String]     -- ^ priority completions
             -> Set String   -- ^ potential completions
             -> String       -- ^ current prompt
             -> Maybe String -- ^ previous search
             -> Maybe String -- ^ completion
wordComplete direction hints options prompt previous = do
  let current = currentWord prompt
  guard (not (null current))
  case previous of
    Just pattern | pattern `isPrefixOf` current ->
      search direction pattern current options

    _ -> find (current `isPrefixOf`) hints <|>
         search direction current current options

-- | trim whitespace and do any other edits we need
-- to focus on the current word
currentWord :: String -> String
currentWord = id -- XXX: fixme

