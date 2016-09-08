-- Heavily inspired by tab completion from glirc:
-- https://github.com/glguy/irc-core/blob/v2/src/Client/Commands/WordCompletion.hs
module Completion where

import           Control.Applicative ( (<|>) )
import           Control.Monad ( guard )
import           Data.Char ( isSpace )
import           Data.List ( find )
import qualified Data.Set as Set
import           Data.Set ( Set )
import qualified Data.Text as T

data Direction = Forwards | Backwards
  deriving (Read, Show, Eq, Ord)

search :: Direction
       -> T.Text       -- ^ prefix
       -> T.Text       -- ^ current match
       -> Set T.Text   -- ^ potential completions
       -> Maybe T.Text
search direction prefix current options
  | Just next <- advanceFun direction current options
  , prefix `T.isPrefixOf` next
  = Just next

  | otherwise = case direction of
    Backwards -> find (prefix `T.isPrefixOf`)
                      (Set.toDescList options)
    Forwards  -> do x <- Set.lookupGE prefix options
                    guard (prefix `T.isPrefixOf` x)
                    Just x
  where
  advanceFun Forwards  = Set.lookupGT
  advanceFun Backwards = Set.lookupLT

wordComplete :: Direction
             -> [T.Text]     -- ^ priority completions
             -> Set T.Text   -- ^ potential completions
             -> T.Text       -- ^ current prompt
             -> Maybe T.Text -- ^ previous search
             -> Maybe T.Text -- ^ completion
wordComplete direction hints options prompt previous = do
  let current = currentWord prompt
  guard (not (T.null current))
  case previous of
    Just pattern | pattern `T.isPrefixOf` current ->
      search direction pattern current options

    _ -> find (current `T.isPrefixOf`) hints <|>
         search direction current current options

-- | trim whitespace and do any other edits we need
-- to focus on the current word
currentWord :: T.Text -> T.Text
currentWord line
  = T.reverse
  $ T.takeWhile (not . isSpace)
  $ T.dropWhile (\x -> x==' ' || x==':')
  $ T.reverse
  $ line
