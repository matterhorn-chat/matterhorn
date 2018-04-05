-- Heavily inspired by tab completion from glirc:
-- https://github.com/glguy/irc-core/blob/v2/src/Client/Commands/WordCompletion.hs
module Completion
  ( Completer(..)
  , wordComplete
  , currentAlternative
  , nextCompletion
  , previousCompletion
  )
where

import           Prelude ()
import           Prelude.MH

import           Data.Char ( isSpace )
import qualified Data.Set as Set
import qualified Data.Text as T

import qualified Zipper as Z

-- A completer stores the stateful selection of a completion alternative
-- from a sequence of alternatives. Each item in the sequence is a pair:
-- the first element of the pair is the string corresonding to the user
-- input and the second element of the pair is what will ultimately
-- replace the user's input. The two are decoupled specifically to deal
-- with permitting nickname completions to "resolve" to usernames.
data Completer =
    Completer { completionAlternatives :: Z.Zipper (Text, Text)
              }

-- Nothing: no completions.
-- Just Left: a single completion.
-- Just Right: more than one completion.
wordComplete :: Set.Set (Text, Text) -> Text -> Maybe (Either Text Completer)
wordComplete options input =
    let curWord = currentWord input
        alts = sort $ Set.toList $ Set.filter ((curWord `T.isPrefixOf`) . fst) options
    in if null alts || T.null curWord
       then Nothing
       else if length alts == 1
            then Just $ Left $ snd $ head alts
            else Just $ Right $ Completer { completionAlternatives = Z.fromList alts
                                          }

currentAlternative :: Completer -> (Text, Text)
currentAlternative = Z.focus . completionAlternatives

nextCompletion :: Completer -> Completer
nextCompletion (Completer z) = Completer $ Z.right z

previousCompletion :: Completer -> Completer
previousCompletion (Completer z) = Completer $ Z.left z

-- | trim whitespace and do any other edits we need
-- to focus on the current word
currentWord :: Text -> Text
currentWord line
  = T.reverse
  $ T.takeWhile (not . isSpace)
  $ T.dropWhile (\x -> x==' ' || x==':')
  $ T.reverse
  $ line
