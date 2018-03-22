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
import           Prelude.Compat

import           Data.Char ( isSpace )
import           Data.List ( sort )
import qualified Data.Set as Set
import qualified Data.Text as T

import qualified Zipper as Z

data Completer =
    Completer { completionAlternatives :: Z.Zipper (T.Text, T.Text)
              }

-- Nothing: no completions.
-- Just Left: a single completion.
-- Just Right: more than one completion.
wordComplete :: Set.Set (T.Text, T.Text) -> T.Text -> Maybe (Either T.Text Completer)
wordComplete options input =
    let curWord = currentWord input
        alts = sort $ Set.toList $ Set.filter ((curWord `T.isPrefixOf`) . fst) options
    in if null alts || T.null curWord
       then Nothing
       else if length alts == 1
            then Just $ Left $ snd $ head alts
            else Just $ Right $ Completer { completionAlternatives = Z.fromList alts
                                          }

currentAlternative :: Completer -> (T.Text, T.Text)
currentAlternative = Z.focus . completionAlternatives

nextCompletion :: Completer -> Completer
nextCompletion (Completer z) = Completer $ Z.right z

previousCompletion :: Completer -> Completer
previousCompletion (Completer z) = Completer $ Z.left z

-- | trim whitespace and do any other edits we need
-- to focus on the current word
currentWord :: T.Text -> T.Text
currentWord line
  = T.reverse
  $ T.takeWhile (not . isSpace)
  $ T.dropWhile (\x -> x==' ' || x==':')
  $ T.reverse
  $ line
