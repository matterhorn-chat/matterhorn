-- Heavily inspired by tab completion from glirc:
-- https://github.com/glguy/irc-core/blob/v2/src/Client/Commands/WordCompletion.hs
module Completion
  ( Completer(..)
  , CompletionAlternative(..)
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
-- from a sequence of alternatives.
data Completer =
    Completer { completionAlternatives :: Z.Zipper () CompletionAlternative
              }

-- A completion alternative is made up of various representations:
-- the string corresonding to the user input, the string that will
-- ultimately replace the user's input, and the the string that we will
-- display in the alternative list. The representations are decoupled
-- specifically to deal with permitting nickname completions to
-- "resolve" to usernames and to permit completions in context-sensitive
-- settings where the completion replacement and display differ greatly
-- from the input.
data CompletionAlternative =
    CompletionAlternative { completionInput :: Text
                          , completionReplacement :: Text
                          , completionDisplay :: Text
                          }
                          deriving (Ord, Eq)

matchesAlternative :: Text -> CompletionAlternative -> Bool
matchesAlternative input alt =
    T.toLower input `T.isPrefixOf` (T.toLower $ completionInput alt)

-- Nothing: no completions.
-- Just Left: a single completion.
-- Just Right: more than one completion.
wordComplete :: Set CompletionAlternative -> Text -> Maybe (Either Text Completer)
wordComplete options input =
    let curWord = currentWord input
        alts = sort $ Set.toList $ Set.filter (matchesAlternative curWord) options
    in if null alts || T.null curWord
       then Nothing
       else if length alts == 1
            then Just $ Left $ completionReplacement $ head alts
            else Just $ Right $ Completer { completionAlternatives = Z.fromList [((), alts)]
                                          }

currentAlternative :: Completer -> CompletionAlternative
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
