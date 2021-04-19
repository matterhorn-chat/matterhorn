-- | This module performs line-wrapping of sequences of flattend inline
-- values produced by 'flattenInlineSeq'.
--
-- This process works by maintaining a 'WrapState' in the 'WrapM'
-- monad, where inline values are pushed onto the current line, and
-- line breaks are introduced as inlines exceed the available width.
-- The most important caveat of this module is that wrapping depends
-- on knowing the width of each 'FlattenedValue', which is provided
-- by the 'fvWidth' function. But 'fvWidth' must return values that
-- are consistent with the how the inlines actually get rendered by
-- 'renderFlattenedValue'. This is because there are visual aspects to
-- how some inlines get rendered that are implicit, such as user or
-- channel sigils that get added at drawing time, that have an impact on
-- their visible width.
module Matterhorn.Draw.RichText.Wrap
  ( WrappedLine
  , doLineWrapping
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import qualified Brick as B
import           Control.Monad.State
import qualified Data.Sequence as Seq
import           Data.Sequence ( ViewL(..)
                               , (|>)
                               )
import qualified Data.Text as T

import           Matterhorn.Constants ( normalChannelSigil, userSigil )
import           Matterhorn.Draw.RichText.Flatten
import           Matterhorn.Constants ( editMarking )


type WrappedLine a = Seq (FlattenedValue a)

data WrapState a =
    WrapState { wrapCompletedLines :: Seq (WrappedLine a)
              -- ^ The completed lines so far
              , wrapCurLine :: (WrappedLine a)
              -- ^ The current line we are accumulating
              , wrapCurCol :: Int
              -- ^ The width of wrapCurLine, in columns
              , wrapWidth :: Int
              -- ^ The maximum allowable width
              }

type WrapM a b = State (WrapState b) a

-- | Push a flattened value onto the current line if possible, or add a
-- line break and add the inline value to a new line if it would cause
-- the current line width to exceed the maximum.
pushValue :: FlattenedValue a -> WrapM () a
pushValue i = do
    let iw = fvWidth i
        pushThisInline =
            modify $ \st -> st { wrapCurLine = wrapCurLine st |> i
                               , wrapCurCol = wrapCurCol st + iw
                               }
    maxWidth <- gets wrapWidth
    curWidth <- gets wrapCurCol
    let remaining = maxWidth - curWidth

    when (iw > remaining) pushLine

    pushThisInline

-- | Insert a new line break by moving the current accumulating line
-- onto the completed lines list and resetting it to empty.
pushLine :: WrapM () a
pushLine = do
    let trimLeadingWhitespace s =
            case Seq.viewl s of
                SingleInline i :< t | fiValue i == FSpace -> trimLeadingWhitespace t
                _ -> s

    modify $ \st ->
        st { wrapCurLine = mempty
           , wrapCompletedLines = wrapCompletedLines st |> trimLeadingWhitespace (wrapCurLine st)
           , wrapCurCol = 0
           }

-- | Given a maximum width and an inline sequence, produce a sequence of
-- lines wrapped at the specified column. This only returns lines longer
-- than the maximum width when those lines have a single inline value
-- that cannot be broken down further (such as a long URL).
doLineWrapping :: Int -> Seq (FlattenedValue a) -> Seq (WrappedLine a)
doLineWrapping maxCols i =
    result
    where
        result = wrapCompletedLines $ execState (mapM_ pushValue i >> pushLine) initialState
        initialState = WrapState { wrapCurLine = mempty
                                 , wrapCompletedLines = mempty
                                 , wrapCurCol = 0
                                 , wrapWidth = maxCols
                                 }

-- The widths returned by this function must match the content widths
-- rendered by renderFlattenedValue.
fvWidth :: FlattenedValue a -> Int
fvWidth (SingleInline fi) = fiWidth fi
fvWidth (NonBreaking rs) = sum $ (sum . fmap fvWidth) <$> rs

-- The widths returned by this function must match the content widths
-- rendered by renderFlattenedValue.
fiWidth :: FlattenedInline a -> Int
fiWidth fi =
    case fiValue fi of
        FText t                      -> B.textWidth t
        FSpace                       -> 1
        FUser t                      -> T.length userSigil + B.textWidth t
        FChannel t                   -> T.length normalChannelSigil + B.textWidth t
        FEmoji t                     -> B.textWidth t + 2
        FEditSentinel _              -> B.textWidth editMarking
