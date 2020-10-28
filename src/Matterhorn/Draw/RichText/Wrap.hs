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


type WrappedLine = Seq FlattenedValue

data WrapState =
    WrapState { wrapCompletedLines :: Seq WrappedLine
              , wrapCurLine :: WrappedLine
              , wrapCurCol :: Int
              , wrapWidth :: Int
              }

type WrapM a = State WrapState a

pushValue :: FlattenedValue -> WrapM ()
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

pushLine :: WrapM ()
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

doLineWrapping :: Int -> Seq FlattenedValue -> Seq WrappedLine
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
fvWidth :: FlattenedValue -> Int
fvWidth (SingleInline fi) = fiWidth fi
fvWidth (NonBreaking rs) = sum $ (sum . fmap fvWidth) <$> rs

-- The widths returned by this function must match the content widths
-- rendered by renderFlattenedValue.
fiWidth :: FlattenedInline -> Int
fiWidth fi =
    case fiValue fi of
        FText t                      -> B.textWidth t
        FSpace                       -> 1
        FUser t                      -> T.length userSigil + B.textWidth t
        FChannel t                   -> T.length normalChannelSigil + B.textWidth t
        FEmoji t                     -> B.textWidth t + 2
        FEditSentinel _              -> B.textWidth editMarking
