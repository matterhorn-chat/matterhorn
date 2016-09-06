{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ParallelListComp #-}

module Markdown (renderMessage) where

import           Brick ( (<=>), (<+>), Widget )
import qualified Brick as B
import           Cheapskate.Types ( Block
                                  , Blocks
                                  , Inlines
                                  , ListType
                                  )
import qualified Cheapskate as C
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Sequence ( Seq
                               , ViewL(..)
                               , ViewR(..)
                               , (><)
                               , (<|)
                               , (|>)
                               , viewl
                               , viewr)
import qualified Data.Sequence as S
import           Lens.Micro.Platform ((^.))
import           Text.Regex.TDFA.String (Regex)

import           Highlighting
import           Themes
import           Types (MessageType(..), PostType(..))

renderMessage :: Blocks -> Maybe String -> MessageType -> Regex -> Widget a
renderMessage bs u mTy uPat =
  case u of
    Just un
      | mTy == CP Emote -> B.str "*" <+> colorUsername un
                       <+> B.str " " <+> vBox (fmap (toWidget uPat) bs)
      | otherwise -> colorUsername un <+> B.str ": " <+> vBox (fmap (toWidget uPat) bs)
    Nothing -> vBox (fmap (toWidget uPat) bs)

vBox :: Foldable f => f (Widget a) -> Widget a
vBox = foldr (<=>) B.emptyWidget

hBox :: Foldable f => f (Widget a) -> Widget a
hBox = foldr (<+>) B.emptyWidget

--

class ToWidget t where
  toWidget :: Regex -> t -> Widget a

header :: Int -> Widget a
header n = B.str $ (take n $ repeat '#')

instance ToWidget Block where
  toWidget uPat (C.Para is) = toInlineChunk is uPat
  toWidget uPat (C.Header n is) =
    B.withDefAttr clientHeaderAttr
      (header n <+> B.str " " <+> toInlineChunk is uPat)
  toWidget uPat (C.Blockquote is) =
    B.padLeft (B.Pad 4) (vBox $ fmap (toWidget uPat) is)
  toWidget uPat (C.List _ l bs) = toList l bs uPat
  toWidget _ (C.CodeBlock _ tx) =
    B.withDefAttr markdownAttr $
      B.vBox [ B.str " | " <+> B.txt ln | ln <- T.lines tx ]
  toWidget _ (C.HtmlBlock txt) = B.txt txt
  toWidget _ (C.HRule) = B.vLimit 1 (B.fill '*')

toInlineChunk :: Inlines -> Regex -> Widget a
toInlineChunk is uPat = B.Widget B.Fixed B.Fixed $ do
  ctx <- B.getContext
  let width = ctx^.B.availWidthL
      fs    = toFragments is
      ws    = fmap (fmap (toWidget uPat)) (split width fs)
  B.render (vBox (fmap hBox ws))

toList :: ListType -> [Blocks] -> Regex -> Widget a
toList lt bs uPat = vBox
  [ B.str i <+> (vBox (fmap (toWidget uPat) b))
  | b <- bs | i <- is ]
  where is = case lt of
          C.Bullet c -> repeat (c:" ")
          C.Numbered _ _ -> [ show (n :: Int) ++ ". "
                            | n <- [1..] ]

-- We want to do word-wrapping, but for that we want a linear
-- sequence of chunks we can break up. The typical Markdown
-- format doesn't fit the bill: when it comes to bold or italic
-- bits, we'd have treat it all as one. This representation is
-- more amenable to splitting up those bits.
data Fragment = Fragment
  { fTextual :: TextFragment
  , fStyle   :: FragmentStyle
  } deriving (Show)

data TextFragment
  = TStr Text
  | TSpace
  | TSoftBreak
  | TLineBreak
  | TCode Text
  | TLink Text
  | TRawHtml Text
    deriving (Show, Eq)

data FragmentStyle
  = Normal
  | Emph
  | Strong
    deriving (Show)

-- We convert it pretty mechanically:
toFragments :: Inlines -> Seq Fragment
toFragments = go Normal
  where go n (viewl-> C.Str t :< xs) =
          Fragment (TStr t) n <| go n xs
        go n (viewl-> C.Space :< xs) =
          Fragment TSpace n <| go n xs
        go n (viewl-> C.SoftBreak :< xs) =
          Fragment TSoftBreak n <| go n xs
        go n (viewl-> C.LineBreak :< xs) =
          Fragment TLineBreak n <| go n xs
        go n (viewl-> C.Link _ t _ :< xs) =
          Fragment (TLink t) n <| go n xs
        go n (viewl-> C.RawHtml t :< xs) =
          Fragment (TRawHtml t) n <| go n xs
        go n (viewl-> C.Code t :< xs) =
          Fragment (TCode t) n <| go n xs
        go n (viewl-> C.Emph is :< xs) =
          go Emph is >< go n xs
        go n (viewl-> C.Strong is :< xs) =
          go Strong is >< go n xs
        go _ _ = S.empty

--
data SplitState = SplitState
  { splitChunks  :: Seq (Seq Fragment)
  , splitCurrCol :: Int
  }

split :: Int -> Seq Fragment -> Seq (Seq Fragment)
split maxCols = splitChunks . go (SplitState (S.singleton S.empty) 0)
  where go st (viewl-> f :< fs) = go st' fs
          where st' =
                  if available >= fsize && fTextual f /= TLineBreak
                                        && fTextual f /= TSoftBreak
                    then st { splitChunks  = addFragment f (splitChunks st)
                            , splitCurrCol = splitCurrCol st + fsize
                            }
                    else st { splitChunks  = splitChunks st |> S.singleton f
                            , splitCurrCol = fsize
                            }
                available = maxCols - splitCurrCol st
                fsize = fragmentSize f
                addFragment :: Fragment -> Seq (Seq Fragment) -> Seq (Seq Fragment)
                addFragment x (viewr-> ls :> l) = ( ls |> (l |> x))
                addFragment _ _ = error "[unreachable]"
        go st _                 = st

fragmentSize :: Fragment -> Int
fragmentSize f = case fTextual f of
  TStr t     -> T.length t
  TCode t    -> T.length t
  TLink t    -> T.length t
  TRawHtml t -> T.length t
  TLineBreak -> 0
  _          -> 1

instance ToWidget Fragment where
  toWidget uPat fragment =
    style $ case fTextual fragment of
      TStr t       -> doMessageMarkup uPat t
      TSpace       -> B.str " "
      TSoftBreak   -> B.emptyWidget
      TLineBreak   -> B.emptyWidget
      TCode txt    -> B.withDefAttr markdownAttr (B.txt txt)
      TLink txt    -> B.txt txt
      TRawHtml txt -> B.txt txt
    where style = case fStyle fragment of
            Normal -> id
            Emph   -> B.withDefAttr clientEmphAttr
            Strong -> B.withDefAttr clientStrongAttr
