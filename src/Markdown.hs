{-# LANGUAGE ViewPatterns #-}

module Markdown where

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

import           Themes

renderMessage :: String -> Widget a
renderMessage s = vBox (fmap toWidget bs)
  where C.Doc _ bs = C.markdown C.def (T.pack s)

vBox :: Foldable f => f (Widget a) -> Widget a
vBox = foldr (<=>) B.emptyWidget

hBox :: Foldable f => f (Widget a) -> Widget a
hBox = foldr (<+>) B.emptyWidget

class ToWidget t where
  toWidget :: t -> Widget a

instance ToWidget Block where
  toWidget (C.Para is) = toInlineChunk is
  toWidget (C.Header _ is) =
    B.withDefAttr clientHeaderAttr (toInlineChunk is)
  toWidget (C.Blockquote is) =
    B.padLeft (B.Pad 4) (vBox $ fmap toWidget is)
  toWidget (C.List _ l bs) = toList l bs
  toWidget (C.CodeBlock _ tx) =
    B.withDefAttr markdownAttr $
      B.vBox [ B.str " | " <+> B.txt ln | ln <- T.lines tx ]
  toWidget (C.HtmlBlock txt) = B.txt txt
  toWidget (C.HRule) = B.vLimit 1 (B.fill '*')

toInlineChunk :: Inlines -> Widget a
toInlineChunk is = B.Widget B.Fixed B.Fixed $ do
  ctx <- B.getContext
  let width = ctx^.B.availWidthL
      fs    = toFragments is
      ws    = fmap (fmap toWidget) (split width fs)
  B.render (vBox (fmap hBox ws))

toList :: ListType -> [Blocks] -> Widget a
toList = undefined

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
  toWidget fragment =
    style $ case fTextual fragment of
      TStr t       -> B.txt t
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
