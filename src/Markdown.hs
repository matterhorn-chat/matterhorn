{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ParallelListComp #-}

module Markdown (renderMessage) where

import           Brick ( (<+>), Widget )
import qualified Brick as B
import           Cheapskate.Types ( Block
                                  , Blocks
                                  , Inlines
                                  , ListType
                                  )
import qualified Cheapskate as C
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Foldable as F
import           Data.Monoid ((<>))
import           Data.Sequence ( Seq
                               , ViewL(..)
                               , ViewR(..)
                               , (<|)
                               , (|>)
                               , viewl
                               , viewr)
import qualified Data.Sequence as S
import           Data.Set (Set)
import qualified Data.Set as Set
import           Lens.Micro.Platform ((^.))

import           Themes
import           Types (MessageType(..), PostType(..))

type UserSet = Set Text

renderMessage :: Blocks -> Maybe Text -> MessageType -> UserSet -> Widget a
renderMessage bs u mTy uSet =
  case u of
    Just un
      | mTy == CP Emote -> B.str "*" <+> colorUsername un
                       <+> B.str " " <+> vBox (fmap (toWidget uSet) bs)
      | otherwise -> colorUsername un <+> B.str ": " <+> vBox (fmap (toWidget uSet) bs)
    Nothing -> vBox (fmap (toWidget uSet) bs)

vBox :: Foldable f => f (Widget a) -> Widget a
vBox = B.vBox . F.toList

hBox :: Foldable f => f (Widget a) -> Widget a
hBox = B.hBox . F.toList

--

class ToWidget t where
  toWidget :: UserSet -> t -> Widget a

header :: Int -> Widget a
header n = B.str (replicate n '#')

instance ToWidget Block where
  toWidget uPat (C.Para is) = toInlineChunk is uPat
  toWidget uPat (C.Header n is) =
    B.withDefAttr clientHeaderAttr
      (header n <+> B.str " " <+> toInlineChunk is uPat)
  toWidget uPat (C.Blockquote is) =
    B.padLeft (B.Pad 4) (vBox $ fmap (toWidget uPat) is)
  toWidget uPat (C.List _ l bs) = toList l bs uPat
  toWidget _ (C.CodeBlock _ tx) =
    B.withDefAttr codeAttr $
      B.vBox [ B.str " | " <+> B.txt ln | ln <- T.lines tx ]
  toWidget _ (C.HtmlBlock txt) = B.txt txt
  toWidget _ (C.HRule) = B.vLimit 1 (B.fill '*')

toInlineChunk :: Inlines -> UserSet -> Widget a
toInlineChunk is uSet = B.Widget B.Fixed B.Fixed $ do
  ctx <- B.getContext
  let width = ctx^.B.availWidthL
      fs    = toFragments uSet is
      ws    = fmap gatherWidgets (split width fs)
  B.render (vBox (fmap hBox ws))

toList :: ListType -> [Blocks] -> UserSet -> Widget a
toList lt bs uSet = vBox
  [ B.str i <+> (vBox (fmap (toWidget uSet) b))
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
  | TLink Text
  | TRawHtml Text
    deriving (Show, Eq)

data FragmentStyle
  = Normal
  | Emph
  | Strong
  | Code
  | User
  | Link
  | Emoji
    deriving (Eq, Show)

-- We convert it pretty mechanically:
toFragments :: UserSet -> Inlines -> Seq Fragment
toFragments uSet = go Normal
  where go n (viewl-> C.Str "@" :<
                      (viewl-> C.Str t :< xs))
          | t `Set.member` uSet =
            Fragment (TStr ("@" <> t)) User <| go n xs
        go n (viewl-> C.Str ":" :< (viewl-> C.Str t :< (viewl-> C.Str ":" :< xs))) =
            Fragment (TStr (":" <> t <> ":")) Emoji <| go n xs
        go n (viewl-> C.Str t :< xs)
          | t `Set.member` uSet =
            Fragment (TStr t) User <| go n xs
          | otherwise =
            Fragment (TStr t) n <| go n xs
        go n (viewl-> C.Space :< xs) =
          Fragment TSpace n <| go n xs
        go n (viewl-> C.SoftBreak :< xs) =
          Fragment TSoftBreak n <| go n xs
        go n (viewl-> C.LineBreak :< xs) =
          Fragment TLineBreak n <| go n xs
        go n (viewl-> C.Link label url _ :< xs) =
          let urlFrags = [ Fragment (TStr " (")  n
                         , Fragment (TLink url) Link
                         , Fragment (TStr ")")  n
                         ]
          in case F.toList label of
              [C.Str s] | s == url -> Fragment (TLink url) Link <| go n xs
              _                    -> go n label <> S.fromList urlFrags <> go n xs
        go n (viewl-> C.RawHtml t :< xs) =
          Fragment (TRawHtml t) n <| go n xs
        go n (viewl-> C.Code t :< xs) =
          Fragment (TStr t) Code <| go n xs
        go n (viewl-> C.Emph is :< xs) =
          go Emph is <> go n xs
        go n (viewl-> C.Strong is :< xs) =
          go Strong is <> go n xs
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
  TLink t    -> T.length t
  TRawHtml t -> T.length t
  TLineBreak -> 0
  _          -> 1

strOf :: TextFragment -> Text
strOf f = case f of
  TStr t     -> t
  TLink t    -> t
  TRawHtml t -> t
  TSpace     -> " "
  _          -> ""

-- This finds adjacent string-ey fragments and concats them, so
-- we can use fewer widgets
gatherWidgets :: Seq Fragment -> Seq (Widget a)
gatherWidgets (viewl-> (Fragment frag style :< rs)) = go style (strOf frag) rs
  where go s t (viewl-> (Fragment f s' :< xs))
          | s == s' = go s (t <> strOf f) xs
        go s t xs =
          let w = case s of
                Normal -> B.txt t
                Emph   -> B.withDefAttr clientEmphAttr (B.txt t)
                Strong -> B.withDefAttr clientStrongAttr (B.txt t)
                Code   -> B.withDefAttr codeAttr (B.txt t)
                Link   -> B.withDefAttr urlAttr (B.txt t)
                Emoji  -> B.withDefAttr emojiAttr (B.txt t)
                User   -> B.withDefAttr (attrForUsername t)
                                        (B.txt t)
          in w <| gatherWidgets xs
gatherWidgets _ =
  S.empty
