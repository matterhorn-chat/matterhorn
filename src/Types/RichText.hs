module Types.RichText
  ( RichTextBlock(..)
  , ListType(..)
  , CodeBlockInfo(..)
  , NumDecoration(..)
  , Element(..)
  , ElementData(..)
  , ElementStyle(..)

  , URL(..)
  , unURL

  , fromMarkdownBlocks
  , setElementStyle

  , findUsernames
  , blockGetURLs
  , findVerbatimChunk
  )
where

import           Prelude ()
import           Prelude.MH

import qualified Cheapskate as C
import           Data.Char ( isAlphaNum, isAlpha )
import qualified Data.Foldable as F
import           Data.Monoid (First(..))
import qualified Data.Set as S
import qualified Data.Sequence as Seq
import           Data.Sequence ( (<|), ViewL((:<)) )
import qualified Data.Text as T

import           Constants ( userSigil, normalChannelSigil )

data RichTextBlock =
    Para (Seq Element)
    | Header Int (Seq Element)
    | Blockquote (Seq RichTextBlock)
    | List Bool ListType (Seq (Seq RichTextBlock))
    | CodeBlock CodeBlockInfo Text
    | HTMLBlock Text
    | HRule
    deriving (Show)

data ListType =
    Bullet Char
    | Numbered NumDecoration Int
    deriving (Eq, Show)

data CodeBlockInfo =
    CodeBlockInfo { codeBlockLanguage :: Text
                  , codeBlockInfo :: Text
                  }
                  deriving (Eq, Show)

data NumDecoration =
    Paren
    | Period
    deriving (Eq, Show)

data Element =
    Element { eStyle :: ElementStyle
            , eData :: ElementData
            }
            deriving (Show, Eq)

setElementStyle :: ElementStyle -> Element -> Element
setElementStyle s e = e { eStyle = s }

newtype URL = URL Text
            deriving (Eq, Show, Ord)

unURL :: URL -> Text
unURL (URL url) = url

data ElementData =
    EText Text
    | ESpace
    | ESoftBreak
    | ELineBreak
    | ERawHtml Text
    | EEditSentinel
    | EEditRecentlySentinel
    | EUser Text
    | EChannel Text
    | EHyperlink URL (Maybe (Seq Element))
    | EImage URL (Maybe (Seq Element))
    | EEmoji Text
    deriving (Show, Eq)

data ElementStyle =
    Normal
    | Emph
    | Strong
    | Code
    | Edited
    | EditedRecently
    | Hyperlink URL
    | Emoji
    deriving (Eq, Show)

fromMarkdownBlocks :: C.Blocks -> Seq RichTextBlock
fromMarkdownBlocks = fmap fromMarkdownBlock

fromMarkdownBlock :: C.Block -> RichTextBlock
fromMarkdownBlock (C.Para is) =
    Para $ fromMarkdownInlines is
fromMarkdownBlock (C.Header level is) =
    Header level $ fromMarkdownInlines is
fromMarkdownBlock (C.Blockquote bs) =
    Blockquote $ fromMarkdownBlock <$> bs
fromMarkdownBlock (C.List f ty bss) =
    List f (fromMarkdownListType ty) $ fmap fromMarkdownBlock <$> Seq.fromList bss
fromMarkdownBlock (C.CodeBlock attr body) =
    CodeBlock (fromMarkdownCodeAttr attr) body
fromMarkdownBlock (C.HtmlBlock body) =
    HTMLBlock body
fromMarkdownBlock C.HRule =
    HRule

fromMarkdownCodeAttr :: C.CodeAttr -> CodeBlockInfo
fromMarkdownCodeAttr (C.CodeAttr lang info) =
    CodeBlockInfo lang info

fromMarkdownListType :: C.ListType -> ListType
fromMarkdownListType (C.Bullet c) =
    Bullet c
fromMarkdownListType (C.Numbered wrap i) =
    let dec = case wrap of
                  C.PeriodFollowing -> Period
                  C.ParenFollowing -> Paren
    in Numbered dec i

-- | Convert a sequence of Markdown inline values to a sequence of
-- Elements.
--
-- This conversion converts sequences of Markdown AST fragments into
-- RichText elements. In particular, this function may determine that
-- some sequences of Markdown text fragments belong together, such as
-- the sequence @["@", "joe", "-", "user"]@, which should be treated as
-- a single "@joe-user" token due to username character validation. When
-- appropriate, this function does such consolidation when converting to
-- RichText elements.
--
-- This function is also responsible for paving the way for
-- line-wrapping later on when RichText is rendered. This means that,
-- when possible, the elements produced by this function need to be as
-- small as possible, without losing structure information. An example
-- of this is Markdown inline code fragments, such as "`this is code`".
-- This function will convert that one Markdown inline code fragment
-- into a sequence of five RichText elements, each with a "Code" style
-- assigned: @[EText "this", ESpace, EText "is", ESpace, EText "code"]@.
-- This "flattening" of the original Markdown structure makes it much
-- easier to do line-wrapping without losing style information, which is
-- key to rendering the text with the right terminal attributes.
--
-- However, there are a couple of cases where such flattening does *not*
-- happen: hyperlinks and images. In these cases we do not flatten the
-- (arbitrary) text label structure of links and images because we need
-- to be able to recover those labels to gather up URLs to show to the
-- user in the URL list. So we leave the complete text structure of
-- those labels behind in the EHyperlink and EImage constructors as
-- sequences of Elements. This means that logic to do line-wrapping will
-- have to explicitly break up link and image labels across line breaks.
fromMarkdownInlines :: Seq C.Inline -> Seq Element
fromMarkdownInlines inlines =
    let go sty is = case Seq.viewl is of
          C.Str ":" :< xs ->
              let validEmojiFragment (C.Str f) =
                      f `elem` ["_", "-"] || T.all isAlphaNum f
                  validEmojiFragment _ = False
                  (emojiFrags, rest) = Seq.spanl validEmojiFragment xs
                  em = T.concat $ unsafeGetStr <$> F.toList emojiFrags
              in case Seq.viewl rest of
                  C.Str ":" :< rest2 ->
                      Element Emoji (EEmoji em) <| go sty rest2
                  _ ->
                      Element sty (EText ":") <| go sty xs
          C.Str t :< xs | userSigil `T.isPrefixOf` t ->
              let (uFrags, rest) = Seq.spanl isNameFragment xs
                  t' = T.concat $ t : (unsafeGetStr <$> F.toList uFrags)
                  u = T.drop 1 t'
              in Element sty (EUser u) <| go sty rest
          C.Str t :< xs | normalChannelSigil `T.isPrefixOf` t ->
              let (cFrags, rest) = Seq.spanl isNameFragment xs
                  cn = T.drop 1 $ T.concat $ t : (unsafeGetStr <$> F.toList cFrags)
              in Element sty (EChannel cn) <| go sty rest
          C.Str t :< xs ->
              Element sty (EText t) <| go sty xs
          C.Space :< xs ->
              Element sty ESpace <| go sty xs
          C.SoftBreak :< xs ->
              Element sty ESoftBreak <| go sty xs
          C.LineBreak :< xs ->
              Element sty ELineBreak <| go sty xs
          C.Link label theUrl _ :< xs ->
              let mLabel = if Seq.null label
                           then Nothing
                           else Just $ fromMarkdownInlines label
                  url = URL theUrl
              in (Element (Hyperlink url) $ EHyperlink url mLabel) <| go sty xs
          C.Image altIs theUrl _ :< xs ->
              let mLabel = if Seq.null altIs
                           then Nothing
                           else Just $ fromMarkdownInlines altIs
                  url = URL theUrl
              in (Element (Hyperlink url) $ EImage url mLabel) <| go sty xs
          C.RawHtml t :< xs ->
              Element sty (ERawHtml t) <| go sty xs
          C.Code t :< xs ->
              -- We turn a single code string into individual Elements
              -- so we can perform line-wrapping on the inline code
              -- text.
              let ts = [ Element Code frag
                       | wd <- T.split (== ' ') t
                       , frag <- case wd of
                           "" -> [ESpace]
                           _  -> [ESpace, EText wd]
                       ]
                  ts' = case ts of
                    (Element _ ESpace:rs) -> rs
                    _                     -> ts
              in Seq.fromList ts' <> go sty xs
          C.Emph as :< xs ->
              go Emph as <> go sty xs
          C.Strong as :< xs ->
              go Strong as <> go sty xs
          C.Entity t :< xs ->
              Element sty (EText t) <| go sty xs
          Seq.EmptyL -> mempty

    in go Normal inlines

unsafeGetStr :: C.Inline -> Text
unsafeGetStr (C.Str t) = t
unsafeGetStr _ = error "BUG: unsafeGetStr called on non-Str Inline"

findUsernames :: Seq RichTextBlock -> S.Set T.Text
findUsernames = S.unions . F.toList . fmap blockFindUsernames

blockFindUsernames :: RichTextBlock -> S.Set T.Text
blockFindUsernames (Para is) =
    elementFindUsernames $ F.toList is
blockFindUsernames (Header _ is) =
    elementFindUsernames $ F.toList is
blockFindUsernames (Blockquote bs) =
    findUsernames bs
blockFindUsernames (List _ _ bs) =
    S.unions $ F.toList $ findUsernames <$> bs
blockFindUsernames _ =
    mempty

elementFindUsernames :: [Element] -> S.Set T.Text
elementFindUsernames [] = mempty
elementFindUsernames (e : es) =
    case eData e of
        EUser u -> S.insert u $ elementFindUsernames es
        _ -> elementFindUsernames es

blockGetURLs :: RichTextBlock -> [(URL, Maybe (Seq Element))]
blockGetURLs (Para is) = catMaybes $ elementGetURL <$> toList is
blockGetURLs (Header _ is) = catMaybes $ elementGetURL <$> toList is
blockGetURLs (Blockquote bs) = mconcat $ blockGetURLs <$> toList bs
blockGetURLs (List _ _ bss) =
    mconcat $ mconcat $
    (fmap blockGetURLs . F.toList) <$> F.toList bss
blockGetURLs _ = mempty

elementGetURL :: Element -> Maybe (URL, Maybe (Seq Element))
elementGetURL (Element _ (EHyperlink url label)) =
    Just (url, label)
elementGetURL (Element _ (EImage url label)) =
    Just (url, label)
elementGetURL _ =
    Nothing

findVerbatimChunk :: Seq RichTextBlock -> Maybe Text
findVerbatimChunk = getFirst . F.foldMap go
  where go (CodeBlock _ t) = First (Just t)
        go _               = First Nothing

isValidNameChar :: Char -> Bool
isValidNameChar c = isAlpha c || c == '_' || c == '.' || c == '-'

isNameFragment :: C.Inline -> Bool
isNameFragment (C.Str t) =
    not (T.null t) && isValidNameChar (T.head t)
isNameFragment _ = False
