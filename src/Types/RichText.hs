-- | This module provides a set of data types to represent message text.
-- The types here are based loosely on the @cheapskate@ package's types
-- but provide higher-level support for the kinds of things we find in
-- Mattermost messages such as user and channel references.
--
-- To convert from Cheapskate's representation, use
-- 'fromMarkdownBlocks'. To actually render text in this representation,
-- see the module 'Draw.RichText'.
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

-- | A block in a rich text document.
data RichTextBlock =
    Para (Seq Element)
    -- ^ A paragraph.
    | Header Int (Seq Element)
    -- ^ A section header with specified depth and contents.
    | Blockquote (Seq RichTextBlock)
    -- ^ A blockquote.
    | List Bool ListType (Seq (Seq RichTextBlock))
    -- ^ An itemized list.
    | CodeBlock CodeBlockInfo Text
    -- ^ A code block.
    | HTMLBlock Text
    -- ^ A fragment of raw HTML.
    | HRule
    -- ^ A horizontal rule.
    deriving (Show)

-- | The type of itemized list items.
data ListType =
    Bullet Char
    -- ^ Decorate the items with bullet using the specified character.
    | Numbered NumDecoration Int
    -- ^ Number the items starting at the specified number; use the
    -- indicated decoration following the number.
    deriving (Eq, Show)

-- | Information about a code block.
data CodeBlockInfo =
    CodeBlockInfo { codeBlockLanguage :: Maybe Text
                  -- ^ The language of the source code in the code
                  -- block, if any. This is encoded in Markdown as a
                  -- sequence of non-whitespace characters following the
                  -- fenced code block opening backticks.
                  , codeBlockInfo :: Maybe Text
                  -- ^ Any text that comes after the language token.
                  -- This text is separated from the language token by
                  -- whitespace.
                  }
                  deriving (Eq, Show)

-- | Ways to decorate numbered itemized list items. The decoration
-- follows the list item number.
data NumDecoration =
    Paren
    | Period
    deriving (Eq, Show)

-- | A single logical inline element in a rich text block.
data Element =
    Element { eStyle :: ElementStyle
            -- ^ The element's visual style.
            , eData :: ElementData
            -- ^ The element's data.
            }
            deriving (Show, Eq)

setElementStyle :: ElementStyle -> Element -> Element
setElementStyle s e = e { eStyle = s }

-- | A URL.
newtype URL = URL Text
            deriving (Eq, Show, Ord)

unURL :: URL -> Text
unURL (URL url) = url

-- | The kinds of data that can appear in rich text elements.
data ElementData =
    EText Text
    -- ^ A sequence of non-whitespace characters.
    | ESpace
    -- ^ A single space.
    | ESoftBreak
    -- ^ A soft line break.
    | ELineBreak
    -- ^ A hard line break.
    | ERawHtml Text
    -- ^ Raw HTML.
    | EEditSentinel Bool
    -- ^ A sentinel indicating that some text has been edited (used
    -- to indicate that mattermost messages have been edited by their
    -- authors). This has no parsable representation; it is only used
    -- to annotate a message prior to rendering to add a visual editing
    -- indicator. The boolean indicates whether the edit was "recent"
    -- (True) or not (False).
    | EUser Text
    -- ^ A user reference. The text here includes only the username, not
    -- the sigil.
    | EChannel Text
    -- ^ A channel reference. The text here includes only the channel
    -- name, not the sigil.
    | EHyperlink URL (Maybe (Seq Element))
    -- ^ A hyperlink to the specified URL. Optionally provides an
    -- element sequence indicating the URL's text label; if absent, the
    -- label is understood to be the URL itself.
    | EImage URL (Maybe (Seq Element))
    -- ^ An image at the specified URL. Optionally provides an element
    -- sequence indicating the image's "alt" text label; if absent, the
    -- label is understood to be the URL itself.
    | EEmoji Text
    -- ^ An emoji reference. The text here includes only the text
    -- portion, not the colons, e.g. "foo" instead of ":foo:".
    deriving (Show, Eq)

-- | Element visual styles.
data ElementStyle =
    Normal
    -- ^ Normal text
    | Emph
    -- ^ Emphasized text
    | Strong
    -- ^ Bold text
    | Code
    -- ^ Inline code segment or code block
    | Hyperlink URL ElementStyle
    -- ^ A terminal hyperlink to the specified URL, composed with
    -- another element style
    deriving (Eq, Show)

-- | Convert a sequence of markdown (Cheapskate) blocks into rich text
-- blocks.
fromMarkdownBlocks :: C.Blocks -> Seq RichTextBlock
fromMarkdownBlocks = fmap fromMarkdownBlock

-- | Convert a single markdown block into a single rich text block.
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
    let strippedLang = T.strip lang
        strippedInfo = T.strip info
        maybeText t = if T.null t then Nothing else Just t
    in CodeBlockInfo (maybeText strippedLang)
                     (maybeText strippedInfo)

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
                      Element Normal (EEmoji em) <| go sty rest2
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
              in (Element (Hyperlink url sty) $ EHyperlink url mLabel) <| go sty xs
          C.Image altIs theUrl _ :< xs ->
              let mLabel = if Seq.null altIs
                           then Nothing
                           else Just $ fromMarkdownInlines altIs
                  url = URL theUrl
              in (Element (Hyperlink url sty) $ EImage url mLabel) <| go sty xs
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

-- | Obtain all username references in a sequence of rich text blocks.
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

-- | Obtain all URLs (and optional labels) in a rich text block.
blockGetURLs :: RichTextBlock -> [(URL, Maybe (Seq Element))]
blockGetURLs (Para is) =
    catMaybes $ elementGetURL <$> toList is
blockGetURLs (Header _ is) =
    catMaybes $ elementGetURL <$> toList is
blockGetURLs (Blockquote bs) =
    mconcat $ blockGetURLs <$> toList bs
blockGetURLs (List _ _ bss) =
    mconcat $ mconcat $
    (fmap blockGetURLs . F.toList) <$> F.toList bss
blockGetURLs _ =
    mempty

elementGetURL :: Element -> Maybe (URL, Maybe (Seq Element))
elementGetURL (Element _ (EHyperlink url label)) =
    Just (url, label)
elementGetURL (Element _ (EImage url label)) =
    Just (url, label)
elementGetURL _ =
    Nothing

-- | Find the first code block in a sequence of rich text blocks.
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
