{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | This module provides a set of data types to represent message text.
-- The types here are based loosely on the @cheapskate@ package's types
-- but provide higher-level support for the kinds of things we find in
-- Mattermost messages such as user and channel references.
--
-- To parse a Markdown document, use 'parseMarkdown'. To actually render
-- text in this representation, see the module 'Draw.RichText'.
module Matterhorn.Types.RichText
  ( Blocks(..)
  , unBlocks

  , Block(..)
  , ListType(..)
  , CodeBlockInfo(..)
  , NumDecoration(..)
  , Inline(..)
  , TeamBaseURL(..)
  , TeamURLName(..)

  , URL(..)
  , unURL

  , parseMarkdown

  , findUsernames
  , blockGetURLs
  , findVerbatimChunk

  -- Exposed for testing only:
  , fromMarkdownBlocks
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import qualified Cheapskate as C
import           Data.Char ( isAlphaNum, isAlpha )
import qualified Data.Foldable as F
import           Data.Monoid (First(..))
import qualified Data.Set as S
import qualified Data.Sequence as Seq
import           Data.Sequence ( (<|), ViewL((:<)) )
import qualified Data.Text as T

import           Network.Mattermost.Types ( PostId(..), Id(..), ServerBaseURL(..) )

import           Matterhorn.Constants ( userSigil, normalChannelSigil )

-- | A team name found in a Mattermost post URL
data TeamURLName = TeamURLName Text
                 deriving (Eq, Show, Ord)

-- | A server base URL with a team name.
data TeamBaseURL = TeamBaseURL TeamURLName ServerBaseURL
                 deriving (Eq, Show)

-- | A sequence of rich text blocks.
newtype Blocks = Blocks (Seq Block)
            deriving (Semigroup, Monoid, Show)

unBlocks :: Blocks -> Seq Block
unBlocks (Blocks bs) = bs

-- | A block in a rich text document.
data Block =
    Para (Seq Inline)
    -- ^ A paragraph.
    | Header Int (Seq Inline)
    -- ^ A section header with specified depth and contents.
    | Blockquote Blocks
    -- ^ A blockquote.
    | List Bool ListType (Seq Blocks)
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
    deriving (Eq, Show, Ord)

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
                  deriving (Eq, Show, Ord)

-- | Ways to decorate numbered itemized list items. The decoration
-- follows the list item number.
data NumDecoration =
    Paren
    | Period
    deriving (Eq, Show, Ord)

-- | A URL.
newtype URL = URL Text
            deriving (Eq, Show, Ord)

unURL :: URL -> Text
unURL (URL url) = url

-- | The kinds of inline values that can appear in rich text blocks.
data Inline =
    EText Text
    -- ^ Plain text.
    | EEmph (Seq Inline)
    -- ^ Emphasized (usually italicized) content.
    | EStrikethrough (Seq Inline)
    -- ^ Strikethrough content.
    | EStrong (Seq Inline)
    -- ^ Boldface content.
    | ECode (Seq Inline)
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
    | EHyperlink URL (Maybe (Seq Inline))
    -- ^ A hyperlink to the specified URL. Optionally provides an
    -- element sequence indicating the URL's text label; if absent, the
    -- label is understood to be the URL itself.
    | EImage URL (Maybe (Seq Inline))
    -- ^ An image at the specified URL. Optionally provides an element
    -- sequence indicating the image's "alt" text label; if absent, the
    -- label is understood to be the URL itself.
    | EEmoji Text
    -- ^ An emoji reference. The text here includes only the text
    -- portion, not the colons, e.g. "foo" instead of ":foo:".
    | ENonBreaking (Seq Inline)
    -- ^ A sequence of elements that must never be separated during line
    -- wrapping.
    | EPermalink TeamURLName PostId (Maybe (Seq Inline))
    -- ^ A permalink to the specified team (name) and post ID with an
    -- optional label.
    deriving (Show, Eq, Ord)

-- | Parse markdown input text to RichText.
parseMarkdown :: Maybe TeamBaseURL
              -- ^ If provided, perform post link detection whenever a
              -- hyperlink is parsed by checking to see if the post link
              -- is a post in this Mattermost team
              -> T.Text
              -- ^ The markdown input text to parse
              -> Blocks
parseMarkdown baseUrl t =
    fromMarkdownBlocks baseUrl bs where C.Doc _ bs = C.markdown C.def t

-- | Convert a sequence of markdown (Cheapskate) blocks into rich text
-- blocks.
fromMarkdownBlocks :: Maybe TeamBaseURL -> C.Blocks -> Blocks
fromMarkdownBlocks baseUrl = Blocks . fmap (fromMarkdownBlock baseUrl)

-- | Convert a single markdown block into a single rich text block.
fromMarkdownBlock :: Maybe TeamBaseURL -> C.Block -> Block
fromMarkdownBlock baseUrl (C.Para is) =
    Para $ fromMarkdownInlines baseUrl is
fromMarkdownBlock baseUrl (C.Header level is) =
    Header level $ fromMarkdownInlines baseUrl is
fromMarkdownBlock baseUrl (C.Blockquote bs) =
    Blockquote $ Blocks $ fromMarkdownBlock baseUrl <$> bs
fromMarkdownBlock baseUrl (C.List f ty bss) =
    List f (fromMarkdownListType ty) $ (Blocks . fmap (fromMarkdownBlock baseUrl)) <$> Seq.fromList bss
fromMarkdownBlock _ (C.CodeBlock attr body) =
    CodeBlock (fromMarkdownCodeAttr attr) body
fromMarkdownBlock _ (C.HtmlBlock body) =
    HTMLBlock body
fromMarkdownBlock _ C.HRule =
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

-- | Remove hyperlinks from an inline sequence. This should only be used
-- for sequences that are themselves used as labels for hyperlinks; this
-- prevents them from embedding their own hyperlinks, which is nonsense.
--
-- Any hyperlinks found in the sequence will be replaced with a text
-- represent of their URL (if they have no label) or the label itself
-- otherwise.
removeHyperlinks :: Seq C.Inline -> Seq C.Inline
removeHyperlinks is =
    case Seq.viewl is of
        h :< t ->
            case h of
                C.Link label theUrl _ ->
                    if Seq.null label
                    then C.Str theUrl <| removeHyperlinks t
                    else removeHyperlinks label <> removeHyperlinks t
                _ -> h <| removeHyperlinks t
        Seq.EmptyL -> mempty

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
-- This function is also partially responsible for paving the way for
-- line-wrapping later on when RichText is rendered. This means that,
-- when possible, the elements produced by this function need to be as
-- small as possible, without losing structure information. An example
-- of this is Markdown inline code fragments, such as "`this is code`".
-- This function will convert that one Markdown inline code fragment
-- into a sequence of five RichText elements, each with a "Code" style
-- assigned: @[EText "this", ESpace, EText "is", ESpace, EText "code"]@.
-- This "flattening" of the original Markdown structure makes it much
-- easier to do line-wrapping without losing style information because
-- it is possible to gather up tokens that do not exceed line widths
-- without losing style information. This is key to rendering the text
-- with the right terminal attributes.
--
-- However, there are a couple of cases where such flattening does *not*
-- happen: hyperlinks and images. In these cases we do not flatten the
-- (arbitrary) text label structure of links and images because we need
-- to be able to recover those labels to gather up URLs to show to the
-- user in the URL list. So we leave the complete text structure of
-- those labels behind in the 'EHyperlink' and 'EImage' constructors as
-- sequences of Elements. This means that logic in 'Draw.RichText' that
-- does line-wrapping will have to explicitly break up link and image
-- labels across line breaks.
fromMarkdownInlines :: Maybe TeamBaseURL -> Seq C.Inline -> Seq Inline
fromMarkdownInlines baseUrl inlines =
    let go is = case Seq.viewl is of
          C.Str "~" :< xs ->
              case Seq.viewl xs of
                  C.Str "~" :< xs2 ->
                      case takeUntilStrikethroughEnd xs2 of
                          Nothing -> EText "~" <| go xs
                          Just (strikethroughInlines, rest) ->
                              (EStrikethrough $ go strikethroughInlines) <|
                              go rest
                  _ ->
                      let (cFrags, rest) = Seq.spanl isNameFragment xs
                          cn = T.concat (unsafeGetStr <$> F.toList cFrags)
                      in if not (T.null cn)
                         then EChannel cn <| go rest
                         else EText normalChannelSigil <| go xs
          C.Str ":" :< xs ->
              let validEmojiFragment (C.Str f) =
                      f `elem` ["_", "-"] || T.all isAlphaNum f
                  validEmojiFragment _ = False
                  (emojiFrags, rest) = Seq.spanl validEmojiFragment xs
                  em = T.concat $ unsafeGetStr <$> F.toList emojiFrags
              in case Seq.viewl rest of
                  C.Str ":" :< rest2 ->
                      EEmoji em <| go rest2
                  _ ->
                      EText ":" <| go xs
          C.Str t :< xs | userSigil `T.isPrefixOf` t ->
              let (uFrags, rest) = Seq.spanl isNameFragment xs
                  t' = T.concat $ t : (unsafeGetStr <$> F.toList uFrags)
                  u = T.drop 1 t'
              in EUser u <| go rest
          C.Str t :< xs ->
              EText t <| go xs
          C.Space :< xs ->
              ESpace <| go xs
          C.SoftBreak :< xs ->
              ESoftBreak <| go xs
          C.LineBreak :< xs ->
              ELineBreak <| go xs
          C.Link label theUrl _ :< xs ->
              let mLabel = if Seq.null label
                           then Nothing
                           else case F.toList label of
                               [C.Str u] | u == theUrl -> Nothing
                               _ -> Just $ fromMarkdownInlines baseUrl $ removeHyperlinks label
                  rest = go xs
                  this = case flip getPermalink theUrl =<< baseUrl of
                      Nothing ->
                          let url = URL theUrl
                          in EHyperlink url mLabel
                      Just (tName, pId) ->
                          EPermalink tName pId mLabel
              in this <| rest
          C.Image altIs theUrl _ :< xs ->
              let mLabel = if Seq.null altIs
                           then Nothing
                           else Just $ fromMarkdownInlines baseUrl altIs
                  url = URL theUrl
              in EImage url mLabel <| go xs
          C.RawHtml t :< xs ->
              ERawHtml t <| go xs
          C.Code t :< xs ->
              -- We turn a single code string into individual Inlines
              -- so we can perform line-wrapping on the inline code
              -- text.
              let ts = [ frag
                       | wd <- T.split (== ' ') t
                       , frag <- case wd of
                           "" -> [ESpace]
                           _  -> [ESpace, EText wd]
                       ]
                  ts' = case ts of
                    (ESpace:rs) -> rs
                    _           -> ts
              in ECode (Seq.fromList ts') <| go xs
          C.Emph as :< xs ->
              (EEmph $ go as) <| go xs
          C.Strong as :< xs ->
              (EStrong $ go as) <| go xs
          C.Entity t :< xs ->
              EText t <| go xs
          Seq.EmptyL -> mempty

    in go inlines

takeUntilStrikethroughEnd :: Seq C.Inline -> Maybe (Seq C.Inline, Seq C.Inline)
takeUntilStrikethroughEnd is =
    let go pos s = case Seq.viewl s of
            C.Str "~" :< rest ->
                case Seq.viewl rest of
                    C.Str "~" :< _ ->
                        Just pos
                    _ -> go (pos + 1) rest
            _ :< rest -> go (pos + 1) rest
            Seq.EmptyL -> Nothing
    in do
        pos <- go 0 is
        let (h, t) = Seq.splitAt pos is
        return (h, Seq.drop 2 t)

-- | If the specified URL matches the active server base URL and team
-- and refers to a post, extract the team name and post ID values and
-- return them.
getPermalink :: TeamBaseURL -> Text -> Maybe (TeamURLName, PostId)
getPermalink (TeamBaseURL tName (ServerBaseURL baseUrl)) url =
    let newBaseUrl = if "/" `T.isSuffixOf` baseUrl
                     then baseUrl
                     else baseUrl <> "/"
    in if not $ newBaseUrl `T.isPrefixOf` url
       then Nothing
       else let rest = T.drop (T.length newBaseUrl) url
                (tName', rawPIdStr) = T.breakOn "/pl/" rest
                pIdStr = T.drop 4 rawPIdStr
            in if tName == TeamURLName tName' && not (T.null pIdStr)
               then Just (tName, PI $ Id pIdStr)
               else Nothing


unsafeGetStr :: C.Inline -> Text
unsafeGetStr (C.Str t) = t
unsafeGetStr _ = error "BUG: unsafeGetStr called on non-Str Inline"

-- | Obtain all username references in a rich text document.
findUsernames :: Blocks -> S.Set T.Text
findUsernames (Blocks bs) = S.unions $ F.toList $ fmap blockFindUsernames bs

blockFindUsernames :: Block -> S.Set T.Text
blockFindUsernames (Para is) =
    inlineFindUsernames $ F.toList is
blockFindUsernames (Header _ is) =
    inlineFindUsernames $ F.toList is
blockFindUsernames (Blockquote bs) =
    findUsernames bs
blockFindUsernames (List _ _ bs) =
    S.unions $ F.toList $ findUsernames <$> bs
blockFindUsernames _ =
    mempty

inlineFindUsernames :: [Inline] -> S.Set T.Text
inlineFindUsernames [] = mempty
inlineFindUsernames (i : is) =
    case i of
        EUser u -> S.insert u $ inlineFindUsernames is
        _ -> inlineFindUsernames is

-- | Obtain all URLs (and optional labels) in a rich text block.
blockGetURLs :: Block -> [(Either (TeamURLName, PostId) URL, Maybe (Seq Inline))]
blockGetURLs (Para is) =
    catMaybes $ elementGetURL <$> toList is
blockGetURLs (Header _ is) =
    catMaybes $ elementGetURL <$> toList is
blockGetURLs (Blockquote bs) =
    mconcat $ blockGetURLs <$> toList (unBlocks bs)
blockGetURLs (List _ _ bss) =
    mconcat $ mconcat $
    (fmap blockGetURLs . F.toList . unBlocks) <$> F.toList bss
blockGetURLs _ =
    mempty

elementGetURL :: Inline -> Maybe (Either (TeamURLName, PostId) URL, Maybe (Seq Inline))
elementGetURL (EHyperlink url label) =
    Just (Right url, label)
elementGetURL (EImage url label) =
    Just (Right url, label)
elementGetURL (EPermalink tName pId label) =
    Just (Left (tName, pId), label)
elementGetURL _ =
    Nothing

-- | Find the first code block in a sequence of rich text blocks.
findVerbatimChunk :: Blocks -> Maybe Text
findVerbatimChunk (Blocks bs) = getFirst $ F.foldMap go bs
  where go (CodeBlock _ t) = First (Just t)
        go _               = First Nothing

isValidNameChar :: Char -> Bool
isValidNameChar c = isAlpha c || c == '_' || c == '.' || c == '-'

isNameFragment :: C.Inline -> Bool
isNameFragment (C.Str t) =
    not (T.null t) && isValidNameChar (T.head t)
isNameFragment _ = False
