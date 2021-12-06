{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | This module provides a set of data types to represent message text.
-- The inline and block types in this module are designed to represent
-- most of what is found in Markdown documents (particularly the
-- Commonmark specification) in addition to other things we find in
-- Mattermost messages, such as username or channel references.
--
-- To parse a Markdown document, use 'parseMarkdown'. To actually render
-- text in this representation, see the module 'Draw.RichText'.
module Matterhorn.Types.RichText
  ( Blocks(..)
  , unBlocks

  , Block(..)
  , sameBlockType
  , CodeBlockInfo(..)
  , Inline(..)
  , Inlines(..)
  , unInlines

  , C.ListType(..)
  , C.ListSpacing(..)
  , C.EnumeratorType(..)
  , C.DelimiterType(..)
  , C.ColAlignment(..)

  , TeamBaseURL(..)
  , TeamURLName(..)

  , URL(..)
  , unURL

  , parseMarkdown

  , findUsernames
  , blockGetURLs
  , findVerbatimChunk
  , makePermalink
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import qualified Commonmark as C
import qualified Commonmark.Extensions as C
import qualified Commonmark.Inlines as C
import qualified Commonmark.TokParsers as C
import           Control.Monad.Identity
import qualified Data.Foldable as F
import           Data.List ( intersperse )
import           Data.Monoid (First(..))
import qualified Data.Set as S
import qualified Data.Sequence as Seq
import           Data.Sequence ( (<|), viewl, viewr, ViewL((:<)), ViewR((:>)) )
import qualified Data.Text as T
import qualified Text.Parsec as P

import           Network.Mattermost.Types ( PostId(..), Id(..), ServerBaseURL(..) )

import           Matterhorn.Constants ( userSigilChar, normalChannelSigilChar )

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

singleB :: Block -> Blocks
singleB = Blocks . Seq.singleton

-- | A block in a rich text document.
--
-- NOTE: update 'sameBlockType' when constructors are added to this
-- type.
data Block =
    Para Inlines
    -- ^ A paragraph.
    | Header Int Inlines
    -- ^ A section header with specified depth and contents.
    | Blockquote Blocks
    -- ^ A blockquote.
    | List C.ListType C.ListSpacing (Seq Blocks)
    -- ^ An itemized list.
    | CodeBlock CodeBlockInfo Text
    -- ^ A code block.
    | HTMLBlock Text
    -- ^ A fragment of raw HTML.
    | HRule
    -- ^ A horizontal rule.
    | Table [C.ColAlignment] [Inlines] [[Inlines]]
    -- ^ A table.
    deriving (Show)

-- | Returns whether two blocks have the same type.
sameBlockType :: Block -> Block -> Bool
sameBlockType (Para {})       (Para {})       = True
sameBlockType (Header {})     (Header {})     = True
sameBlockType (Blockquote {}) (Blockquote {}) = True
sameBlockType (List {})       (List {})       = True
sameBlockType (CodeBlock {})  (CodeBlock {})  = True
sameBlockType (HTMLBlock {})  (HTMLBlock {})  = True
sameBlockType _               _               = False

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

-- | A URL.
newtype URL = URL Text
            deriving (Eq, Show, Ord)

unURL :: URL -> Text
unURL (URL url) = url

-- | The kinds of inline values that can appear in rich text blocks.
data Inline =
    EText Text
    -- ^ Plain text that SHOULD be a contiguous sequence of
    -- non-whitespace characters.
    | EEmph Inlines
    -- ^ Emphasized (usually italicized) content.
    | EStrikethrough Inlines
    -- ^ Strikethrough content.
    | EStrong Inlines
    -- ^ Boldface content.
    | ECode Inlines
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
    | EHyperlink URL Inlines
    -- ^ A hyperlink to the specified URL. Optionally provides an
    -- element sequence indicating the URL's text label; if absent, the
    -- label is understood to be the URL itself.
    | EImage URL Inlines
    -- ^ An image at the specified URL. Optionally provides an element
    -- sequence indicating the image's "alt" text label; if absent, the
    -- label is understood to be the URL itself.
    | EEmoji Text
    -- ^ An emoji reference. The text here includes only the text
    -- portion, not the colons, e.g. "foo" instead of ":foo:".
    | ENonBreaking Inlines
    -- ^ A sequence of elements that must never be separated during line
    -- wrapping.
    | EPermalink TeamURLName PostId (Maybe Inlines)
    -- ^ A permalink to the specified team (name) and post ID with an
    -- optional label.
    deriving (Show, Eq, Ord)

-- | A sequence of inline values.
newtype Inlines = Inlines (Seq Inline)
                deriving (Monoid, Ord, Eq, Show)

unInlines :: Inlines -> Seq Inline
unInlines (Inlines is) = is

singleI :: Inline -> Inlines
singleI = Inlines . Seq.singleton

instance Semigroup Inlines where
    (Inlines l) <> (Inlines r) =
        Inlines $ case (viewr l, viewl r) of
            (lInit :> lLast, rHead :< rTail) ->
                case (lLast, rHead) of
                    (EText a, EText b) ->
                        lInit <> ((EText $ a <> b) <| rTail)
                    (ECode a, ECode b) ->
                        lInit <> ((ECode $ a <> b) <| rTail)
                    (EEmph a, EEmph b) ->
                        lInit <> ((EEmph $ a <> b) <| rTail)
                    (EStrikethrough a, EStrikethrough b) ->
                        lInit <> ((EStrikethrough $ a <> b) <| rTail)
                    (EStrong a, EStrong b) ->
                        lInit <> ((EStrong $ a <> b) <| rTail)
                    (_, _) ->
                        l <> r
            (_, _) -> l <> r

-- A dummy instance just to satisfy commonmark; we don't use this.
instance C.Rangeable Inlines where
    ranged _ = id

-- A dummy instance just to satisfy commonmark; we don't use this.
instance C.HasAttributes Inlines where
    addAttributes _ = id

instance C.IsInline Inlines where
    lineBreak = singleI ELineBreak
    softBreak = singleI ESoftBreak
    str t = Inlines $ Seq.fromList $
            filter (/= (EText "")) $
            intersperse ESpace $ EText <$> T.splitOn " " t
    entity = singleI . EText
    escapedChar = singleI . EText . T.singleton
    emph = singleI . EEmph
    strong = singleI . EStrong
    link url _title desc = singleI $ EHyperlink (URL url) desc
    image url _title desc = singleI $ EImage (URL url) desc
    code t = singleI $ ECode $ C.str t
    rawInline _ = singleI . ERawHtml

instance C.HasStrikethrough Inlines where
    strikethrough = singleI . EStrikethrough

instance C.HasPipeTable Inlines Blocks where
    pipeTable a h b = singleB $ Table a h b

-- Syntax extension for parsing ~channel references.
channelSpec :: (Monad m) => C.SyntaxSpec m Inlines Blocks
channelSpec =
    mempty { C.syntaxInlineParsers = [C.withAttributes parseChannel]
           }

parseChannel :: (Monad m) => C.InlineParser m Inlines
parseChannel = P.try $ do
    void $ C.symbol normalChannelSigilChar
    let chunk = C.satisfyWord (const True) <|> C.symbol '_' <|> C.symbol '-'
    cts <- P.many1 chunk
    return $ singleI $ EChannel $ C.untokenize cts

-- Syntax extension for parsing @username references.
usernameSpec :: (Monad m) => C.SyntaxSpec m Inlines Blocks
usernameSpec =
    mempty { C.syntaxInlineParsers = [C.withAttributes parseUsername]
           }

parseUsername :: (Monad m) => C.InlineParser m Inlines
parseUsername = P.try $ do
    void $ C.symbol userSigilChar
    let chunk = C.satisfyWord (const True) <|> C.symbol '_' <|> C.symbol '-'
        [period] = C.tokenize "" "."
    uts <- intersperse period <$> P.sepBy1 chunk (C.symbol '.')
    return $ singleI $ EUser $ C.untokenize uts

-- Syntax extension for parsing :emoji: references.
--
-- NOTE: the commonmark-extensions package also provides a syntax
-- extension for exactly this. Why don't we use it? I'm glad you asked.
-- We don't use it because that extension actually checks to see whether
-- emoji are valid by looking in a database (provided by the 'emojis'
-- package). While that's actually a great feature, it is problematic
-- when that package's emoji database does not exactly match the one
-- that the Mattermost server uses. As a result, Matterhorn may think
-- that some valid emoji (according to the server) is invalid (according
-- to the 'emojis' package). Instead of using that extension, we made
-- our own that does *not* validate the emoji references at parse time.
-- We just parse them and keep them around, and then validate them at
-- *render* time. That way we can allow anything to parse, but change
-- how we render valid and invalid emoji based on a copy of the server's
-- emoji database that we bundle with Matterhorn.
emojiSpec :: (Monad m) => C.SyntaxSpec m Inlines Blocks
emojiSpec =
    mempty { C.syntaxInlineParsers = [C.withAttributes parseEmoji]
           }

parseEmoji :: (Monad m) => C.InlineParser m Inlines
parseEmoji = P.try $ do
    void $ C.symbol ':'
    ts <- P.many1 $ C.satisfyWord (const True)
               <|> C.symbol '_'
               <|> C.symbol '+'
               <|> C.symbol '-'
    void $ C.symbol ':'
    let kw = C.untokenize ts
    return $ singleI $ EEmoji kw

-- A dummy instance just to satisfy commonmark; we don't use this.
instance C.HasAttributes Blocks where
    addAttributes _ = id

-- A dummy instance just to satisfy commonmark; we don't use this.
instance C.Rangeable Blocks where
    ranged _ = id

instance C.IsBlock Inlines Blocks where
    paragraph = singleB . Para
    plain = singleB . Para
    thematicBreak = singleB HRule
    blockQuote = singleB . Blockquote
    codeBlock infoTxt content = singleB $ CodeBlock (parseCodeBlockInfo infoTxt) content
    heading level i = singleB $ Header level i
    rawBlock _format content = singleB $ CodeBlock (parseCodeBlockInfo "") content
    list ty spacing bs = singleB $ List ty spacing $ Seq.fromList bs
    referenceLinkDefinition _label (_dest, _title) = mempty

parseCodeBlockInfo :: Text -> CodeBlockInfo
parseCodeBlockInfo t = CodeBlockInfo lang info
    where
        ws = T.words t
        (lang, info) = case ws of
            [l, i] -> (Just l, Just i)
            [l]    -> (Just l, Nothing)
            _      -> (Nothing, Nothing)

-- | Parse markdown input text to RichText.
--
-- Note that this always returns a block sequence even if the input
-- cannot be parsed. It isn't yet clear just how permissive the
-- commonmark parser is, but so far we have not encountered any issues.
-- If the input document is so broken that commonmark cannot parse it,
-- we return an empty block sequence.
parseMarkdown :: Maybe TeamBaseURL
              -- ^ If provided, perform post link detection whenever a
              -- hyperlink is parsed by checking to see if the post link
              -- is a post in this Mattermost team
              -> T.Text
              -- ^ The markdown input text to parse
              -> Blocks
parseMarkdown mBaseUrl t =
    let customSyntax = mconcat $ markdownExtensions <> [C.defaultSyntaxSpec]
        markdownExtensions =
            [ C.autolinkSpec
            , C.strikethroughSpec
            , C.pipeTableSpec
            , usernameSpec
            , channelSpec
            , emojiSpec
            ]

    in case runIdentity $ C.commonmarkWith customSyntax "-" t of
        Left _ -> mempty
        Right bs -> case mBaseUrl of
            Nothing -> bs
            Just baseUrl -> rewriteBlocksPermalinks baseUrl bs

makePermalink :: TeamBaseURL -> PostId -> Text
makePermalink (TeamBaseURL (TeamURLName tName) (ServerBaseURL baseUrl)) pId =
    baseUrl <> tName <> "/pl/" <> unId (unPI pId)

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

-- | Locate post hyperlinks in the block sequence and rewrite them as
-- post permalinks.
rewriteBlocksPermalinks :: TeamBaseURL -> Blocks -> Blocks
rewriteBlocksPermalinks u (Blocks bs) = Blocks $ rewriteBlockPermalinks u <$> bs

-- | Locate post hyperlinks in the block and rewrite them as post
-- permalinks.
rewriteBlockPermalinks :: TeamBaseURL -> Block -> Block
rewriteBlockPermalinks u (Table a h b) = Table a (rewriteInlinePermalinks u <$> h)
                                                 (fmap (fmap (rewriteInlinePermalinks u)) b)
rewriteBlockPermalinks u (Para s) = Para $ rewriteInlinePermalinks u s
rewriteBlockPermalinks u (Header i s) = Header i $ rewriteInlinePermalinks u s
rewriteBlockPermalinks u (Blockquote bs) = Blockquote $ rewriteBlocksPermalinks u bs
rewriteBlockPermalinks u (List ty spacing bss) = List ty spacing $ rewriteBlocksPermalinks u <$> bss
rewriteBlockPermalinks _ b@(CodeBlock {}) = b
rewriteBlockPermalinks _ b@(HTMLBlock {}) = b
rewriteBlockPermalinks _ b@HRule = b

-- | Locate post hyperlinks in the inline sequence and rewrite them as
-- post permalinks.
rewriteInlinePermalinks :: TeamBaseURL -> Inlines -> Inlines
rewriteInlinePermalinks u (Inlines is) = Inlines $ rewriteInlinePermalink u <$> is

-- | Locate post hyperlinks in the inline value and rewrite them as post
-- permalinks.
rewriteInlinePermalink :: TeamBaseURL -> Inline -> Inline
rewriteInlinePermalink u i@(EHyperlink url label) =
    case getPermalink u (unURL url) of
        Nothing -> i
        Just (tName, pId) ->
            -- Get rid of permalink labels if they just match the URL,
            -- because that's how Commonmark-extensions parses them. We
            -- would rather only preserve the label if it differs from
            -- the URL.
            let newLabel = if label == Inlines (Seq.fromList [EText $ unURL url])
                           then Nothing
                           else Just label
            in EPermalink tName pId newLabel
rewriteInlinePermalink u (EEmph s) = EEmph $ rewriteInlinePermalinks u s
rewriteInlinePermalink u (ECode s) = ECode $ rewriteInlinePermalinks u s
rewriteInlinePermalink u (EStrikethrough s) = EStrikethrough $ rewriteInlinePermalinks u s
rewriteInlinePermalink u (EStrong s) = EStrong $ rewriteInlinePermalinks u s
rewriteInlinePermalink u (ENonBreaking s) = ENonBreaking $ rewriteInlinePermalinks u s
rewriteInlinePermalink _ i@(EText {}) = i
rewriteInlinePermalink _ i@ESpace = i
rewriteInlinePermalink _ i@ESoftBreak = i
rewriteInlinePermalink _ i@ELineBreak = i
rewriteInlinePermalink _ i@(EEditSentinel {}) = i
rewriteInlinePermalink _ i@(ERawHtml {}) = i
rewriteInlinePermalink _ i@(EEmoji {}) = i
rewriteInlinePermalink _ i@(EUser {}) = i
rewriteInlinePermalink _ i@(EChannel {}) = i
rewriteInlinePermalink _ i@(EImage {}) = i
rewriteInlinePermalink _ i@(EPermalink {}) = i

-- | Obtain all username references in a rich text document.
findUsernames :: Blocks -> S.Set T.Text
findUsernames (Blocks bs) = S.unions $ F.toList $ fmap blockFindUsernames bs

blockFindUsernames :: Block -> S.Set T.Text
blockFindUsernames (Para is) =
    inlineFindUsernames $ F.toList $ unInlines is
blockFindUsernames (Header _ is) =
    inlineFindUsernames $ F.toList $ unInlines is
blockFindUsernames (Blockquote bs) =
    findUsernames bs
blockFindUsernames (Table _ header rows) =
    let cellFindUsernames = inlineFindUsernames . F.toList . unInlines
    in S.unions $
       ((cellFindUsernames <$> header) <>
        (concat $ (fmap cellFindUsernames) <$> rows))
blockFindUsernames (List _ _ bs) =
    S.unions $ F.toList $ findUsernames <$> bs
blockFindUsernames HRule =
    mempty
blockFindUsernames (HTMLBlock {}) =
    mempty
blockFindUsernames (CodeBlock {}) =
    mempty

inlineFindUsernames :: [Inline] -> S.Set T.Text
inlineFindUsernames [] = mempty
inlineFindUsernames (i : is) =
    case i of
        EUser u -> S.insert u $ inlineFindUsernames is
        _ -> inlineFindUsernames is

-- | Obtain all URLs (and optional labels) in a rich text block.
blockGetURLs :: Block -> [(Either (TeamURLName, PostId) URL, Maybe Inlines)]
blockGetURLs (Para is) =
    catMaybes $ elementGetURL <$> (toList $ unInlines is)
blockGetURLs (Header _ is) =
    catMaybes $ elementGetURL <$> (toList $ unInlines is)
blockGetURLs (Blockquote bs) =
    mconcat $ blockGetURLs <$> toList (unBlocks bs)
blockGetURLs (List _ _ bss) =
    mconcat $ mconcat $
    (fmap blockGetURLs . F.toList . unBlocks) <$> F.toList bss
blockGetURLs (Table _ header rows) =
    let cellFindURLs = catMaybes . fmap elementGetURL . F.toList . unInlines
    in (concatMap cellFindURLs header) <>
       (concatMap (concatMap cellFindURLs) rows)
blockGetURLs HRule =
    mempty
blockGetURLs (HTMLBlock {}) =
    mempty
blockGetURLs (CodeBlock {}) =
    mempty

elementGetURL :: Inline -> Maybe (Either (TeamURLName, PostId) URL, Maybe Inlines)
elementGetURL (EHyperlink url label) =
    Just (Right url, Just label)
elementGetURL (EImage url label) =
    Just (Right url, Just label)
elementGetURL (EPermalink tName pId label) =
    Just (Left (tName, pId), label)
elementGetURL _ =
    Nothing

-- | Find the first code block in a sequence of rich text blocks.
findVerbatimChunk :: Blocks -> Maybe Text
findVerbatimChunk (Blocks bs) = getFirst $ F.foldMap go bs
  where go (CodeBlock _ t) = First (Just t)
        go _               = First Nothing
