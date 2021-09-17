{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ParallelListComp #-}
module Matterhorn.Draw.RichText
  ( renderRichText
  , renderText
  , renderText'
  , cursorSentinel
  , findVerbatimChunk
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick ( (<+>), Widget, hLimit, imageL
                       , render, Size(..), Widget(..)
                       )
import qualified Brick as B
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Table as B
import qualified Brick.Widgets.Skylighting as BS
import           Control.Monad.Reader
import qualified Data.Foldable as F
import qualified Data.Sequence as S
import           Data.Sequence ( ViewL(..)
                               , (<|)
                               , viewl
                               )
import qualified Data.Text as T
import qualified Graphics.Vty as V
import qualified Skylighting.Core as Sky

import           Matterhorn.Constants ( normalChannelSigil, editMarking )
import           Matterhorn.Draw.RichText.Flatten
import           Matterhorn.Draw.RichText.Wrap
import           Matterhorn.Themes
import           Matterhorn.Types ( HighlightSet(..), emptyHSet, SemEq(..)
                                  , addUserSigil, resultToWidget )
import           Matterhorn.Types.RichText


-- | Render rich text.
renderRichText :: SemEq a
               => Text
               -- ^ The username of the currently-authenticated user.
               -> HighlightSet
               -- ^ A highlight set for highlighting channel and
               -- usernames and code blocks.
               -> Maybe Int
               -- ^ An optional maximum width.
               -> Bool
               -- ^ Whether to do line wrapping.
               -> Maybe Int
               -- ^ At what height to truncate long verbatim/code blocks
               -> Maybe (Int -> Inline -> Maybe a)
               -- ^ An optional function to build resource names for
               -- clickable regions.
               -> Blocks
               -- ^ The content to render.
               -> Widget a
renderRichText curUser hSet w doWrap doVerbTrunc nameGen (Blocks bs) =
    runReader (do
              blocks <- mapM renderBlock (addBlankLines bs)
              return $ B.vBox $ toList blocks)
              (DrawCfg { drawCurUser = curUser
                       , drawHighlightSet = hSet
                       , drawLineWidth = w
                       , drawDoLineWrapping = doWrap
                       , drawTruncateVerbatimBlocks = doVerbTrunc
                       , drawNameGen = nameGen
                       })

-- Render text to markdown without username highlighting, permalink
-- detection, or clickable links
renderText :: SemEq a => Text -> Widget a
renderText txt = renderText' Nothing "" emptyHSet Nothing txt

renderText' :: SemEq a
            => Maybe TeamBaseURL
            -- ^ An optional base URL against which to match post links.
            -> Text
            -- ^ The username of the currently-authenticated user.
            -> HighlightSet
            -- ^ A highlight set for highlighting channel and usernames.
            -> Maybe (Int -> Inline -> Maybe a)
            -- ^ An optional function to build resource names for
            -- clickable regions.
            -> Text
            -- ^ The text to parse and then render as rich text.
            -> Widget a
renderText' baseUrl curUser hSet nameGen t =
    renderRichText curUser hSet Nothing True Nothing nameGen $
        parseMarkdown baseUrl t

-- Add blank lines only between adjacent elements of the same type, to
-- save space
addBlankLines :: Seq Block -> Seq Block
addBlankLines = go' . viewl
  where go' EmptyL = S.empty
        go' (x :< xs) = go x (viewl xs)
        go a (b :< rs)
            | sameBlockType a b = a <| blank <| go b (viewl rs)
            | otherwise         = a <| go b (viewl rs)
        go x EmptyL = S.singleton x
        blank = Para (Inlines $ S.singleton ESpace)

vBox :: F.Foldable f => f (Widget a) -> Widget a
vBox = B.vBox . toList

hBox :: F.Foldable f => f (Widget a) -> Widget a
hBox = B.hBox . toList

header :: Int -> Widget a
header n = B.txt (T.replicate n "#")

maybeHLimit :: Maybe Int -> Widget a -> Widget a
maybeHLimit Nothing w = w
maybeHLimit (Just i) w = hLimit i w

type M a b = Reader (DrawCfg b) a

data DrawCfg a =
    DrawCfg { drawCurUser :: Text
            , drawHighlightSet :: HighlightSet
            , drawLineWidth :: Maybe Int
            , drawDoLineWrapping :: Bool
            , drawTruncateVerbatimBlocks :: Maybe Int
            , drawNameGen :: Maybe (Int -> Inline -> Maybe a)
            }

renderBlock :: SemEq a => Block -> M (Widget a) a
renderBlock (Table aligns headings body) = do
    headingWs <- mapM renderInlines headings
    bodyWs <- forM body $ mapM renderInlines
    let t = B.table (headingWs : bodyWs)
        alignPairs = zip [0..] aligns
        align (_, LeftAlignedCol) = id
        align (_, DefaultAlignedCol) = id
        align (i, RightAlignedCol) = B.alignRight i
        align (i, CenterAlignedCol) = B.alignCenter i
        applyAlignment = foldr (.) id (align <$> alignPairs)
    return $ B.renderTable $ applyAlignment t
renderBlock (Para is) =
    renderInlines is
renderBlock (Header n is) = do
    headerTxt <- withReader (\c -> c { drawLineWidth = subtract 1 <$> drawLineWidth c }) $
                 renderInlines is
    return $ B.withDefAttr clientHeaderAttr $
        hBox [ B.padRight (B.Pad 1) $ header n
             , headerTxt
             ]
renderBlock (Blockquote bs) = do
    w <- asks drawLineWidth
    bws <- mapM renderBlock (unBlocks bs)
    return $ maybeHLimit w $ addQuoting $ vBox bws
renderBlock (List ty spacing bs) = do
    w <- asks drawLineWidth
    lst <- renderList ty spacing bs
    return $ maybeHLimit w lst
renderBlock (CodeBlock ci tx) = do
    hSet <- asks drawHighlightSet

    let f = maybe renderRawCodeBlock
                  (renderCodeBlock (hSyntaxMap hSet))
                  mSyntax
        mSyntax = do
            lang <- codeBlockLanguage ci
            Sky.lookupSyntax lang (hSyntaxMap hSet)
    w <- f tx
    trunc <- asks drawTruncateVerbatimBlocks
    case trunc of
        Nothing -> return w
        Just maxHeight -> return $ maybeTruncVerbatim maxHeight w
renderBlock (HTMLBlock t) = do
    w <- asks drawLineWidth
    return $ maybeHLimit w $ textWithCursor t
renderBlock (HRule) = do
    w <- asks drawLineWidth
    return $ maybeHLimit w $ B.vLimit 1 (B.fill '*')

maybeTruncVerbatim :: Int -> B.Widget n -> B.Widget n
maybeTruncVerbatim maxHeight w =
    Widget (B.hSize w) (B.vSize w) $ do
        result <- render w
        let h = V.imageHeight (result^.B.imageL)
        if h > maxHeight
           then render $ B.vBox [ B.vLimit maxHeight $ B.Widget B.Fixed B.Fixed $ return result
                                , B.withDefAttr verbatimTruncateMessageAttr $
                                  B.str $ "(Showing " <> show maxHeight <> " of " <> show h <> " lines)"
                                ]
           else return result

quoteChar :: Char
quoteChar = '>'

addQuoting :: B.Widget n -> B.Widget n
addQuoting w =
    B.Widget B.Fixed (B.vSize w) $ do
        ctx <- B.getContext
        childResult <- B.render $ B.hLimit (ctx^.B.availWidthL - 2) w

        let quoteBorder = B.raw $ V.charFill (ctx^.B.attrL) quoteChar 1 height
            height = V.imageHeight $ childResult^.B.imageL

        B.render $ B.hBox [ B.padRight (B.Pad 1) quoteBorder
                          , B.Widget B.Fixed B.Fixed $ return childResult
                          ]

renderCodeBlock :: Sky.SyntaxMap -> Sky.Syntax -> Text -> M (Widget a) b
renderCodeBlock syntaxMap syntax tx = do
    let result = Sky.tokenize cfg syntax tx
        cfg = Sky.TokenizerConfig syntaxMap False
    case result of
        Left _ -> renderRawCodeBlock tx
        Right tokLines -> do
            let padding = B.padLeftRight 1 (B.vLimit (length tokLines) B.vBorder)
            return $ (B.txt $ "[" <> Sky.sName syntax <> "]") B.<=>
                     (padding <+> BS.renderRawSource textWithCursor tokLines)

renderRawCodeBlock :: Text -> M (Widget a) b
renderRawCodeBlock tx = do
    doWrap <- asks drawDoLineWrapping

    let hPolicy = if doWrap then Greedy else Fixed
    return $ B.withDefAttr codeAttr $
        Widget hPolicy Fixed $ do
            c <- B.getContext
            let theLines = expandEmpty <$> T.lines tx
                expandEmpty "" = " "
                expandEmpty s  = s
                wrapFunc = if doWrap then wrappedTextWithCursor
                                     else textWithCursor
            renderedText <- render (B.hLimit (c^.B.availWidthL - 3) $ B.vBox $
                                    wrapFunc <$> theLines)

            let textHeight = V.imageHeight $ renderedText^.imageL
                padding = B.padLeftRight 1 (B.vLimit textHeight B.vBorder)

            render $ padding <+> (resultToWidget renderedText)

renderInlines :: SemEq a => Inlines -> M (Widget a) a
renderInlines es = do
    w <- asks drawLineWidth
    hSet <- asks drawHighlightSet
    curUser <- asks drawCurUser
    nameGen <- asks drawNameGen

    return $ B.Widget B.Fixed B.Fixed $ do
        ctx <- B.getContext
        let width = fromMaybe (ctx^.B.availWidthL) w
            ws    = fmap (renderWrappedLine curUser) $
                    mconcat $
                    (doLineWrapping width <$> (F.toList $ flattenInlineSeq hSet nameGen es))
        B.render (vBox ws)

renderList :: SemEq a => ListType -> ListSpacing -> Seq Blocks -> M (Widget a) a
renderList ty _spacing bs = do
    let is = case ty of
          BulletList _ -> repeat ("• ")
          OrderedList s _ Period ->
            [ T.pack (show (n :: Int)) <> ". " | n <- [s..] ]
          OrderedList s _ OneParen ->
            [ T.pack (show (n :: Int)) <> ") " | n <- [s..] ]
          OrderedList s _ TwoParens ->
            [ T.pack (show (n :: Int)) <> ")) " | n <- [s..] ]

    results <- forM (zip is $ unBlocks <$> (F.toList bs)) $ \(i, b) -> do
        blocks <- mapM renderBlock b
        return $ B.txt i <+> vBox blocks

    return $ vBox results

renderWrappedLine :: Show a => Text -> WrappedLine a -> Widget a
renderWrappedLine curUser l = hBox $ F.toList $ renderFlattenedValue curUser <$> l

renderFlattenedValue :: Show a => Text -> FlattenedValue a -> Widget a
renderFlattenedValue curUser (NonBreaking rs) =
    let renderLine = hBox . F.toList . fmap (renderFlattenedValue curUser)
    in vBox (F.toList $ renderLine <$> F.toList rs)
renderFlattenedValue curUser (SingleInline fi) = addClickable $ addHyperlink $ addStyles widget
    where
        val = fiValue fi
        mUrl = fiURL fi
        mName = fiName fi
        styles = fiStyles fi

        addStyles w = foldr addStyle w styles
        addStyle s =
            B.withDefAttr $ case s of
                Strong        -> clientStrongAttr
                Code          -> codeAttr
                Permalink     -> permalinkAttr
                Strikethrough -> strikeThroughAttr
                Emph          -> clientEmphAttr

        addHyperlink = case mUrl of
            Nothing -> id
            Just u -> B.withDefAttr urlAttr . B.hyperlink (unURL u)

        addClickable w = case mName of
            Nothing -> id w
            Just nm -> B.clickable nm w

        widget = case val of
            FSpace               -> B.txt " "
            FUser u              -> colorUsername curUser u $ addUserSigil u
            FChannel c           -> B.withDefAttr channelNameAttr $
                                    B.txt $ normalChannelSigil <> c
            FEmoji em            -> B.withDefAttr emojiAttr $
                                    B.txt $ ":" <> em <> ":"
            FText t              -> if t == T.singleton (cursorSentinel)
                                    then B.visible $ B.txt " "
                                    else textWithCursor t
            FEditSentinel recent -> let attr = if recent
                                               then editedRecentlyMarkingAttr
                                               else editedMarkingAttr
                                    in B.withDefAttr attr $ B.txt editMarking

textWithCursor :: Text -> Widget a
textWithCursor t
    | T.any (== cursorSentinel) t = B.visible $ B.txt $ removeCursor t
    | otherwise = B.txt t

wrappedTextWithCursor :: Text -> Widget a
wrappedTextWithCursor t
    | T.any (== cursorSentinel) t = B.visible $ B.txtWrap $ removeCursor t
    | otherwise = B.txtWrap t

removeCursor :: Text -> Text
removeCursor = T.filter (/= cursorSentinel)

-- Cursor sentinel for tracking the user's cursor position in previews.
cursorSentinel :: Char
cursorSentinel = '‸'
