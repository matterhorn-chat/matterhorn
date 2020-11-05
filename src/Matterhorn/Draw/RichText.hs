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

import           Matterhorn.Constants ( normalChannelSigil, userSigil, editMarking )
import           Matterhorn.Draw.RichText.Flatten
import           Matterhorn.Draw.RichText.Wrap
import           Matterhorn.Themes
import           Matterhorn.Types ( HighlightSet(..), emptyHSet )
import           Matterhorn.Types.RichText


-- Cursor sentinel for tracking the user's cursor position in previews.
cursorSentinel :: Char
cursorSentinel = '‸'

-- Render markdown with username highlighting
renderRichText :: Text -> HighlightSet -> Maybe Int -> Bool -> Blocks -> Widget a
renderRichText curUser hSet w doWrap (Blocks bs) =
    runReader (do
              blocks <- mapM blockToWidget (addBlankLines bs)
              return $ B.vBox $ toList blocks)
              (DrawCfg { drawCurUser = curUser
                       , drawHighlightSet = hSet
                       , drawLineWidth = w
                       , drawDoLineWrapping = doWrap
                       })

-- Add blank lines only between adjacent elements of the same type, to
-- save space
addBlankLines :: Seq Block -> Seq Block
addBlankLines = go' . viewl
  where go' EmptyL = S.empty
        go' (x :< xs) = go x (viewl xs)
        go a@Para {} (b@Para {} :< rs) =
             a <| blank <| go b (viewl rs)
        go a@Header {} (b@Header {} :< rs) =
             a <| blank <| go b (viewl rs)
        go a@Blockquote {} (b@Blockquote {} :< rs) =
             a <| blank <| go b (viewl rs)
        go a@List {} (b@List {} :< rs) =
             a <| blank <| go b (viewl rs)
        go a@CodeBlock {} (b@CodeBlock {} :< rs) =
             a <| blank <| go b (viewl rs)
        go a@HTMLBlock {} (b@HTMLBlock {} :< rs) =
             a <| blank <| go b (viewl rs)
        go x (y :< rs) = x <| go y (viewl rs)
        go x (EmptyL) = S.singleton x
        blank = Para (Inlines $ S.singleton ESpace)

-- Render text to markdown without username highlighting or permalink detection
renderText :: Text -> Widget a
renderText txt = renderText' Nothing "" emptyHSet txt

renderText' :: Maybe TeamBaseURL -> Text -> HighlightSet -> Text -> Widget a
renderText' baseUrl curUser hSet t = renderRichText curUser hSet Nothing True rtBs
  where rtBs = parseMarkdown baseUrl t

vBox :: F.Foldable f => f (Widget a) -> Widget a
vBox = B.vBox . toList

hBox :: F.Foldable f => f (Widget a) -> Widget a
hBox = B.hBox . toList

header :: Int -> Widget a
header n = B.txt (T.replicate n "#")

maybeHLimit :: Maybe Int -> Widget a -> Widget a
maybeHLimit Nothing w = w
maybeHLimit (Just i) w = hLimit i w

type M a = Reader DrawCfg a

data DrawCfg =
    DrawCfg { drawCurUser :: Text
            , drawHighlightSet :: HighlightSet
            , drawLineWidth :: Maybe Int
            , drawDoLineWrapping :: Bool
            }

blockToWidget :: Block -> M (Widget a)
blockToWidget (Para is) =
    wrapInlines is
blockToWidget (Header n is) = do
    headerTxt <- withReader (\c -> c { drawLineWidth = subtract 1 <$> drawLineWidth c }) $
                 wrapInlines is
    return $ B.withDefAttr clientHeaderAttr $
        hBox [ B.padRight (B.Pad 1) $ header n
             , headerTxt
             ]
blockToWidget (Blockquote bs) = do
    w <- asks drawLineWidth
    bws <- mapM blockToWidget (unBlocks bs)
    return $ maybeHLimit w $ addQuoting $ vBox bws
blockToWidget (List ty spacing bs) = do
    w <- asks drawLineWidth
    lst <- blocksToList ty spacing bs
    return $ maybeHLimit w lst
blockToWidget (CodeBlock ci tx) = do
    hSet <- asks drawHighlightSet

    let f = maybe rawCodeBlockToWidget
                  (codeBlockToWidget (hSyntaxMap hSet))
                  mSyntax
        mSyntax = do
            lang <- codeBlockLanguage ci
            Sky.lookupSyntax lang (hSyntaxMap hSet)
    f tx
blockToWidget (HTMLBlock t) = do
    w <- asks drawLineWidth
    return $ maybeHLimit w $ textWithCursor t
blockToWidget (HRule) = do
    w <- asks drawLineWidth
    return $ maybeHLimit w $ B.vLimit 1 (B.fill '*')

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

codeBlockToWidget :: Sky.SyntaxMap -> Sky.Syntax -> Text -> M (Widget a)
codeBlockToWidget syntaxMap syntax tx = do
    let result = Sky.tokenize cfg syntax tx
        cfg = Sky.TokenizerConfig syntaxMap False
    case result of
        Left _ -> rawCodeBlockToWidget tx
        Right tokLines -> do
            let padding = B.padLeftRight 1 (B.vLimit (length tokLines) B.vBorder)
            return $ (B.txt $ "[" <> Sky.sName syntax <> "]") B.<=>
                     (padding <+> BS.renderRawSource textWithCursor tokLines)

rawCodeBlockToWidget :: Text -> M (Widget a)
rawCodeBlockToWidget tx = do
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

            render $ padding <+> (Widget Fixed Fixed $ return renderedText)

wrapInlines :: Inlines -> M (Widget a)
wrapInlines es = do
    w <- asks drawLineWidth
    hSet <- asks drawHighlightSet
    curUser <- asks drawCurUser

    return $ B.Widget B.Fixed B.Fixed $ do
        ctx <- B.getContext
        let width = fromMaybe (ctx^.B.availWidthL) w
            ws    = fmap (renderWrappedLine curUser) $
                    mconcat $
                    (doLineWrapping width <$> (F.toList $ flattenInlineSeq hSet es))
        B.render (vBox ws)

blocksToList :: ListType -> ListSpacing -> Seq Blocks -> M (Widget a)
blocksToList ty _spacing bs = do
    let is = case ty of
          BulletList _ -> repeat ("• ")
          OrderedList s _ Period ->
            [ T.pack (show (n :: Int)) <> ". " | n <- [s..] ]
          OrderedList s _ OneParen ->
            [ T.pack (show (n :: Int)) <> ") " | n <- [s..] ]
          OrderedList s _ TwoParens ->
            [ T.pack (show (n :: Int)) <> ")) " | n <- [s..] ]

    results <- forM (zip is $ unBlocks <$> (F.toList bs)) $ \(i, b) -> do
        blocks <- mapM blockToWidget b
        return $ B.txt i <+> vBox blocks

    return $ vBox results

renderWrappedLine :: Text -> WrappedLine -> Widget a
renderWrappedLine curUser l = hBox $ F.toList $ renderFlattenedValue curUser <$> l

renderFlattenedValue :: Text -> FlattenedValue -> Widget a
renderFlattenedValue curUser (NonBreaking rs) =
    let renderLine = hBox . F.toList . fmap (renderFlattenedValue curUser)
    in vBox (F.toList $ renderLine <$> F.toList rs)
renderFlattenedValue curUser (SingleInline fi) = addHyperlink $ addStyles widget
    where
        val = fiValue fi
        mUrl = fiURL fi
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

        widget = case val of
            FSpace                       -> B.txt " "
            FUser u                      -> colorUsername curUser u $ userSigil <> u
            FChannel c                   -> B.withDefAttr channelNameAttr $
                                            B.txt $ normalChannelSigil <> c
            FEmoji em                    -> B.withDefAttr emojiAttr $
                                            B.txt $ ":" <> em <> ":"
            FText t                      -> if t == T.singleton (cursorSentinel)
                                            then B.visible $ B.txt " "
                                            else textWithCursor t
            FEditSentinel recent         -> let attr = if recent
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
