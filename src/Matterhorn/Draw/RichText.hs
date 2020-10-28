{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE RecordWildCards #-}
module Matterhorn.Draw.RichText
  ( MessageData(..)
  , renderRichText
  , renderMessage
  , renderText
  , renderText'
  , cursorSentinel
  , addEllipsis
  , findVerbatimChunk
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick ( (<+>), Widget, hLimit, imageL
                       , raw, render, Size(..), Widget(..)
                       )
import qualified Brick as B
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Skylighting as BS
import           Control.Monad.Reader
import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import           Data.Sequence ( ViewL(..)
                               , ViewR(..)
                               , (<|)
                               , (|>)
                               , viewl
                               , viewr)
import qualified Data.Sequence as S
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Graphics.Vty as V
import qualified Skylighting.Core as Sky

import           Network.Mattermost.Lenses ( postEditAtL, postCreateAtL )
import           Network.Mattermost.Types ( ServerTime(..) )

import           Matterhorn.Constants ( normalChannelSigil, userSigil, editMarking )
import           Matterhorn.Draw.RichText.Flatten
import           Matterhorn.Draw.RichText.Wrap
import           Matterhorn.Themes
import           Matterhorn.Types ( Name, HighlightSet(..) )
import           Matterhorn.Types.Messages
import           Matterhorn.Types.Posts
import           Matterhorn.Types.RichText


emptyHSet :: HighlightSet
emptyHSet = HighlightSet Set.empty Set.empty mempty

omittedUsernameType :: MessageType -> Bool
omittedUsernameType = \case
  CP Join -> True
  CP Leave -> True
  CP TopicChange -> True
  _ -> False

-- Add the edit sentinel to the end of the last block in the sequence.
-- If the last block is a paragraph, append it to that paragraph.
-- Otherwise, append a new block so it appears beneath the last
-- block-level element.
addEditSentinel :: Inline -> Seq Block -> Seq Block
addEditSentinel d bs =
    case viewr bs of
        EmptyR -> bs
        (rest :> b) -> rest <> appendEditSentinel d b

appendEditSentinel :: Inline -> Block -> Seq Block
appendEditSentinel sentinel b =
    let s = Para (S.singleton sentinel)
    in case b of
        Para is -> S.singleton $ Para (is |> ESpace |> sentinel)
        _ -> S.fromList [b, s]

-- | A bundled structure that includes all the information necessary
-- to render a given message
data MessageData =
    MessageData { mdEditThreshold :: Maybe ServerTime
                -- ^ If specified, any messages edited before this point
                -- in time are not indicated as edited.
                , mdShowOlderEdits :: Bool
                -- ^ Indicates whether "edited" markers should be shown
                -- for old messages (i.e., ignore the mdEditThreshold
                -- value).
                , mdShowReactions :: Bool
                -- ^ Whether to render reactions.
                , mdMessage :: Message
                -- ^ The message to render.
                , mdUserName :: Maybe Text
                -- ^ The username of the message's author, if any. This
                -- is passed here rather than obtaining from the message
                -- because we need to do lookups in the ChatState to
                -- compute this, and we don't pass the ChatState into
                -- renderMessage.
                , mdParentMessage :: Maybe Message
                -- ^ The parent message of this message, if any.
                , mdParentUserName :: Maybe Text
                -- ^ The author of the parent message, if any.
                , mdThreadState :: ThreadState
                -- ^ The thread state of this message.
                , mdRenderReplyParent :: Bool
                -- ^ Whether to render the parent message.
                , mdHighlightSet :: HighlightSet
                -- ^ The highlight set to use to highlight usernames,
                -- channel names, etc.
                , mdIndentBlocks :: Bool
                -- ^ Whether to indent the message underneath the
                -- author's name (True) or just display it to the right
                -- of the author's name (False).
                , mdMessageWidthLimit :: Maybe Int
                -- ^ A width override to use to wrap non-code blocks
                -- and code blocks without syntax highlighting. If
                -- unspecified, all blocks in the message will be
                -- wrapped and truncated at the width specified by the
                -- rendering context. If specified, all non-code blocks
                -- will be wrapped at this width and highlighted code
                -- blocks will be rendered using the context's width.
                , mdMyUsername :: Text
                -- ^ The username of the user running Matterhorn.
                , mdWrapNonhighlightedCodeBlocks :: Bool
                -- ^ Whether to wrap text in non-highlighted code
                -- blocks.
                }

-- | renderMessage performs markdown rendering of the specified message.
renderMessage :: MessageData -> Widget Name
renderMessage md@MessageData { mdMessage = msg, .. } =
    let msgUsr = case mdUserName of
          Just u -> if omittedUsernameType (msg^.mType) then Nothing else Just u
          Nothing -> Nothing
        botElem = if isBotMessage msg then B.txt "[BOT]" else B.emptyWidget
        nameElems = case msgUsr of
          Just un
            | isEmote msg ->
                [ B.withDefAttr pinnedMessageIndicatorAttr $ B.txt $ if msg^.mPinned then "[PIN]" else ""
                , B.txt $ (if msg^.mFlagged then "[!] " else "") <> "*"
                , colorUsername mdMyUsername un un
                , botElem
                , B.txt " "
                ]
            | otherwise ->
                [ B.withDefAttr pinnedMessageIndicatorAttr $ B.txt $ if msg^.mPinned then "[PIN] " else ""
                , colorUsername mdMyUsername un un
                , botElem
                , B.txt $ (if msg^.mFlagged then "[!]" else "") <> ": "
                ]
          Nothing -> []

        -- Use the editing threshold to determine whether to append an
        -- editing indication to this message.
        maybeAugment bs = case msg^.mOriginalPost of
            Nothing -> bs
            Just p ->
                if p^.postEditAtL > p^.postCreateAtL
                then case mdEditThreshold of
                    Just cutoff | p^.postEditAtL >= cutoff ->
                        addEditSentinel (EEditSentinel True) bs
                    _ -> if mdShowOlderEdits
                         then addEditSentinel (EEditSentinel False) bs
                         else bs
                else bs

        augmentedText = maybeAugment $ unBlocks $ msg^.mText
        msgWidget =
            vBox $ (layout mdHighlightSet mdMessageWidthLimit nameElems augmentedText . viewl) augmentedText :
                   catMaybes [msgAtch, msgReac]
        replyIndent = B.Widget B.Fixed B.Fixed $ do
            ctx <- B.getContext
            -- NB: The amount subtracted here must be the total padding
            -- added below (pad 1 + vBorder)
            w <- B.render $ B.hLimit (ctx^.B.availWidthL - 2) msgWidget
            B.render $ B.vLimit (V.imageHeight $ w^.B.imageL) $
                B.padRight (B.Pad 1) B.vBorder B.<+> (B.Widget B.Fixed B.Fixed $ return w)
        msgAtch = if Seq.null (msg^.mAttachments)
          then Nothing
          else Just $ B.withDefAttr clientMessageAttr $ vBox
                 [ B.txt ("  [attached: `" <> a^.attachmentName <> "`]")
                 | a <- toList (msg^.mAttachments)
                 ]
        msgReac = if Map.null (msg^.mReactions) || (not mdShowReactions)
          then Nothing
          else let renderR e us =
                       let n = Set.size us
                       in if | n == 1    -> " [" <> e <> "]"
                             | n > 1     -> " [" <> e <> " " <> T.pack (show n) <> "]"
                             | otherwise -> ""
                   reactionMsg = Map.foldMapWithKey renderR (msg^.mReactions)
               in Just $ B.withDefAttr emojiAttr $ B.txt ("   " <> reactionMsg)
        withParent p =
            case mdThreadState of
                NoThread -> msgWidget
                InThreadShowParent -> p B.<=> replyIndent
                InThread -> replyIndent
    in if not mdRenderReplyParent
       then msgWidget
       else case msg^.mInReplyToMsg of
          NotAReply -> msgWidget
          InReplyTo _ ->
              case mdParentMessage of
                  Nothing -> withParent (B.str "[loading...]")
                  Just pm ->
                      let parentMsg = renderMessage md
                            { mdShowOlderEdits    = False
                            , mdMessage           = pm
                            , mdUserName          = mdParentUserName
                            , mdParentMessage     = Nothing
                            , mdRenderReplyParent = False
                            , mdIndentBlocks      = False
                            }
                      in withParent (addEllipsis $ B.forceAttr replyParentAttr parentMsg)

    where
        layout :: HighlightSet -> Maybe Int -> [Widget Name] -> Seq Block
               -> ViewL Block -> Widget Name
        layout hs w nameElems bs xs | length xs > 1     = multiLnLayout hs w nameElems bs
        layout hs w nameElems bs (Blockquote {} :< _) = multiLnLayout hs w nameElems bs
        layout hs w nameElems bs (CodeBlock {} :< _)  = multiLnLayout hs w nameElems bs
        layout hs w nameElems bs (HTMLBlock {} :< _)  = multiLnLayout hs w nameElems bs
        layout hs w nameElems bs (List {} :< _)       = multiLnLayout hs w nameElems bs
        layout hs w nameElems bs (Para inlns :< _)
            | F.any breakCheck inlns                    = multiLnLayout hs w nameElems bs
        layout hs w nameElems bs _                      = nameNextToMessage hs w nameElems bs

        multiLnLayout hs w nameElems bs =
            if mdIndentBlocks
               then vBox [ hBox nameElems
                         , hBox [B.txt "  ", renderRichText mdMyUsername hs ((subtract 2) <$> w)
                                                 mdWrapNonhighlightedCodeBlocks (Blocks bs)]
                         ]
               else nameNextToMessage hs w nameElems bs

        nameNextToMessage hs w nameElems bs =
            Widget Fixed Fixed $ do
                nameResult <- render $ hBox nameElems
                let newW = subtract (V.imageWidth (nameResult^.imageL)) <$> w
                render $ hBox [ raw (nameResult^.imageL)
                              , renderRichText mdMyUsername hs newW mdWrapNonhighlightedCodeBlocks (Blocks bs)
                              ]

        breakCheck i = i `elem` [ELineBreak, ESoftBreak]

addEllipsis :: Widget a -> Widget a
addEllipsis w = B.Widget (B.hSize w) (B.vSize w) $ do
    ctx <- B.getContext
    let aw = ctx^.B.availWidthL
    result <- B.render w
    let withEllipsis = (B.hLimit (aw - 3) $ B.vLimit 1 $ (B.Widget B.Fixed B.Fixed $ return result)) <+>
                       B.str "..."
    if (V.imageHeight (result^.B.imageL) > 1) || (V.imageWidth (result^.B.imageL) == aw) then
        B.render withEllipsis else
        return result

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
        blank = Para (S.singleton ESpace)

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
blockToWidget (List _ l bs) = do
    w <- asks drawLineWidth
    lst <- blocksToList l bs
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

wrapInlines :: Seq Inline -> M (Widget a)
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

blocksToList :: ListType -> Seq Blocks -> M (Widget a)
blocksToList lt bs = do
    let is = case lt of
          Bullet _ -> repeat ("• ")
          Numbered Period s ->
            [ T.pack (show (n :: Int)) <> ". " | n <- [s..] ]
          Numbered Paren s ->
            [ T.pack (show (n :: Int)) <> ") " | n <- [s..] ]

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
            FText t                      -> if t == T.singleton (cursorSentinel)
                                            then B.visible $ B.txt " "
                                            else textWithCursor t

            FSpace                       -> B.txt " "
            FUser u                      -> colorUsername curUser u $ userSigil <> u
            FChannel c                   -> B.withDefAttr channelNameAttr $
                                            B.txt $ normalChannelSigil <> c
            FEditSentinel recent         -> let attr = if recent
                                                       then editedRecentlyMarkingAttr
                                                       else editedMarkingAttr
                                            in B.withDefAttr attr $ B.txt editMarking
            FEmoji em                    -> B.withDefAttr emojiAttr $
                                            B.txt $ ":" <> em <> ":"

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
