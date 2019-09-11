{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf       #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE ViewPatterns     #-}

module Markdown
  ( MessageData(..)
  , renderMessage
  , renderText
  , renderText'
  , cursorSentinel
  , addEllipsis
  , findVerbatimChunk
  )
where

import           Prelude ()
import           Prelude.MH

import           Brick ( (<+>), Widget, textWidth, hLimit, imageL, raw, render, Size(..), Widget(..) )
import qualified Brick as B
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Skylighting as BS
import qualified Cheapskate as C
import           Cheapskate.Types ( Block
                                  , Blocks
                                  , Inlines
                                  , ListType
                                  )
import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map
import           Data.Monoid (First(..))
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

import           Themes
import           Types ( Name, HighlightSet(..), userSigil, normalChannelSigil )
import           Types.Messages
import           Types.Posts
import           Types.UserNames ( isNameFragment, takeWhileNameFragment )


emptyHSet :: HighlightSet
emptyHSet = HighlightSet Set.empty Set.empty mempty

omittedUsernameType :: MessageType -> Bool
omittedUsernameType = \case
  CP Join -> True
  CP Leave -> True
  CP TopicChange -> True
  _ -> False


-- The special string we use to indicate the placement of a styled
-- indication that a message has been edited.
editMarkingSentinel :: Text
editMarkingSentinel = "#__mh_edit"

-- The special string we use to indicate the placement of a styled
-- indication that a message has been edited recently.
editRecentlyMarkingSentinel :: Text
editRecentlyMarkingSentinel = "#__mh_edit_r"

-- The actual user-facing text that we render in place of the edit
-- marking sentinel.
editMarking :: Text
editMarking = "(edited)"

-- Add the edit sentinel to the end of the last block in the sequence.
-- If the last block is a paragraph, append it to that paragraph.
-- Otherwise, append a new block so it appears beneath the last
-- block-level element.
addEditSentinel :: Text -> Blocks -> Blocks
addEditSentinel s bs =
    case viewr bs of
        EmptyR -> bs
        (rest :> b) -> rest <> appendEditSentinel s b

appendEditSentinel :: Text -> Block -> Blocks
appendEditSentinel sentinel b =
    let s = C.Para (S.singleton m)
        m = C.Str sentinel
    in case b of
        C.Para is -> S.singleton $ C.Para (is |> C.Str " " |> m)
        _ -> S.fromList [b, s]

-- | A bundled structure that includes all the information necessary
-- to render a given message
data MessageData = MessageData
  { mdEditThreshold     :: Maybe ServerTime
  , mdShowOlderEdits    :: Bool
  , mdShowReactions     :: Bool
  , mdMessage           :: Message
  , mdUserName          :: Maybe Text
  , mdIsBot             :: Bool
  , mdParentMessage     :: Maybe Message
  , mdParentUserName    :: Maybe Text
  , mdThreadState       :: ThreadState
  , mdRenderReplyParent :: Bool
  , mdHighlightSet      :: HighlightSet
  , mdIndentBlocks      :: Bool
  , mdMessageWidthLimit :: Maybe Int
  }

-- | renderMessage performs markdown rendering of the specified message.
--
-- The 'mdEditThreshold' argument specifies a time boundary where
-- "edited" markers are not shown for any messages older than this
-- mark (under the presumption that they are distracting for really
-- old stuff).  The mdEditThreshold will be None if there is no
-- boundary known yet; the boundary is typically set to the "new"
-- message boundary.
--
-- The 'mdShowOlderEdits' argument is a value read from the user's
-- configuration file that indicates that "edited" markers should be
-- shown for old messages (i.e., ignore the mdEditThreshold value).
renderMessage :: MessageData -> Widget Name
renderMessage md@MessageData { mdMessage = msg, .. } =
    let msgUsr = case mdUserName of
          Just u -> if omittedUsernameType (msg^.mType) then Nothing else Just u
          Nothing -> Nothing
        botElem = if mdIsBot then B.txt "[BOT]" else B.emptyWidget
        nameElems = case msgUsr of
          Just un
            | isEmote msg ->
                [ B.txt "*", colorUsername un un
                , botElem
                , B.txt " "
                ]
            | msg^.mFlagged ->
                [ colorUsername un un
                , botElem
                , B.txt "[!]: "
                ]
            | otherwise ->
                [ colorUsername un un
                , botElem
                , B.txt ": "
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
                        addEditSentinel editRecentlyMarkingSentinel bs
                    _ -> if mdShowOlderEdits
                         then addEditSentinel editMarkingSentinel bs
                         else bs
                else bs

        augmentedText = maybeAugment $ msg^.mText
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
        layout :: HighlightSet -> Maybe Int -> [Widget Name] -> Blocks -> ViewL Block -> Widget Name
        layout hs w nameElems bs xs | length xs > 1     = multiLnLayout hs w nameElems bs
        layout hs w nameElems bs (C.Blockquote {} :< _) = multiLnLayout hs w nameElems bs
        layout hs w nameElems bs (C.CodeBlock {} :< _)  = multiLnLayout hs w nameElems bs
        layout hs w nameElems bs (C.HtmlBlock {} :< _)  = multiLnLayout hs w nameElems bs
        layout hs w nameElems bs (C.List {} :< _)       = multiLnLayout hs w nameElems bs
        layout hs w nameElems bs (C.Para inlns :< _)
            | F.any breakCheck inlns                    = multiLnLayout hs w nameElems bs
        layout hs w nameElems bs _                      = nameNextToMessage hs w nameElems bs

        multiLnLayout hs w nameElems bs =
            if mdIndentBlocks
               then vBox [ hBox nameElems
                         , hBox [B.txt "  ", renderMarkdown hs ((subtract 2) <$> w) bs]
                         ]
               else nameNextToMessage hs w nameElems bs

        nameNextToMessage hs w nameElems bs =
            Widget Fixed Fixed $ do
                nameResult <- render $ hBox nameElems
                let newW = subtract (V.imageWidth (nameResult^.imageL)) <$> w
                render $ hBox [raw (nameResult^.imageL), renderMarkdown hs newW bs]

        breakCheck C.LineBreak = True
        breakCheck C.SoftBreak = True
        breakCheck _ = False

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
renderMarkdown :: HighlightSet -> Maybe Int -> Blocks -> Widget a
renderMarkdown hSet w =
  B.vBox . toList . fmap (blockToWidget hSet w) . addBlankLines

-- Add blank lines only between adjacent elements of the same type, to
-- save space
addBlankLines :: Seq Block -> Seq Block
addBlankLines = go' . viewl
  where go' EmptyL = S.empty
        go' (x :< xs) = go x (viewl xs)
        go a@C.Para {} (b@C.Para {} :< rs) =
             a <| blank <| go b (viewl rs)
        go a@C.Header {} (b@C.Header {} :< rs) =
             a <| blank <| go b (viewl rs)
        go a@C.Blockquote {} (b@C.Blockquote {} :< rs) =
             a <| blank <| go b (viewl rs)
        go a@C.List {} (b@C.List {} :< rs) =
             a <| blank <| go b (viewl rs)
        go a@C.CodeBlock {} (b@C.CodeBlock {} :< rs) =
             a <| blank <| go b (viewl rs)
        go a@C.HtmlBlock {} (b@C.HtmlBlock {} :< rs) =
             a <| blank <| go b (viewl rs)
        go x (y :< rs) = x <| go y (viewl rs)
        go x (EmptyL) = S.singleton x
        blank = C.Para (S.singleton (C.Str " "))

-- Render text to markdown without username highlighting
renderText :: Text -> Widget a
renderText txt = renderText' emptyHSet txt

renderText' :: HighlightSet -> Text -> Widget a
renderText' hSet txt = renderMarkdown hSet Nothing bs
  where C.Doc _ bs = C.markdown C.def txt

vBox :: F.Foldable f => f (Widget a) -> Widget a
vBox = B.vBox . toList

hBox :: F.Foldable f => f (Widget a) -> Widget a
hBox = B.hBox . toList

header :: Int -> Widget a
header n = B.txt (T.replicate n "#")

maybeHLimit :: Maybe Int -> Widget a -> Widget a
maybeHLimit Nothing w = w
maybeHLimit (Just i) w = hLimit i w

blockToWidget :: HighlightSet -> Maybe Int -> Block -> Widget a
blockToWidget hSet w (C.Para is) =
    maybeHLimit w $ toInlineChunk is hSet
blockToWidget hSet w (C.Header n is) =
    maybeHLimit w $
    B.withDefAttr clientHeaderAttr $
      hBox [B.padRight (B.Pad 1) $ header n, toInlineChunk is hSet]
blockToWidget hSet w (C.Blockquote is) =
    maybeHLimit w $
    addQuoting (vBox $ fmap (blockToWidget hSet w) is)
blockToWidget hSet w (C.List _ l bs) =
    maybeHLimit w $
    blocksToList l w bs hSet
blockToWidget hSet _ (C.CodeBlock ci tx) =
    let f = maybe rawCodeBlockToWidget (codeBlockToWidget (hSyntaxMap hSet)) mSyntax
        mSyntax = Sky.lookupSyntax (C.codeLang ci) (hSyntaxMap hSet)
    in f tx
blockToWidget _ w (C.HtmlBlock txt) =
    maybeHLimit w $
    textWithCursor txt
blockToWidget _ w (C.HRule) =
    maybeHLimit w $
    B.vLimit 1 (B.fill '*')

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

codeBlockToWidget :: Sky.SyntaxMap -> Sky.Syntax -> Text -> Widget a
codeBlockToWidget syntaxMap syntax tx =
    let result = Sky.tokenize cfg syntax tx
        cfg = Sky.TokenizerConfig syntaxMap False
    in case result of
        Left _ -> rawCodeBlockToWidget tx
        Right tokLines ->
            let padding = B.padLeftRight 1 (B.vLimit (length tokLines) B.vBorder)
            in (B.txt $ "[" <> Sky.sName syntax <> "]") B.<=>
               (padding <+> BS.renderRawSource textWithCursor tokLines)

rawCodeBlockToWidget :: Text -> Widget a
rawCodeBlockToWidget tx =
    B.withDefAttr codeAttr $
        let padding = B.padLeftRight 1 (B.vLimit (length theLines) B.vBorder)
            theLines = expandEmpty <$> T.lines tx
            expandEmpty "" = " "
            expandEmpty s  = s
        in padding <+> (B.vBox $ textWithCursor <$> theLines)

toInlineChunk :: Inlines -> HighlightSet -> Widget a
toInlineChunk is hSet = B.Widget B.Fixed B.Fixed $ do
  ctx <- B.getContext
  let width = ctx^.B.availWidthL
      fs    = toFragments hSet is
      ws    = fmap gatherWidgets (split width hSet fs)
  B.render (vBox (fmap hBox ws))

blocksToList :: ListType -> Maybe Int -> [Blocks] -> HighlightSet -> Widget a
blocksToList lt w bs hSet = vBox
  [ B.txt i <+> (vBox (fmap (blockToWidget hSet w) b))
  | b <- bs | i <- is ]
  where is = case lt of
          C.Bullet _ -> repeat ("• ")
          C.Numbered C.PeriodFollowing s ->
            [ T.pack (show (n :: Int)) <> ". " | n <- [s..] ]
          C.Numbered C.ParenFollowing s ->
            [ T.pack (show (n :: Int)) <> ") " | n <- [s..] ]

-- We want to do word-wrapping, but for that we want a linear
-- sequence of chunks we can break up. The typical Markdown
-- format doesn't fit the bill: when it comes to bold or italic
-- bits, we'd have treat it all as one. This representation is
-- more amenable to splitting up those bits.
data Fragment = Fragment
  { fTextual :: TextFragment
  , _fStyle  :: FragmentStyle
  } deriving (Show, Eq)

data TextFragment
  = TStr Text
  | TSpace
  | TSoftBreak
  | TLineBreak
  | TLink Text
  | TRawHtml Text
  | TEditSentinel
  | TEditRecentlySentinel
  | TComplex (Seq Fragment)
    deriving (Show, Eq)

data FragmentStyle
  = Normal
  | Emph
  | Strong
  | Code
  | User Text
  | Link Text
  | Emoji
  | Channel
  | Edited
  | EditedRecently
    deriving (Eq, Show)

unsafeGetStr :: C.Inline -> Text
unsafeGetStr (C.Str t) = t
unsafeGetStr _ = error "BUG: unsafeGetStr called on non-Str Inline"

toFragments :: HighlightSet -> Inlines -> Seq Fragment
toFragments hSet = go Normal
  where go n c = case viewl c of
          C.Str t :< xs | t == editMarkingSentinel ->
            Fragment TEditSentinel Edited <| go n xs
          C.Str t :< xs | t == editRecentlyMarkingSentinel ->
            Fragment TEditRecentlySentinel EditedRecently <| go n xs
          C.Str t :< xs | userSigil `T.isPrefixOf` t ->
            let (uFrags, rest) = takeWhileNameFragment $ F.toList xs
                t' = T.concat $ t : (unsafeGetStr <$> F.toList uFrags)
                u = T.drop 1 t'
                sty = if u `Set.member` (hUserSet hSet)
                      then User u
                      else n
            in Fragment (TStr t') sty <| go n (S.fromList rest)
          C.Str t :< xs | normalChannelSigil `T.isPrefixOf` t ->
            let (cFrags, rest) = S.spanl isNameFragment xs
                cn = T.concat $ t : (unsafeGetStr <$> F.toList cFrags)
                sty = if cn `Set.member` (hChannelSet hSet)
                      then Channel
                      else n
            in Fragment (TStr cn) sty <| go n rest
          C.Str t :< xs ->
            Fragment (TStr t) n <| go n xs
          C.Space :< xs ->
            Fragment TSpace n <| go n xs
          C.SoftBreak :< xs ->
            Fragment TSoftBreak n <| go n xs
          C.LineBreak :< xs ->
            Fragment TLineBreak n <| go n xs
          C.Link label url _ :< xs ->
            case toList label of
              [C.Str s] | s == url -> Fragment (TLink url) (Link url) <| go n xs
              _                    -> Fragment (TComplex $ toFragments hSet label) (Link url) <| go n xs
          C.RawHtml t :< xs ->
            Fragment (TRawHtml t) n <| go n xs
          C.Code t :< xs ->
            let ts  = [ Fragment frag Code
                      | wd <- T.split (== ' ') t
                      , frag <- case wd of
                          "" -> [TSpace]
                          _  -> [TSpace, TStr wd]
                      ]
                ts' = case ts of
                  (Fragment TSpace _:rs) -> rs
                  _                      -> ts
            in S.fromList ts' <> go n xs
          C.Emph is :< xs ->
            go Emph is <> go n xs
          C.Strong is :< xs ->
            go Strong is <> go n xs
          C.Image altIs url _ :< xs ->
            Fragment (TStr ("[image" <> altInlinesString altIs <> "]")) (Link url) <| go n xs
          C.Entity t :< xs ->
            Fragment (TStr t) (Link t) <| go n xs
          EmptyL -> S.empty

data SplitState = SplitState
  { splitChunks  :: Seq (Seq Fragment)
  , splitCurrCol :: Int
  }

separate :: HighlightSet -> Seq Fragment -> Seq Fragment
separate hSet sq = case viewl sq of
  Fragment (TStr s) n :< xs -> gatherStrings s n xs
  Fragment x n :< xs        -> Fragment x n <| separate hSet xs
  EmptyL                    -> S.empty
  where HighlightSet { hUserSet = uSet, hChannelSet = cSet } = hSet
        gatherStrings s n rs =
          let s' = removeCursor s
          in case viewl rs of
            _ | (userSigil `T.isPrefixOf` s' && (T.toLower (T.drop 1 s') `Set.member` uSet)) ->
                buildString s n <| separate hSet rs
            _ | (normalChannelSigil `T.isPrefixOf` s' && (T.drop 1 s' `Set.member` cSet)) ->
                buildString s n <| separate hSet rs
            Fragment (TStr s'') _ :< _
              | s'' == userSigil || s'' == normalChannelSigil ->
                buildString s n <| separate hSet rs
            Fragment (TStr s'') n' :< xs
              | n == n' -> gatherStrings (s <> s'') n xs
            Fragment _ _ :< _ -> buildString s n <| separate hSet rs
            EmptyL -> S.singleton (buildString s n)
        buildString s n =
            let s' = removeCursor s
            in if | n == Code -> Fragment (TStr s) n
                  | ":" `T.isPrefixOf` s' &&
                    ":" `T.isSuffixOf` s' &&
                    textWidth s' > 2 ->
                      Fragment (TStr s) Emoji
                  | Just uname <- userSigil `T.stripPrefix` removeCursor s
                    , removeCursor (T.toLower uname) `Set.member` uSet ->
                      Fragment (TStr s) (User uname)
                  | normalChannelSigil `T.isPrefixOf` (removeCursor s) &&
                    (T.drop 1 (removeCursor s) `Set.member` cSet) ->
                      Fragment (TStr s) Channel
                  | otherwise -> Fragment (TStr s) n

removeCursor :: Text -> Text
removeCursor = T.filter (/= cursorSentinel)

split :: Int -> HighlightSet -> Seq Fragment -> Seq (Seq Fragment)
split maxCols hSet = splitChunks
                          . go (SplitState (S.singleton S.empty) 0)
                          . separate hSet
  where go st (viewl-> f :< fs) = go st' fs
          where st' =
                  if | fTextual f == TSoftBreak || fTextual f == TLineBreak ->
                         st { splitChunks = splitChunks st |> S.empty
                            , splitCurrCol = 0
                            }
                     | available >= fsize ->
                         st { splitChunks  = addFragment f (splitChunks st)
                            , splitCurrCol = splitCurrCol st + fsize
                            }
                     | fTextual f == TSpace ->
                         st { splitChunks = splitChunks st |> S.empty
                            , splitCurrCol = 0
                            }
                     | otherwise ->
                         st { splitChunks  = splitChunks st |> S.singleton f
                            , splitCurrCol = fsize
                            }
                available = maxCols - splitCurrCol st
                fsize = fragmentSize f
                addFragment x (viewr-> ls :> l) = ( ls |> (l |> x))
                addFragment _ _ = error "[unreachable]"
        go st _                 = st

fragmentSize :: Fragment -> Int
fragmentSize f = case fTextual f of
  TStr t     -> textWidth t
  TLink t    -> textWidth t
  TRawHtml t -> textWidth t
  TEditSentinel -> textWidth editMarking
  TEditRecentlySentinel -> textWidth editMarking
  TSpace     -> 1
  TLineBreak -> 0
  TSoftBreak -> 0
  TComplex fs -> sum $ fragmentSize <$> fs

strOf :: TextFragment -> Text
strOf f = case f of
  TStr t     -> t
  TLink t    -> t
  TRawHtml t -> t
  TEditSentinel -> editMarking
  TEditRecentlySentinel -> editMarking
  TSpace     -> " "
  TComplex fs -> T.concat $ F.toList $ (strOf . fTextual) <$> fs
  _          -> ""

-- This finds adjacent string-ey fragments and concats them, so
-- we can use fewer widgets
gatherWidgets :: Seq Fragment -> Seq (Widget a)
gatherWidgets (viewl-> (Fragment frag style :< rs)) = go style (strOf frag) rs
  where go s t (viewl-> (Fragment f s' :< xs))
          | s == s' = go s (t <> strOf f) xs
        go s t xs =
          let rawText = B.txt (removeCursor t)
              rawWidget = case s of
                Normal -> rawText
                Emph   -> B.withDefAttr clientEmphAttr rawText
                Edited -> B.withDefAttr editedMarkingAttr $ B.txt t
                EditedRecently -> B.withDefAttr editedRecentlyMarkingAttr $ B.txt t
                Strong -> B.withDefAttr clientStrongAttr rawText
                Code   -> B.withDefAttr codeAttr rawText
                Link l ->
                    B.hyperlink l $ B.withDefAttr urlAttr $ case frag of
                        TComplex fs ->
                            hBox $ gatherWidgets fs
                        _ ->
                            rawText
                Emoji  -> B.withDefAttr emojiAttr rawText
                User u -> colorUsername u t
                Channel -> B.withDefAttr channelNameAttr rawText
              widget
                | T.any (== cursorSentinel) t = B.visible rawWidget
                | otherwise = rawWidget
          in widget <| gatherWidgets xs
gatherWidgets _ =
  S.empty

textWithCursor :: Text -> Widget a
textWithCursor t
    | T.any (== cursorSentinel) t = B.visible $ B.txt $ removeCursor t
    | otherwise = B.txt t

findVerbatimChunk :: C.Blocks -> Maybe Text
findVerbatimChunk = getFirst . F.foldMap go
  where go (C.CodeBlock _ t) = First (Just t)
        go _                 = First Nothing
