{-# LANGUAGE MultiWayIf       #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE ViewPatterns     #-}

module Markdown
  ( MessageData(..)
  , renderMessage
  , renderText
  , renderText'
  , blockGetURLs
  , cursorSentinel
  , addEllipsis
  , replyArrow
  , findVerbatimChunk
  )
where

import Prelude ()
import Prelude.MH

import           Brick ( (<+>), Widget, textWidth )
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as B
import qualified Brick.Widgets.Skylighting as BS
import qualified Brick as B
import           Cheapskate.Types ( Block
                                  , Blocks
                                  , Inlines
                                  , ListType
                                  )
import qualified Cheapskate as C
import qualified Data.Text as T
import qualified Data.Foldable as F
import           Data.Monoid (First(..))
import           Data.Sequence ( ViewL(..)
                               , ViewR(..)
                               , (<|)
                               , (|>)
                               , viewl
                               , viewr)
import qualified Data.Sequence as S
import qualified Skylighting as Sky
import qualified Data.Set as Set
import qualified Graphics.Vty as V

import           Network.Mattermost.Lenses (postEditAtL, postCreateAtL)
import           Network.Mattermost.Types (ServerTime(..))
import           Themes
import           Types (HighlightSet(..), userSigil, normalChannelSigil)
import           Types.Posts
import           Types.Messages

emptyHSet :: HighlightSet
emptyHSet = HighlightSet Set.empty Set.empty

omitUsernameTypes :: [MessageType]
omitUsernameTypes =
    [ CP Join
    , CP Leave
    , CP TopicChange
    ]

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
  , mdMessage           :: Message
  , mdUserName          :: Maybe Text
  , mdParentMessage     :: Maybe Message
  , mdParentUserName    :: Maybe Text
  , mdRenderReplyParent :: Bool
  , mdHighlightSet      :: HighlightSet
  , mdIndentBlocks      :: Bool
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
renderMessage :: MessageData -> Widget a
renderMessage md@MessageData { mdMessage = msg, .. } =
    let msgUsr = case mdUserName of
          Just u
            | msg^.mType `elem` omitUsernameTypes -> Nothing
            | otherwise -> Just u
          Nothing -> Nothing
        nameElems = case msgUsr of
          Just un
            | msg^.mType == CP Emote ->
                [ B.txt "*", colorUsername un un
                , B.txt " "
                ]
            | msg^.mFlagged ->
                [ colorUsername un un
                , B.txt "[!]: "
                ]
            | otherwise ->
                [ colorUsername un un
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
        rmd = renderMarkdown mdHighlightSet augmentedText
        msgWidget =
            (layout nameElems rmd . viewl) augmentedText
        withParent p = (replyArrow <+> p) B.<=> msgWidget
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
        layout n m xs
            | length xs > 1               = multiLnLayout n m
        layout n m (C.Blockquote {} :< _) = multiLnLayout n m
        layout n m (C.CodeBlock {} :< _)  = multiLnLayout n m
        layout n m (C.HtmlBlock {} :< _)  = multiLnLayout n m
        layout n m (C.List {} :< _)       = multiLnLayout n m
        layout n m (C.Para inlns :< _)
            | F.any breakCheck inlns      = multiLnLayout n m
        layout n m _                      = hBox $ join [n, return m]
        multiLnLayout n m =
            if mdIndentBlocks
               then vBox [ hBox n
                         , hBox [B.txt "  ", m]
                         ]
               else hBox $ n <> [m]
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
renderMarkdown :: HighlightSet -> Blocks -> Widget a
renderMarkdown hSet =
  B.vBox . toList . fmap (blockToWidget hSet) . addBlankLines

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
renderText' hSet txt = renderMarkdown hSet bs
  where C.Doc _ bs = C.markdown C.def txt

vBox :: F.Foldable f => f (Widget a) -> Widget a
vBox = B.vBox . toList

hBox :: F.Foldable f => f (Widget a) -> Widget a
hBox = B.hBox . toList

--

-- class ToWidget t where
--   toWidget :: HighlightSet -> t -> Widget a

header :: Int -> Widget a
header n = B.txt (T.replicate n "#")

blockToWidget :: HighlightSet -> Block -> Widget a
blockToWidget hSet (C.Para is) = toInlineChunk is hSet
blockToWidget hSet (C.Header n is) =
    B.withDefAttr clientHeaderAttr $
      hBox [header n, B.txt " ", toInlineChunk is hSet]
blockToWidget hSet (C.Blockquote is) =
    addQuoting (vBox $ fmap (blockToWidget hSet) is)
blockToWidget hSet (C.List _ l bs) = blocksToList l bs hSet
blockToWidget _ (C.CodeBlock ci tx) =
      let f = maybe rawCodeBlockToWidget codeBlockToWidget mSyntax
          mSyntax = Sky.lookupSyntax (C.codeLang ci) Sky.defaultSyntaxMap
      in f tx
blockToWidget _ (C.HtmlBlock txt) = textWithCursor txt
blockToWidget _ (C.HRule) = B.vLimit 1 (B.fill '*')

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

codeBlockToWidget :: Sky.Syntax -> Text -> Widget a
codeBlockToWidget syntax tx =
    let result = Sky.tokenize cfg syntax tx
        cfg = Sky.TokenizerConfig Sky.defaultSyntaxMap False
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
      fs    = toFragments is
      ws    = fmap gatherWidgets (split width hSet fs)
  B.render (vBox (fmap hBox ws))

blocksToList :: ListType -> [Blocks] -> HighlightSet -> Widget a
blocksToList lt bs hSet = vBox
  [ B.txt i <+> (vBox (fmap (blockToWidget hSet) b))
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
  } deriving (Show)

data TextFragment
  = TStr Text
  | TSpace
  | TSoftBreak
  | TLineBreak
  | TLink Text
  | TRawHtml Text
  | TEditSentinel
  | TEditRecentlySentinel
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

-- We convert it pretty mechanically:
toFragments :: Inlines -> Seq Fragment
toFragments = go Normal
  where go n c = case viewl c of
          C.Str t :< xs | t == editMarkingSentinel ->
            Fragment TEditSentinel Edited <| go n xs
          C.Str t :< xs | t == editRecentlyMarkingSentinel ->
            Fragment TEditRecentlySentinel EditedRecently <| go n xs
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
              _                    -> go (Link url) label <> go n xs
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

--

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
            _ | (userSigil `T.isPrefixOf` s' && (T.drop 1 s' `Set.member` uSet)) ->
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
                    , removeCursor uname `Set.member` uSet ->
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

strOf :: TextFragment -> Text
strOf f = case f of
  TStr t     -> t
  TLink t    -> t
  TRawHtml t -> t
  TEditSentinel -> editMarking
  TEditRecentlySentinel -> editMarking
  TSpace     -> " "
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
                  B.modifyDefAttr (`V.withURL` l)
                    (B.withDefAttr urlAttr rawText)
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

inlinesText :: Seq C.Inline -> Text
inlinesText = F.fold . fmap go
  where go (C.Str t)       = t
        go C.Space         = " "
        go C.SoftBreak     = " "
        go C.LineBreak     = " "
        go (C.Emph is)     = F.fold (fmap go is)
        go (C.Strong is)   = F.fold (fmap go is)
        go (C.Code t)      = t
        go (C.Link is _ _) = F.fold (fmap go is)
        go (C.Image is _ _) = "[image" <> altInlinesString is <> "]"
        go (C.Entity t)    = t
        go (C.RawHtml t)   = t

altInlinesString :: Seq C.Inline -> Text
altInlinesString is | S.null is = ""
                    | otherwise = ":" <> inlinesText is

blockGetURLs :: C.Block -> Seq (Text, Text)
blockGetURLs (C.Para is) = mconcat $ inlineGetURLs <$> toList is
blockGetURLs (C.Header _ is) = mconcat $ inlineGetURLs <$> toList is
blockGetURLs (C.Blockquote bs) = mconcat $ blockGetURLs <$> toList bs
blockGetURLs (C.List _ _ bss) = mconcat $ mconcat $ (blockGetURLs <$>) <$> (toList <$> bss)
blockGetURLs _ = mempty

inlineGetURLs :: C.Inline -> Seq (Text, Text)
inlineGetURLs (C.Emph is) = mconcat $ inlineGetURLs <$> toList is
inlineGetURLs (C.Strong is) = mconcat $ inlineGetURLs <$> toList is
inlineGetURLs (C.Link is url "") = (url, inlinesText is) S.<| (mconcat $ inlineGetURLs <$> toList is)
inlineGetURLs (C.Link is _ url) = (url, inlinesText is) S.<| (mconcat $ inlineGetURLs <$> toList is)
inlineGetURLs (C.Image is url _) = S.singleton (url, inlinesText is)
inlineGetURLs _ = mempty

replyArrow :: Widget a
replyArrow =
    hBox [ B.str " "
         , B.borderElem B.bsCornerTL
         , B.str "▸"
         ]

findVerbatimChunk :: C.Blocks -> Maybe Text
findVerbatimChunk = getFirst . F.foldMap go
  where go (C.CodeBlock _ t) = First (Just t)
        go _                 = First Nothing
