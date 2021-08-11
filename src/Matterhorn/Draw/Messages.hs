{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
module Matterhorn.Draw.Messages
  ( MessageData(..)
  , renderMessage
  , nameForUserRef
  , renderSingleMessage
  , unsafeRenderMessageSelection
  , renderLastMessages
  , addEllipsis
  )
where

import           Brick
import           Brick.Widgets.Border
import           Control.Monad.Trans.Reader ( withReaderT )
import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as S
import           Data.Sequence ( ViewL(..)
                               , ViewR(..)
                               , (|>)
                               , viewl
                               , viewr)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Graphics.Vty as V
import           Lens.Micro.Platform ( (.~), to )
import           Network.Mattermost.Lenses ( postEditAtL, postCreateAtL )
import           Network.Mattermost.Types ( ServerTime(..), userUsername, postId )
import           Prelude ()
import           Matterhorn.Prelude

import           Matterhorn.Draw.Util
import           Matterhorn.Draw.RichText
import           Matterhorn.Themes
import           Matterhorn.Types
import           Matterhorn.Types.RichText
import           Matterhorn.Types.DirectionalSeq

maxMessageHeight :: Int
maxMessageHeight = 200

-- | nameForUserRef converts the UserRef into a printable name, based
-- on the current known user data.
nameForUserRef :: ChatState -> UserRef -> Maybe Text
nameForUserRef st uref =
    case uref of
        NoUser -> Nothing
        UserOverride _ t -> Just t
        UserI _ uId -> displayNameForUserId uId st

-- | renderSingleMessage is the main message drawing function.
--
-- The `ind` argument specifies an "indicator boundary".  Showing
-- various indicators (e.g. "edited") is not typically done for
-- messages that are older than this boundary value.
renderSingleMessage :: ChatState
                    -> HighlightSet
                    -> Maybe ServerTime
                    -> Message
                    -> ThreadState
                    -> Widget Name
renderSingleMessage st hs ind m threadState =
  renderChatMessage st hs ind threadState (withBrackets . renderTime st . withServerTime) m

renderChatMessage :: ChatState
                  -> HighlightSet
                  -> Maybe ServerTime
                  -> ThreadState
                  -> (ServerTime -> Widget Name)
                  -> Message
                  -> Widget Name
renderChatMessage st hs ind threadState renderTimeFunc msg =
    let showOlderEdits = configShowOlderEdits config
        showTimestamp = configShowMessageTimestamps config
        config = st^.csResources.crConfiguration
        parent = case msg^.mInReplyToMsg of
          NotAReply -> Nothing
          InReplyTo pId -> getMessageForPostId st pId
        m = renderMessage MessageData
              { mdMessage           = msg
              , mdUserName          = msg^.mUser.to (nameForUserRef st)
              , mdParentMessage     = parent
              , mdParentUserName    = parent >>= (^.mUser.to (nameForUserRef st))
              , mdEditThreshold     = ind
              , mdHighlightSet      = hs
              , mdShowOlderEdits    = showOlderEdits
              , mdRenderReplyParent = True
              , mdIndentBlocks      = True
              , mdThreadState       = threadState
              , mdShowReactions     = True
              , mdMessageWidthLimit = Nothing
              , mdMyUsername        = userUsername $ myUser st
              , mdWrapNonhighlightedCodeBlocks = True
              , mdTruncateVerbatimBlocks = st^.csVerbatimTruncateSetting
              }
        fullMsg =
          case msg^.mUser of
            NoUser
              | isGap msg -> withDefAttr gapMessageAttr m
              | otherwise ->
                case msg^.mType of
                    C DateTransition ->
                        withDefAttr dateTransitionAttr (hBorderWithLabel m)
                    C NewMessagesTransition ->
                        withDefAttr newMessageTransitionAttr (hBorderWithLabel m)
                    C Error ->
                        withDefAttr errorMessageAttr m
                    _ ->
                        withDefAttr clientMessageAttr m
            _ | isJoinLeave msg -> withDefAttr clientMessageAttr m
              | otherwise -> m
        maybeRenderTime w =
            if showTimestamp
            then let maybePadTime = if threadState == InThreadShowParent
                                    then (txt " " <=>) else id
                 in hBox [maybePadTime $ renderTimeFunc (msg^.mDate), txt " ", w]
            else w
        maybeRenderTimeWith f = if isTransition msg then id else f
    in maybeRenderTimeWith maybeRenderTime fullMsg

-- | Render a selected message with focus, including the messages
-- before and the messages after it. The foldable parameters exist
-- because (depending on the situation) we might use either of the
-- message list types for the 'before' and 'after' (i.e. the
-- chronological or retrograde message sequences).
unsafeRenderMessageSelection :: (SeqDirection dir1, SeqDirection dir2)
                             => ( (Message, ThreadState)
                                , ( DirectionalSeq dir1 (Message, ThreadState)
                                  , DirectionalSeq dir2 (Message, ThreadState)
                                  )
                                )
                             -> (Message -> ThreadState -> Widget Name)
                             -> Widget Name
unsafeRenderMessageSelection ((curMsg, curThreadState), (before, after)) doMsgRender =
  Widget Greedy Greedy $ do
    ctx <- getContext
    curMsgResult <- withReaderT relaxHeight $ render $
                    forceAttr messageSelectAttr $
                    padRight Max $ doMsgRender curMsg curThreadState

    let targetHeight = ctx^.availHeightL
        upperHeight = targetHeight `div` 2
        lowerHeight = targetHeight - upperHeight

    lowerHalfResults <- renderMessageSeq targetHeight (render1 doMsgRender) vLimit after
    upperHalfResults <- renderMessageSeq targetHeight (render1 doMsgRender) cropTopTo before

    let upperHalfResultsHeight = sum $ (V.imageHeight . image) <$> upperHalfResults
        lowerHalfResultsHeight = sum $ (V.imageHeight . image) <$> lowerHalfResults
        curHeight = V.imageHeight $ curMsgResult^.imageL
        uncropped = vBox $ fmap resultToWidget $
                           (reverse upperHalfResults) <> (curMsgResult : lowerHalfResults)

        cropTop h w = Widget Fixed Fixed $ do
            result <- withReaderT relaxHeight $ render w
            render $ cropTopTo h $ resultToWidget result
        cropBottom h w = Widget Fixed Fixed $ do
            result <- withReaderT relaxHeight $ render w
            render $ cropBottomTo h $ resultToWidget result

        lowerHalf = vBox $ fmap resultToWidget lowerHalfResults
        upperHalf = vBox $ fmap resultToWidget $ reverse upperHalfResults

    render $ if | lowerHalfResultsHeight < (lowerHeight - curHeight) ->
                    cropTop targetHeight uncropped
                | upperHalfResultsHeight < upperHeight ->
                    vLimit targetHeight uncropped
                | otherwise ->
                    cropTop upperHeight upperHalf <=> (resultToWidget curMsgResult) <=>
                       (if curHeight < lowerHeight
                         then cropBottom (lowerHeight - curHeight) lowerHalf
                         else cropBottom lowerHeight lowerHalf)

resultToWidget :: Result n -> Widget n
resultToWidget = Widget Fixed Fixed . return

renderMessageSeq :: (SeqDirection dir)
                 => Int
                 -> (Message -> ThreadState -> Widget Name)
                 -> (Int -> Widget Name -> Widget Name)
                 -> DirectionalSeq dir (Message, ThreadState)
                 -> RenderM Name [Result Name]
renderMessageSeq remainingHeight renderFunc limitFunc ms
    | messagesLength ms == 0 = return []
    | otherwise = do
        let Just (m, threadState) = messagesHead ms
            maybeCache = case m^.mMessageId of
                Nothing -> id
                Just i -> cached (RenderedMessage i)
        result <- render $ limitFunc remainingHeight $ maybeCache $ renderFunc m threadState
        rest <- renderMessageSeq (remainingHeight - (V.imageHeight $ result^.imageL)) renderFunc limitFunc (messagesDrop 1 ms)
        return $ result : rest

renderLastMessages :: ChatState
                   -> HighlightSet
                   -> Maybe ServerTime
                   -> DirectionalSeq Retrograde (Message, ThreadState)
                   -> Widget Name
renderLastMessages st hs editCutoff msgs =
    Widget Greedy Greedy $ do
        ctx <- getContext
        let targetHeight = ctx^.availHeightL
            doMsgRender = renderSingleMessage st hs editCutoff

            newMessagesTransitions = filterMessages (isNewMessagesTransition . fst) msgs
            newMessageTransition = fst <$> (listToMaybe $ F.toList newMessagesTransitions)

            isBelow m transition = m^.mDate > transition^.mDate

            go :: Int -> DirectionalSeq Retrograde (Message, ThreadState) -> RenderM Name [Result Name]
            go _ ms | messagesLength ms == 0 = return []
            go remainingHeight ms = do
                let Just (m, threadState) = messagesHead ms
                    newMessagesAbove = maybe False (isBelow m) newMessageTransition

                result <- render $ render1 doMsgRender m threadState

                croppedResult <- render $ cropTopTo remainingHeight $ resultToWidget result

                -- If the new message fills the window, check whether
                -- there is still a "New Messages" transition that is
                -- not displayed. If there is, then we need to replace
                -- the top line of the new image with a "New Messages"
                -- indicator.
                if V.imageHeight (result^.imageL) >= remainingHeight
                then do
                    single <- if newMessagesAbove
                              then do
                                  result' <- render $
                                      vBox [ withDefAttr newMessageTransitionAttr $ hBorderWithLabel (txt "New Messages ↑")
                                           , cropTopBy 1 $ resultToWidget croppedResult
                                           ]
                                  return result'
                              else do
                                  return croppedResult
                    return [single]
                else do
                    let unusedHeight = remainingHeight - V.imageHeight (result^.imageL)
                    rest <- go unusedHeight $ messagesDrop 1 ms
                    return $ result : rest

        results <- go targetHeight msgs
        render $ vBox $ (Widget Fixed Fixed . return) <$> reverse results

relaxHeight :: Context -> Context
relaxHeight c = c & availHeightL .~ (max maxMessageHeight (c^.availHeightL))

render1 :: (Message -> ThreadState -> Widget Name)
        -> Message
        -> ThreadState
        -> Widget Name
render1 doMsgRender msg threadState = case msg^.mDeleted of
    True -> emptyWidget
    False ->
        Widget Greedy Fixed $ do
            withReaderT relaxHeight $
                render $ padRight Max $
                doMsgRender msg threadState

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
                , mdTruncateVerbatimBlocks :: Maybe Int
                -- ^ At what height to truncate long verbatim/code blocks.
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
        botElem = if isBotMessage msg then txt "[BOT]" else emptyWidget
        mId = msg^.mMessageId
        clickableAuthor un = case mId of
            Nothing -> id
            -- We use the index (-1) since indexes for clickable
            -- usernames elsewhere in this message start at 0.
            Just i -> clickable (ClickableUsernameInMessage i (-1) un)
        nameElems = case msgUsr of
          Just un
            | isEmote msg ->
                [ withDefAttr pinnedMessageIndicatorAttr $ txt $ if msg^.mPinned then "[PIN]" else ""
                , txt $ (if msg^.mFlagged then "[!] " else "") <> "*"
                , clickableAuthor un $ colorUsername mdMyUsername un un
                , botElem
                , txt " "
                ]
            | otherwise ->
                [ withDefAttr pinnedMessageIndicatorAttr $ txt $ if msg^.mPinned then "[PIN] " else ""
                , clickableAuthor un $ colorUsername mdMyUsername un un
                , botElem
                , txt $ (if msg^.mFlagged then "[!]" else "") <> ": "
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

        augmentedText = unBlocks $ maybeAugment $ msg^.mText
        msgWidget =
            vBox $ (layout mdHighlightSet mdMessageWidthLimit nameElems augmentedText . viewl) augmentedText :
                   catMaybes [msgAtch, msgReac]
        replyIndent = Widget Fixed Fixed $ do
            ctx <- getContext
            -- NB: The amount subtracted here must be the total padding
            -- added below (pad 1 + vBorder)
            w <- render $ hLimit (ctx^.availWidthL - 2) msgWidget
            render $ vLimit (V.imageHeight $ w^.imageL) $
                padRight (Pad 1) vBorder <+> (Widget Fixed Fixed $ return w)
        msgAtch = if S.null (msg^.mAttachments)
          then Nothing
          else Just $ withDefAttr clientMessageAttr $ vBox
                 [ padLeft (Pad 2) $ clickable (ClickableAttachment (a^.attachmentFileId)) $
                                     txt ("[attached: `" <> a^.attachmentName <> "`]")
                 | a <- toList (msg^.mAttachments)
                 ]
        msgReac = if Map.null (msg^.mReactions) || (not mdShowReactions)
          then Nothing
          else let renderR e us lst =
                       let n = Set.size us
                       in if | n == 1    -> makeReactionWidget e us (" [" <> e <> "]") : lst
                             | otherwise -> makeReactionWidget e us (" [" <> e <> " " <> T.pack (show n) <> "]") : lst
                   nonEmptyReactions = Map.filter (not . Set.null) $ msg^.mReactions
                   makeReactionWidget e us t =
                       let w = txt t in
                       maybe w (flip clickable w) $ makeName e us
                   hasAnyReactions = not $ null nonEmptyReactions
                   makeName e us = do
                       pid <- postId <$> msg^.mOriginalPost
                       Just $ ClickableReactionInMessage pid e us
                   reactionWidget = Widget Fixed Fixed $ do
                       ctx <- getContext
                       let lineW = ctx^.availWidthL
                       reacs <- mapM render $ Map.foldrWithKey renderR [] nonEmptyReactions
                       let reacLines :: [Result n] -> Int -> [Result n] -> [[Result n]]
                           reacLines l _ []     = if null l then [] else [l]
                           reacLines l w (r:rs) =
                               let rW = V.imageWidth $ r^.imageL
                               in if rW <= w
                                  then reacLines (l <> [r]) (w - rW) rs
                                  else if rW > lineW
                                       then l : [r] : reacLines [] lineW rs
                                       else l : reacLines [] lineW (r:rs)

                       render $ vBox $ hBox <$> (fmap (fmap resultToWidget)) (reacLines [] lineW reacs)
               in if hasAnyReactions
                  then Just $ withDefAttr emojiAttr $ txt "   " <+> reactionWidget
                  else Nothing
        withParent p =
            case mdThreadState of
                NoThread -> msgWidget
                InThreadShowParent -> p <=> replyIndent
                InThread -> replyIndent
    in if not mdRenderReplyParent
       then msgWidget
       else case msg^.mInReplyToMsg of
          NotAReply -> msgWidget
          InReplyTo _ ->
              case mdParentMessage of
                  Nothing -> withParent (str "[loading...]")
                  Just pm ->
                      let parentMsg = renderMessage md
                            { mdShowOlderEdits    = False
                            , mdMessage           = pm
                            , mdUserName          = mdParentUserName
                            , mdParentMessage     = Nothing
                            , mdRenderReplyParent = False
                            , mdIndentBlocks      = False
                            }
                      in withParent (addEllipsis $ forceAttr replyParentAttr parentMsg)

    where
        layout :: HighlightSet -> Maybe Int -> [Widget Name] -> Seq Block
               -> ViewL Block -> Widget Name
        layout hs w nameElems bs xs | length xs > 1     = multiLnLayout hs w nameElems bs
        layout hs w nameElems bs (Blockquote {} :< _) = multiLnLayout hs w nameElems bs
        layout hs w nameElems bs (CodeBlock {} :< _)  = multiLnLayout hs w nameElems bs
        layout hs w nameElems bs (HTMLBlock {} :< _)  = multiLnLayout hs w nameElems bs
        layout hs w nameElems bs (List {} :< _)       = multiLnLayout hs w nameElems bs
        layout hs w nameElems bs (Para inlns :< _)
            | F.any breakCheck (unInlines inlns)      = multiLnLayout hs w nameElems bs
        layout hs w nameElems bs _                    = nameNextToMessage hs w nameElems bs

        multiLnLayout hs w nameElems bs =
            if mdIndentBlocks
               then vBox [ hBox nameElems
                         , hBox [txt "  ", renderRichText mdMyUsername hs ((subtract 2) <$> w)
                                                 mdWrapNonhighlightedCodeBlocks
                                                 mdTruncateVerbatimBlocks
                                                 (Just clickableNames) (Blocks bs)]
                         ]
               else nameNextToMessage hs w nameElems bs

        nameNextToMessage hs w nameElems bs =
            Widget Fixed Fixed $ do
                nameResult <- render $ hBox nameElems
                let newW = subtract (V.imageWidth (nameResult^.imageL)) <$> w
                render $ hBox [ Widget Fixed Fixed $ return nameResult
                              , renderRichText mdMyUsername hs newW
                                  mdWrapNonhighlightedCodeBlocks
                                  mdTruncateVerbatimBlocks
                                  (Just clickableNames) (Blocks bs)
                              ]

        breakCheck i = i `elem` [ELineBreak, ESoftBreak]

        clickableNames i (EHyperlink u _) =
            case msg^.mMessageId of
                Just mId -> Just $ ClickableURLInMessage mId i $ LinkURL u
                Nothing -> Nothing
        clickableNames i (EUser name) =
            case msg^.mMessageId of
                Just mId -> Just $ ClickableUsernameInMessage mId i name
                Nothing -> Nothing
        clickableNames _ _ = Nothing

-- Add the edit sentinel to the end of the last block in the sequence.
-- If the last block is a paragraph, append it to that paragraph.
-- Otherwise, append a new block so it appears beneath the last
-- block-level element.
addEditSentinel :: Inline -> Blocks -> Blocks
addEditSentinel d (Blocks bs) =
    case viewr bs of
        EmptyR -> Blocks bs
        (rest :> b) -> Blocks rest <> appendEditSentinel d b

appendEditSentinel :: Inline -> Block -> Blocks
appendEditSentinel sentinel b =
    let s = Para (Inlines $ S.singleton sentinel)
    in Blocks $ case b of
        Para is -> S.singleton $ Para (Inlines $ unInlines is |> ESpace |> sentinel)
        _ -> S.fromList [b, s]

omittedUsernameType :: MessageType -> Bool
omittedUsernameType = \case
    CP Join -> True
    CP Leave -> True
    CP TopicChange -> True
    _ -> False

addEllipsis :: Widget a -> Widget a
addEllipsis w = Widget (hSize w) (vSize w) $ do
    ctx <- getContext
    let aw = ctx^.availWidthL
    result <- render w
    let withEllipsis = (hLimit (aw - 3) $ vLimit 1 $ (Widget Fixed Fixed $ return result)) <+>
                       str "..."
    if (V.imageHeight (result^.imageL) > 1) || (V.imageWidth (result^.imageL) == aw) then
        render withEllipsis else
        return result
