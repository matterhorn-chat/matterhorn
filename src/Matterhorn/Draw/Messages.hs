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
import           Network.Mattermost.Types ( ServerTime(..), userUsername )
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
unsafeRenderMessageSelection :: (Foldable f, Foldable g)
                             => ((Message, ThreadState), (f (Message, ThreadState), g (Message, ThreadState)))
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

        lowerRender img (m, tState) = render1HLimit doMsgRender V.vertJoin targetHeight img tState m
        upperRender img (m, tState) = render1HLimit doMsgRender (flip V.vertJoin) targetHeight img tState m

    lowerHalf <- foldM lowerRender V.emptyImage after
    upperHalf <- foldM upperRender V.emptyImage before

    let curHeight = V.imageHeight $ curMsgResult^.imageL
        uncropped = upperHalf V.<-> curMsgResult^.imageL V.<-> lowerHalf
        img = if | V.imageHeight lowerHalf < (lowerHeight - curHeight) ->
                     V.cropTop targetHeight uncropped
                 | V.imageHeight upperHalf < upperHeight ->
                     V.cropBottom targetHeight uncropped
                 | otherwise ->
                     V.cropTop upperHeight upperHalf V.<-> curMsgResult^.imageL V.<->
                        (if curHeight < lowerHeight
                          then V.cropBottom (lowerHeight - curHeight) lowerHalf
                          else V.cropBottom lowerHeight lowerHalf)
    return $ emptyResult & imageL .~ img

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

            go :: V.Image -> DirectionalSeq Retrograde (Message, ThreadState) -> RenderM Name V.Image
            go img ms | messagesLength ms == 0 = return img
            go img ms = do
                let Just (m, threadState) = messagesHead ms
                    newMessagesAbove = maybe False (isBelow m) newMessageTransition
                newImg <- render1HLimit doMsgRender (flip V.vertJoin) targetHeight img threadState m
                -- If the new message fills the window, check whether
                -- there is still a "New Messages" transition that is
                -- not displayed. If there is, then we need to replace
                -- the top line of the new image with a "New Messages"
                -- indicator.
                if V.imageHeight newImg >= targetHeight && newMessagesAbove
                then do
                    transitionResult <- render $ withDefAttr newMessageTransitionAttr $
                                                 hBorderWithLabel (txt "New Messages ↑")
                    let newImg2 = V.vertJoin (transitionResult^.imageL)
                                               (V.cropTop (targetHeight - 1) newImg)
                    return newImg2
                else go newImg $ messagesDrop 1 ms

        img <- go V.emptyImage msgs
        return $ emptyResult & imageL .~ (V.cropTop targetHeight img)

relaxHeight :: Context -> Context
relaxHeight c = c & availHeightL .~ (max maxMessageHeight (c^.availHeightL))

render1HLimit :: (Message -> ThreadState -> Widget Name)
              -> (V.Image -> V.Image -> V.Image)
              -> Int
              -> V.Image
              -> ThreadState
              -> Message
              -> RenderM Name V.Image
render1HLimit doMsgRender fjoin lim img threadState msg
  | V.imageHeight img >= lim = return img
  | otherwise = fjoin img <$> render1 doMsgRender threadState msg

render1 :: (Message -> ThreadState -> Widget Name)
        -> ThreadState
        -> Message
        -> RenderM Name V.Image
render1 doMsgRender threadState msg = case msg^.mDeleted of
    True -> return V.emptyImage
    False -> do
        r <- withReaderT relaxHeight $
             render $ padRight Max $
             doMsgRender msg threadState
        return $ r^.imageL

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
        botElem = if isBotMessage msg then txt "[BOT]" else emptyWidget
        nameElems = case msgUsr of
          Just un
            | isEmote msg ->
                [ withDefAttr pinnedMessageIndicatorAttr $ txt $ if msg^.mPinned then "[PIN]" else ""
                , txt $ (if msg^.mFlagged then "[!] " else "") <> "*"
                , colorUsername mdMyUsername un un
                , botElem
                , txt " "
                ]
            | otherwise ->
                [ withDefAttr pinnedMessageIndicatorAttr $ txt $ if msg^.mPinned then "[PIN] " else ""
                , colorUsername mdMyUsername un un
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
                 [ txt ("  [attached: `" <> a^.attachmentName <> "`]")
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
               in Just $ withDefAttr emojiAttr $ txt ("   " <> reactionMsg)
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
                                                 mdWrapNonhighlightedCodeBlocks Nothing (Blocks bs)]
                         ]
               else nameNextToMessage hs w nameElems bs

        nameNextToMessage hs w nameElems bs =
            Widget Fixed Fixed $ do
                nameResult <- render $ hBox nameElems
                let newW = subtract (V.imageWidth (nameResult^.imageL)) <$> w
                render $ hBox [ raw (nameResult^.imageL)
                              , renderRichText mdMyUsername hs newW mdWrapNonhighlightedCodeBlocks Nothing (Blocks bs)
                              ]

        breakCheck i = i `elem` [ELineBreak, ESoftBreak]

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
