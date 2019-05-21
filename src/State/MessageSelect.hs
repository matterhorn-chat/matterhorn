module State.MessageSelect
  (
  -- * Message selection mode
    beginMessageSelect
  , flagSelectedMessage
  , viewSelectedMessage
  , fillSelectedGap
  , yankSelectedMessageVerbatim
  , yankSelectedMessage
  , openSelectedMessageURLs
  , beginConfirmDeleteSelectedMessage
  , messageSelectUp
  , messageSelectUpBy
  , messageSelectDown
  , messageSelectDownBy
  , messageSelectFirst
  , messageSelectLast
  , deleteSelectedMessage
  , beginReplyCompose
  , beginEditMessage
  , flagMessage
  , getSelectedMessage
  )
where

import           Prelude ()
import           Prelude.MH

import           Brick ( invalidateCacheEntry )
import           Brick.Main ( viewportScroll, hScrollToBeginning
                            , vScrollToBeginning )
import           Brick.Widgets.Edit ( applyEdit )
import           Data.Text.Zipper ( clearZipper, insertMany )
import           Lens.Micro.Platform

import qualified Network.Mattermost.Endpoints as MM
import           Network.Mattermost.Types

import           Clipboard ( copyToClipboard )
import           Markdown ( findVerbatimChunk )
import           State.Common
import           State.Messages
import           Types
import           Types.Common


getSelectedMessage :: ChatState -> Maybe Message
getSelectedMessage st
    | not (appMode st `elem` [MessageSelect, MessageSelectDeleteConfirm, ReactionEmojiListOverlay]) = Nothing
    | otherwise = do
        selMsgId <- selectMessageId $ st^.csMessageSelect
        let chanMsgs = st ^. csCurrentChannel . ccContents . cdMessages
        findMessage selMsgId chanMsgs

beginMessageSelect :: MH ()
beginMessageSelect = do
    -- Get the number of messages in the current channel and set the
    -- currently selected message index to be the most recently received
    -- message that corresponds to a Post (i.e. exclude informative
    -- messages).
    --
    -- If we can't find one at all, we ignore the mode switch request
    -- and just return.
    chanMsgs <- use(csCurrentChannel . ccContents . cdMessages)
    let recentMsg = getLatestSelectableMessage chanMsgs

    when (isJust recentMsg) $ do
        setMode MessageSelect
        csMessageSelect .= MessageSelectState (recentMsg >>= _mMessageId)

-- | Tell the server that the message we currently have selected
-- should have its flagged state toggled.
flagSelectedMessage :: MH ()
flagSelectedMessage = do
  selected <- use (to getSelectedMessage)
  case selected of
    Just msg
      | isFlaggable msg, Just pId <- messagePostId msg ->
        flagMessage pId (not (msg^.mFlagged))
    _        -> return ()

viewSelectedMessage :: MH ()
viewSelectedMessage = do
  selected <- use (to getSelectedMessage)
  case selected of
    Just msg
      | not (isGap msg) -> viewMessage msg
    _        -> return ()

fillSelectedGap :: MH ()
fillSelectedGap = do
  selected <- use (to getSelectedMessage)
  case selected of
    Just msg
      | isGap msg -> do cId <- use csCurrentChannelId
                        asyncFetchMessagesForGap cId msg
    _        -> return ()

viewMessage :: Message -> MH ()
viewMessage m = do
    csViewedMessage .= Just m
    let vs = viewportScroll ViewMessageArea
    mh $ do
        vScrollToBeginning vs
        hScrollToBeginning vs
        invalidateCacheEntry ViewMessageArea
    setMode ViewMessage

yankSelectedMessageVerbatim :: MH ()
yankSelectedMessageVerbatim = do
    selectedMessage <- use (to getSelectedMessage)
    case selectedMessage of
        Nothing -> return ()
        Just m -> do
            setMode Main
            case findVerbatimChunk (m^.mText) of
                Just txt -> copyToClipboard txt
                Nothing  -> return ()

yankSelectedMessage :: MH ()
yankSelectedMessage = do
    selectedMessage <- use (to getSelectedMessage)
    case selectedMessage of
        Nothing -> return ()
        Just m -> do
            setMode Main
            copyToClipboard $ m^.mMarkdownSource

openSelectedMessageURLs :: MH ()
openSelectedMessageURLs = whenMode MessageSelect $ do
    mCurMsg <- use (to getSelectedMessage)
    curMsg <- case mCurMsg of
        Nothing -> error "BUG: openSelectedMessageURLs: no selected message available"
        Just m -> return m

    let urls = msgURLs curMsg
    when (not (null urls)) $ do
        openedAll <- and <$> mapM (openURL . OpenLinkChoice) urls
        case openedAll of
            True -> setMode Main
            False ->
                mhError $ ConfigOptionMissing "urlOpenCommand"

beginConfirmDeleteSelectedMessage :: MH ()
beginConfirmDeleteSelectedMessage = do
    st <- use id
    selected <- use (to getSelectedMessage)
    case selected of
        Just msg | isDeletable msg && isMine st msg ->
            setMode MessageSelectDeleteConfirm
        _ -> return ()

messageSelectUp :: MH ()
messageSelectUp = do
    mode <- gets appMode
    selected <- use (csMessageSelect.to selectMessageId)
    case selected of
        Just _ | mode == MessageSelect -> do
            chanMsgs <- use (csCurrentChannel.ccContents.cdMessages)
            let nextMsgId = getPrevMessageId selected chanMsgs
            csMessageSelect .= MessageSelectState (nextMsgId <|> selected)
        _ -> return ()

messageSelectDown :: MH ()
messageSelectDown = do
    selected <- use (csMessageSelect.to selectMessageId)
    case selected of
        Just _ -> whenMode MessageSelect $ do
            chanMsgs <- use (csCurrentChannel.ccContents.cdMessages)
            let nextMsgId = getNextMessageId selected chanMsgs
            csMessageSelect .= MessageSelectState (nextMsgId <|> selected)
        _ -> return ()

messageSelectDownBy :: Int -> MH ()
messageSelectDownBy amt
    | amt <= 0 = return ()
    | otherwise =
        messageSelectDown >> messageSelectDownBy (amt - 1)

messageSelectUpBy :: Int -> MH ()
messageSelectUpBy amt
    | amt <= 0 = return ()
    | otherwise =
      messageSelectUp >> messageSelectUpBy (amt - 1)

messageSelectFirst :: MH ()
messageSelectFirst = do
    selected <- use (csMessageSelect.to selectMessageId)
    case selected of
        Just _ -> whenMode MessageSelect $ do
            chanMsgs <- use (csCurrentChannel.ccContents.cdMessages)
            case getEarliestSelectableMessage chanMsgs of
              Just firstMsg ->
                csMessageSelect .= MessageSelectState (firstMsg^.mMessageId <|> selected)
              Nothing -> mhLog LogError "No first message found from current message?!"
        _ -> return ()

messageSelectLast :: MH ()
messageSelectLast = do
    selected <- use (csMessageSelect.to selectMessageId)
    case selected of
        Just _ -> whenMode MessageSelect $ do
            chanMsgs <- use (csCurrentChannel.ccContents.cdMessages)
            case getLatestSelectableMessage chanMsgs of
              Just lastSelMsg ->
                csMessageSelect .= MessageSelectState (lastSelMsg^.mMessageId <|> selected)
              Nothing -> mhLog LogError "No last message found from current message?!"
        _ -> return ()

deleteSelectedMessage :: MH ()
deleteSelectedMessage = do
    selectedMessage <- use (to getSelectedMessage)
    st <- use id
    cId <- use csCurrentChannelId
    case selectedMessage of
        Just msg | isMine st msg && isDeletable msg ->
            case msg^.mOriginalPost of
              Just p ->
                  doAsyncChannelMM Preempt cId
                      (\s _ _ -> MM.mmDeletePost (postId p) s)
                      (\_ _ -> Just $ do
                          csEditState.cedEditMode .= NewPost
                          setMode Main)
              Nothing -> return ()
        _ -> return ()

beginReplyCompose :: MH ()
beginReplyCompose = do
    selected <- use (to getSelectedMessage)
    case selected of
        Just msg | isReplyable msg -> do
            let Just p = msg^.mOriginalPost
            setMode Main
            csEditState.cedEditMode .= Replying msg p
        _ -> return ()

beginEditMessage :: MH ()
beginEditMessage = do
    selected <- use (to getSelectedMessage)
    st <- use id
    case selected of
        Just msg | isMine st msg && isEditable msg -> do
            let Just p = msg^.mOriginalPost
            setMode Main
            csEditState.cedEditMode .= Editing p (msg^.mType)
            -- If the post that we're editing is an emote, we need
            -- to strip the formatting because that's only there to
            -- indicate that the post is an emote. This is annoying and
            -- can go away one day when there is an actual post type
            -- value of "emote" that we can look at. Note that the
            -- removed formatting needs to be reinstated just prior to
            -- issuing the API call to update the post.
            let sanitized = sanitizeUserText $ postMessage p
            let toEdit = if isEmote msg
                         then removeEmoteFormatting sanitized
                         else sanitized
            csEditState.cedEditor %= applyEdit (clearZipper >> (insertMany toEdit))
        _ -> return ()

-- | Tell the server that we have flagged or unflagged a message.
flagMessage :: PostId -> Bool -> MH ()
flagMessage pId f = do
  session <- getSession
  myId <- gets myUserId
  doAsyncWith Normal $ do
    let doFlag = if f then MM.mmFlagPost else MM.mmUnflagPost
    doFlag myId pId session
    return Nothing
