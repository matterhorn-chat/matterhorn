module Matterhorn.State.MessageSelect
  (
  -- * Message selection mode
    beginMessageSelect
  , flagSelectedMessage
  , pinSelectedMessage
  , viewSelectedMessage
  , fillSelectedGap
  , copyPostLink
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
import           Matterhorn.Prelude

import           Brick ( invalidateCache )
import           Brick.Widgets.Edit ( applyEdit )
import           Data.Text.Zipper ( clearZipper, insertMany )
import           Lens.Micro.Platform

import qualified Network.Mattermost.Endpoints as MM
import           Network.Mattermost.Types

import           Matterhorn.Clipboard ( copyToClipboard )
import           Matterhorn.State.Common
import           Matterhorn.State.Links
import           Matterhorn.State.Messages
import           Matterhorn.Types
import           Matterhorn.Types.RichText ( findVerbatimChunk, makePermalink )
import           Matterhorn.Types.Common
import           Matterhorn.Windows.ViewMessage


-- | In these modes, we allow access to the selected message state.
messageSelectCompatibleModes :: [Mode]
messageSelectCompatibleModes =
    [ MessageSelect
    , MessageSelectDeleteConfirm
    , ReactionEmojiListOverlay
    ]

getSelectedMessage :: ChatState -> Maybe Message
getSelectedMessage st
    | not (st^.csCurrentTeam.tsMode `elem` messageSelectCompatibleModes) = Nothing
    | otherwise = do
        selMsgId <- selectMessageId $ st^.csCurrentTeam.tsMessageSelect
        let chanMsgs = st ^. csCurrentChannel . ccContents . cdMessages
        findMessage selMsgId chanMsgs

beginMessageSelect :: MH ()
beginMessageSelect = do
    -- Invalidate the rendering cache since we cache messages to speed
    -- up the selection UI responsiveness. (See Draw.Messages for
    -- caching behavior.)
    mh invalidateCache

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
        csCurrentTeam.tsMessageSelect .= MessageSelectState (recentMsg >>= _mMessageId)

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

-- | Tell the server that the message we currently have selected
-- should have its pinned state toggled.
pinSelectedMessage :: MH ()
pinSelectedMessage = do
  selected <- use (to getSelectedMessage)
  case selected of
    Just msg
      | isPinnable msg, Just pId <- messagePostId msg ->
        pinMessage pId (not (msg^.mPinned))
    _ -> return ()

viewSelectedMessage :: MH ()
viewSelectedMessage = do
  selected <- use (to getSelectedMessage)
  case selected of
    Just msg
      | not (isGap msg) -> viewMessage msg
    _        -> return ()

copyPostLink :: MH ()
copyPostLink = do
  selected <- use (to getSelectedMessage)
  case selected of
    Just msg | isPostMessage msg -> do
        tId <- use csCurrentTeamId
        baseUrl <- getServerBaseUrl tId
        let Just pId = messageIdPostId =<< _mMessageId msg
        copyToClipboard $ makePermalink baseUrl pId
        setMode Main
    _ -> return ()

fillSelectedGap :: MH ()
fillSelectedGap = do
  selected <- use (to getSelectedMessage)
  case selected of
    Just msg
      | isGap msg -> do tId <- use csCurrentTeamId
                        cId <- use (csCurrentChannelId tId)
                        asyncFetchMessagesForGap cId msg
    _        -> return ()

viewMessage :: Message -> MH ()
viewMessage m = do
    tId <- use csCurrentTeamId
    let w = tabbedWindow VMTabMessage (viewMessageWindowTemplate tId) MessageSelect (78, 25)
    csCurrentTeam.tsViewedMessage .= Just (m, w)
    runTabShowHandlerFor (twValue w) w
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
        openedAll <- and <$> mapM (openLinkTarget . _linkTarget) urls
        case openedAll of
            True -> return ()
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
    mode <- use (csCurrentTeam.tsMode)
    selected <- use (csCurrentTeam.tsMessageSelect.to selectMessageId)
    case selected of
        Just _ | mode == MessageSelect -> do
            chanMsgs <- use (csCurrentChannel.ccContents.cdMessages)
            let nextMsgId = getPrevMessageId selected chanMsgs
            csCurrentTeam.tsMessageSelect .= MessageSelectState (nextMsgId <|> selected)
        _ -> return ()

messageSelectDown :: MH ()
messageSelectDown = do
    selected <- use (csCurrentTeam.tsMessageSelect.to selectMessageId)
    case selected of
        Just _ -> whenMode MessageSelect $ do
            chanMsgs <- use (csCurrentChannel.ccContents.cdMessages)
            let nextMsgId = getNextMessageId selected chanMsgs
            csCurrentTeam.tsMessageSelect .= MessageSelectState (nextMsgId <|> selected)
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
    selected <- use (csCurrentTeam.tsMessageSelect.to selectMessageId)
    case selected of
        Just _ -> whenMode MessageSelect $ do
            chanMsgs <- use (csCurrentChannel.ccContents.cdMessages)
            case getEarliestSelectableMessage chanMsgs of
              Just firstMsg ->
                csCurrentTeam.tsMessageSelect .= MessageSelectState (firstMsg^.mMessageId <|> selected)
              Nothing -> mhLog LogError "No first message found from current message?!"
        _ -> return ()

messageSelectLast :: MH ()
messageSelectLast = do
    selected <- use (csCurrentTeam.tsMessageSelect.to selectMessageId)
    case selected of
        Just _ -> whenMode MessageSelect $ do
            chanMsgs <- use (csCurrentChannel.ccContents.cdMessages)
            case getLatestSelectableMessage chanMsgs of
              Just lastSelMsg ->
                csCurrentTeam.tsMessageSelect .= MessageSelectState (lastSelMsg^.mMessageId <|> selected)
              Nothing -> mhLog LogError "No last message found from current message?!"
        _ -> return ()

deleteSelectedMessage :: MH ()
deleteSelectedMessage = do
    selectedMessage <- use (to getSelectedMessage)
    st <- use id
    tId <- use csCurrentTeamId
    cId <- use (csCurrentChannelId tId)
    case selectedMessage of
        Just msg | isMine st msg && isDeletable msg ->
            case msg^.mOriginalPost of
              Just p ->
                  doAsyncChannelMM Preempt cId
                      (\s _ -> MM.mmDeletePost (postId p) s)
                      (\_ _ -> Just $ do
                          csCurrentTeam.tsEditState.cedEditMode .= NewPost
                          setMode Main)
              Nothing -> return ()
        _ -> return ()

beginReplyCompose :: MH ()
beginReplyCompose = do
    selected <- use (to getSelectedMessage)
    case selected of
        Just msg | isReplyable msg -> do
            rootMsg <- getReplyRootMessage msg
            let Just p = rootMsg^.mOriginalPost
            setMode Main
            csCurrentTeam.tsEditState.cedEditMode .= Replying rootMsg p
        _ -> return ()

beginEditMessage :: MH ()
beginEditMessage = do
    selected <- use (to getSelectedMessage)
    st <- use id
    case selected of
        Just msg | isMine st msg && isEditable msg -> do
            let Just p = msg^.mOriginalPost
            setMode Main
            csCurrentTeam.tsEditState.cedEditMode .= Editing p (msg^.mType)
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
            csCurrentTeam.tsEditState.cedEditor %= applyEdit (insertMany toEdit . clearZipper)
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

-- | Tell the server that we have pinned or unpinned a message.
pinMessage :: PostId -> Bool -> MH ()
pinMessage pId f = do
  session <- getSession
  doAsyncWith Normal $ do
    let doPin = if f then MM.mmPinPostToChannel else MM.mmUnpinPostToChannel
    void $ doPin pId session
    return Nothing
