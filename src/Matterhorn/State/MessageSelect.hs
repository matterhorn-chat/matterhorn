{-# LANGUAGE RankNTypes #-}
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
import           Data.Maybe ( fromJust )
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

getSelectedMessage :: TeamId -> ChatState -> Maybe Message
getSelectedMessage tId st
    | not (st^.csTeam(tId).tsMode `elem` messageSelectCompatibleModes) = Nothing
    | otherwise = do
        selMsgId <- selectMessageId $ st^.csTeam(tId).tsMessageSelect
        cId <- st^.csCurrentChannelId(tId)
        chan <- st^?csChannel(cId)
        let chanMsgs = chan ^. ccContents . cdMessages
        findMessage selMsgId chanMsgs

beginMessageSelect :: TeamId -> MH ()
beginMessageSelect tId = do
    withCurrentChannel tId $ \_ chan -> do
        -- Invalidate the rendering cache since we cache messages to
        -- speed up the selection UI responsiveness. (See Draw.Messages
        -- for caching behavior.)
        mh invalidateCache

        -- Get the number of messages in the current channel and set
        -- the currently selected message index to be the most recently
        -- received message that corresponds to a Post (i.e. exclude
        -- informative messages).
        --
        -- If we can't find one at all, we ignore the mode switch
        -- request and just return.
        let chanMsgs = chan ^. ccContents . cdMessages
            recentMsg = getLatestSelectableMessage chanMsgs

        when (isJust recentMsg) $ do
            setMode tId MessageSelect
            csTeam(tId).tsMessageSelect .= MessageSelectState (recentMsg >>= _mMessageId)

-- | Tell the server that the message we currently have selected
-- should have its flagged state toggled.
flagSelectedMessage :: TeamId -> MH ()
flagSelectedMessage tId = do
  selected <- use (to (getSelectedMessage tId))
  case selected of
    Just msg
      | isFlaggable msg, Just pId <- messagePostId msg ->
        flagMessage pId (not (msg^.mFlagged))
    _        -> return ()

-- | Tell the server that the message we currently have selected
-- should have its pinned state toggled.
pinSelectedMessage :: TeamId -> MH ()
pinSelectedMessage tId = do
  selected <- use (to (getSelectedMessage tId))
  case selected of
    Just msg
      | isPinnable msg, Just pId <- messagePostId msg ->
        pinMessage pId (not (msg^.mPinned))
    _ -> return ()

viewSelectedMessage :: TeamId -> MH ()
viewSelectedMessage tId = do
  selected <- use (to (getSelectedMessage tId))
  case selected of
    Just msg
      | not (isGap msg) -> viewMessage tId msg
    _        -> return ()

fillSelectedGap :: TeamId -> MH ()
fillSelectedGap tId = do
    withCurrentChannel tId $ \cId _ -> do
        selected <- use (to (getSelectedMessage tId))
        case selected of
          Just msg
            | isGap msg -> asyncFetchMessagesForGap cId msg
          _        -> return ()

copyPostLink :: TeamId -> MH ()
copyPostLink tId = do
  selected <- use (to (getSelectedMessage tId))
  case selected of
    Just msg | isPostMessage msg -> do
        baseUrl <- getServerBaseUrl tId
        let pId = fromJust (messageIdPostId =<< _mMessageId msg)
        copyToClipboard $ makePermalink baseUrl pId
        setMode tId Main
    _ -> return ()

viewMessage :: TeamId -> Message -> MH ()
viewMessage tId m = do
    let w = tabbedWindow VMTabMessage (viewMessageWindowTemplate tId) MessageSelect (78, 25)
    csTeam(tId).tsViewedMessage .= Just (m, w)
    runTabShowHandlerFor (twValue w) w
    setMode tId ViewMessage

yankSelectedMessageVerbatim :: TeamId -> MH ()
yankSelectedMessageVerbatim tId = do
    selectedMessage <- use (to (getSelectedMessage tId))
    case selectedMessage of
        Nothing -> return ()
        Just m -> do
            setMode tId Main
            case findVerbatimChunk (m^.mText) of
                Just txt -> copyToClipboard txt
                Nothing  -> return ()

yankSelectedMessage :: TeamId -> MH ()
yankSelectedMessage tId = do
    selectedMessage <- use (to (getSelectedMessage tId))
    case selectedMessage of
        Nothing -> return ()
        Just m -> do
            setMode tId Main
            copyToClipboard $ m^.mMarkdownSource

openSelectedMessageURLs :: TeamId -> MH ()
openSelectedMessageURLs tId = whenMode tId MessageSelect $ do
    mCurMsg <- use (to (getSelectedMessage tId))
    curMsg <- case mCurMsg of
        Nothing -> error "BUG: openSelectedMessageURLs: no selected message available"
        Just m -> return m

    let urls = msgURLs curMsg
    when (not (null urls)) $ do
        mapM_ (openLinkTarget . _linkTarget) urls

beginConfirmDeleteSelectedMessage :: TeamId -> MH ()
beginConfirmDeleteSelectedMessage tId = do
    st <- use id
    selected <- use (to (getSelectedMessage tId))
    case selected of
        Just msg | isDeletable msg && isMine st msg ->
            setMode tId MessageSelectDeleteConfirm
        _ -> return ()

messageSelectUp :: TeamId -> MH ()
messageSelectUp tId = do
    withCurrentChannel tId $ \_ chan -> do
        mode <- use (csTeam(tId).tsMode)
        selected <- use (csTeam(tId).tsMessageSelect.to selectMessageId)
        case selected of
            Just _ | mode == MessageSelect -> do
                let chanMsgs = chan^.ccContents.cdMessages
                    nextMsgId = getPrevMessageId selected chanMsgs
                csTeam(tId).tsMessageSelect .= MessageSelectState (nextMsgId <|> selected)
            _ -> return ()

messageSelectDown :: TeamId -> MH ()
messageSelectDown tId = do
    withCurrentChannel tId $ \_ chan -> do
        selected <- use (csTeam(tId).tsMessageSelect.to selectMessageId)
        case selected of
            Just _ ->
                whenMode tId MessageSelect $ do
                    let chanMsgs = chan^.ccContents.cdMessages
                        nextMsgId = getNextMessageId selected chanMsgs
                    csTeam(tId).tsMessageSelect .= MessageSelectState (nextMsgId <|> selected)
            _ -> return ()

messageSelectDownBy :: TeamId -> Int -> MH ()
messageSelectDownBy tId amt
    | amt <= 0 = return ()
    | otherwise =
        messageSelectDown tId >> messageSelectDownBy tId (amt - 1)

messageSelectUpBy :: TeamId -> Int -> MH ()
messageSelectUpBy tId amt
    | amt <= 0 = return ()
    | otherwise =
      messageSelectUp tId >> messageSelectUpBy tId (amt - 1)

messageSelectFirst :: TeamId -> MH ()
messageSelectFirst tId = do
    withCurrentChannel tId $ \_ chan -> do
        selected <- use (csTeam(tId).tsMessageSelect.to selectMessageId)
        case selected of
            Just _ ->
                whenMode tId MessageSelect $ do
                    let chanMsgs = chan^.ccContents.cdMessages
                    case getEarliestSelectableMessage chanMsgs of
                      Just firstMsg ->
                        csTeam(tId).tsMessageSelect .= MessageSelectState (firstMsg^.mMessageId <|> selected)
                      Nothing -> mhLog LogError "No first message found from current message?!"
            _ -> return ()

messageSelectLast :: TeamId -> MH ()
messageSelectLast tId = do
    withCurrentChannel tId $ \_ chan -> do
        selected <- use (csTeam(tId).tsMessageSelect.to selectMessageId)
        case selected of
            Just _ ->
                whenMode tId MessageSelect $ do
                    let chanMsgs = chan^.ccContents.cdMessages
                    case getLatestSelectableMessage chanMsgs of
                      Just lastSelMsg ->
                        csTeam(tId).tsMessageSelect .= MessageSelectState (lastSelMsg^.mMessageId <|> selected)
                      Nothing -> mhLog LogError "No last message found from current message?!"
            _ -> return ()

deleteSelectedMessage :: TeamId -> Lens' ChatState EditState -> MH ()
deleteSelectedMessage tId which = do
    withCurrentChannel tId $ \cId _ -> do
        selectedMessage <- use (to (getSelectedMessage tId))
        st <- use id
        case selectedMessage of
            Just msg | isMine st msg && isDeletable msg ->
                case msg^.mOriginalPost of
                  Just p ->
                      doAsyncChannelMM Preempt cId
                          (\s _ -> MM.mmDeletePost (postId p) s)
                          (\_ _ -> Just $ do
                              which.cedEditMode .= NewPost
                              setMode tId Main)
                  Nothing -> return ()
            _ -> return ()

beginReplyCompose :: TeamId -> Lens' ChatState EditState -> MH ()
beginReplyCompose tId which = do
    selected <- use (to (getSelectedMessage tId))
    case selected of
        Just msg | isReplyable msg -> do
            rootMsg <- getReplyRootMessage msg
            let p = fromJust $ rootMsg^.mOriginalPost
            setMode tId Main
            which.cedEditMode .= Replying rootMsg p
        _ -> return ()

beginEditMessage :: TeamId -> Lens' ChatState EditState -> MH ()
beginEditMessage tId which = do
    selected <- use (to (getSelectedMessage tId))
    st <- use id
    case selected of
        Just msg | isMine st msg && isEditable msg -> do
            let p = fromJust $ msg^.mOriginalPost
            setMode tId Main
            which.cedEditMode .= Editing p (msg^.mType)
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
            which.cedEditor %= applyEdit (insertMany toEdit . clearZipper)
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
