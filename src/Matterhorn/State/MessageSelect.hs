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
  , openThreadWindow
  , exitMessageSelect
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick ( invalidateCache )
import           Brick.Widgets.Edit ( applyEdit )
import           Control.Monad ( replicateM_ )
import           Data.Text.Zipper ( clearZipper, insertMany )
import           Data.Maybe ( fromJust )
import           Lens.Micro.Platform

import qualified Network.Mattermost.Endpoints as MM
import           Network.Mattermost.Types

import           Matterhorn.Clipboard ( copyToClipboard )
import           Matterhorn.State.Common
import           Matterhorn.State.Links
import {-# SOURCE #-} Matterhorn.State.Messages ( asyncFetchMessagesForGap )
import           Matterhorn.Types
import           Matterhorn.Types.RichText ( findVerbatimChunk, makePermalink )
import           Matterhorn.Types.Common
import           Matterhorn.Windows.ViewMessage
import qualified Matterhorn.State.ThreadWindow as TW


getSelectedMessage :: Lens' ChatState (MessageInterface n i)
                   -> ChatState
                   -> Maybe Message
getSelectedMessage which st = do
    selMsgId <- selectMessageId $ st^.which.miMessageSelect
    let chanMsgs = st^.which.miMessages
    findMessage selMsgId chanMsgs

withSelectedMessage :: Lens' ChatState (MessageInterface n i)
                    -> (Message -> MH ())
                    -> MH ()
withSelectedMessage which act = do
    selectedMessage <- use (to (getSelectedMessage which))
    case selectedMessage of
        Nothing -> return ()
        Just m -> act m

beginMessageSelect :: Lens' ChatState (MessageInterface n i)
                   -> MH ()
beginMessageSelect which = do
    -- Invalidate the rendering cache since we cache messages to speed
    -- up the selection UI responsiveness. (See Draw.Messages for
    -- caching behavior.)
    mh invalidateCache

    -- Get the number of messages in the listing and set the currently
    -- selected message index to be the most recently received message
    -- that corresponds to a Post (i.e. exclude informative messages).
    --
    -- If we can't find one at all, we ignore the mode switch request
    -- and just return.
    msgs <- use (which.miMessages)
    let recentMsg = getLatestSelectableMessage msgs

    when (isJust recentMsg) $ do
        which.miMode .= MessageSelect
        which.miMessageSelect .= MessageSelectState (recentMsg >>= _mMessageId)

exitMessageSelect :: Lens' ChatState (MessageInterface n i) -> MH ()
exitMessageSelect which = which.miMode .= Compose

-- | Tell the server that the message we currently have selected
-- should have its flagged state toggled.
flagSelectedMessage :: Lens' ChatState (MessageInterface n i)
                    -> MH ()
flagSelectedMessage which =
    withSelectedMessage which $ \msg ->
        when (isFlaggable msg) $ do
            case messagePostId msg of
                Just pId -> flagMessage pId (not (msg^.mFlagged))
                Nothing -> return ()

-- | Tell the server that the message we currently have selected
-- should have its pinned state toggled.
pinSelectedMessage :: Lens' ChatState (MessageInterface n i)
                   -> MH ()
pinSelectedMessage which =
    withSelectedMessage which $ \msg -> do
        when (isPinnable msg) $ do
            case messagePostId msg of
                Just pId -> pinMessage pId (not (msg^.mPinned))
                Nothing -> return ()

viewSelectedMessage :: TeamId
                    -> Lens' ChatState (MessageInterface n i)
                    -> MH ()
viewSelectedMessage tId which =
    withSelectedMessage which $ \msg ->
        when (not (isGap msg)) $ viewMessage tId msg

-- This will only work for channel message selection, not thread message
-- selection, since there will never be gap entries in the thread view.
-- But this is generalized enough that it looks like it should work for
-- thread views, but it won't because asyncFetchMessagesForGap only
-- works for channel message selection (and should).
fillSelectedGap :: TeamId
                -> Lens' ChatState (MessageInterface n i)
                -> MH ()
fillSelectedGap tId which = do
    cId <- use (which.miChannelId)
    withSelectedMessage which $ \msg ->
        when (isGap msg) $ asyncFetchMessagesForGap cId msg

copyPostLink :: TeamId
             -> Lens' ChatState (MessageInterface n i)
             -> MH ()
copyPostLink tId which =
    withSelectedMessage which $ \msg ->
        when (isPostMessage msg) $ do
            baseUrl <- getServerBaseUrl tId
            let pId = fromJust (messageIdPostId =<< _mMessageId msg)
            copyToClipboard $ makePermalink baseUrl pId
            exitMessageSelect which

viewMessage :: TeamId -> Message -> MH ()
viewMessage tId m = do
    let w = tabbedWindow VMTabMessage (viewMessageWindowTemplate tId) (78, 25)
    csTeam(tId).tsViewedMessage .= Just (m, w)
    runTabShowHandlerFor (twValue w) w
    pushMode tId ViewMessage

yankSelectedMessageVerbatim :: Lens' ChatState (MessageInterface n i)
                            -> MH ()
yankSelectedMessageVerbatim which =
    withSelectedMessage which $ \msg -> do
        exitMessageSelect which
        case findVerbatimChunk (msg^.mText) of
            Just txt -> copyToClipboard txt
            Nothing  -> return ()

openThreadWindow :: TeamId
                 -> Lens' ChatState (MessageInterface n i)
                 -> MH ()
openThreadWindow tId which =
    withSelectedMessage which $ \msg -> do
        when (isPostMessage msg) $ do
            rootMsg <- getReplyRootMessage msg
            let p = fromJust $ rootMsg^.mOriginalPost
            case msg^.mChannelId of
                Nothing -> return ()
                Just cId -> do
                    -- Leave message selection mode
                    exitMessageSelect which
                    TW.openThreadWindow tId cId (postId p)

yankSelectedMessage :: Lens' ChatState (MessageInterface n i)
                    -> MH ()
yankSelectedMessage which =
    withSelectedMessage which $ \msg -> do
        exitMessageSelect which
        copyToClipboard $ msg^.mMarkdownSource

openSelectedMessageURLs :: Lens' ChatState (MessageInterface n i)
                        -> MH ()
openSelectedMessageURLs which =
    withSelectedMessage which $ \msg -> do
        let urls = msgURLs msg
        when (not (null urls)) $ do
            mapM_ (openLinkTarget . _linkTarget) urls

beginConfirmDeleteSelectedMessage :: TeamId
                                  -> Lens' ChatState (MessageInterface n i)
                                  -> MH ()
beginConfirmDeleteSelectedMessage tId which = do
    st <- use id
    withSelectedMessage which $ \msg ->
        when (isDeletable msg && isMine st msg) $
            pushMode tId MessageSelectDeleteConfirm

messageSelectUp :: Lens' ChatState (MessageInterface n i)
                -> MH ()
messageSelectUp which =
    withSelectedMessage which $ \msg -> do
        let selected = _mMessageId msg
        msgs <- use (which.miMessages)
        let nextMsgId = getPrevMessageId selected msgs
        which.miMessageSelect .= MessageSelectState (nextMsgId <|> selected)

messageSelectDown :: Lens' ChatState (MessageInterface n i)
                  -> MH ()
messageSelectDown which =
    withSelectedMessage which $ \msg -> do
        let selected = _mMessageId msg
        msgs <- use (which.miMessages)
        let nextMsgId = getNextMessageId selected msgs
        which.miMessageSelect .= MessageSelectState (nextMsgId <|> selected)

messageSelectDownBy :: Lens' ChatState (MessageInterface n i)
                    -> Int
                    -> MH ()
messageSelectDownBy which amt =
    replicateM_ amt $ messageSelectDown which

messageSelectUpBy :: Lens' ChatState (MessageInterface n i)
                  -> Int
                  -> MH ()
messageSelectUpBy which amt =
    replicateM_ amt $ messageSelectUp which

messageSelectFirst :: Lens' ChatState (MessageInterface n i)
                   -> MH ()
messageSelectFirst which =
    withSelectedMessage which $ \msg -> do
        let selected = _mMessageId msg
        msgs <- use (which.miMessages)
        case getEarliestSelectableMessage msgs of
          Just firstMsg ->
            which.miMessageSelect .= MessageSelectState (firstMsg^.mMessageId <|> selected)
          Nothing -> mhLog LogError "No first message found from current message?!"

messageSelectLast :: Lens' ChatState (MessageInterface n i)
                  -> MH ()
messageSelectLast which =
    withSelectedMessage which $ \msg -> do
        let selected = _mMessageId msg
        msgs <- use (which.miMessages)
        case getLatestSelectableMessage msgs of
          Just lastSelMsg ->
            which.miMessageSelect .= MessageSelectState (lastSelMsg^.mMessageId <|> selected)
          Nothing -> mhLog LogError "No last message found from current message?!"

deleteSelectedMessage :: Lens' ChatState (MessageInterface n i)
                      -> MH ()
deleteSelectedMessage which = do
    st <- use id
    withSelectedMessage which $ \msg ->
        when (isMine st msg && isDeletable msg) $
            case msg^.mOriginalPost of
                Just p ->
                    doAsyncMM Preempt
                        (\s -> MM.mmDeletePost (postId p) s)
                        (\_ -> Just $ do
                            m <- use (which.miEditor.esResetEditMode)
                            which.miEditor.esEditMode .= m
                            exitMessageSelect which)
                Nothing -> return ()

beginReplyCompose :: Lens' ChatState (MessageInterface n i)
                  -> MH ()
beginReplyCompose which = do
    withSelectedMessage which $ \msg ->
        when (isReplyable msg) $ do
            rootMsg <- getReplyRootMessage msg
            let p = fromJust $ rootMsg^.mOriginalPost
            exitMessageSelect which
            which.miEditor.esEditMode .= Replying rootMsg p

beginEditMessage :: Lens' ChatState (MessageInterface n i)
                 -> MH ()
beginEditMessage which = do
    st <- use id
    withSelectedMessage which $ \msg ->
        when (isMine st msg && isEditable msg) $ do
            let p = fromJust $ msg^.mOriginalPost
            exitMessageSelect which
            which.miEditor.esEditMode .= Editing p (msg^.mType)
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
            which.miEditor.esEditor %= applyEdit (insertMany toEdit . clearZipper)

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
