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


getSelectedMessage :: SimpleGetter ChatState MessageSelectState
                   -> Traversal' ChatState Messages
                   -> ChatState
                   -> Maybe Message
getSelectedMessage selWhich msgsWhich st = do
    selMsgId <- selectMessageId $ st^.selWhich
    let chanMsgs = st^.msgsWhich
    findMessage selMsgId chanMsgs

beginMessageSelect :: TeamId
                   -> Lens' ChatState MessageSelectState
                   -> Traversal' ChatState Messages
                   -> Mode
                   -> MH ()
beginMessageSelect tId selWhich msgsWhich m = do
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
    msgs <- use msgsWhich
    let recentMsg = getLatestSelectableMessage msgs

    when (isJust recentMsg) $ do
        pushMode tId m
        selWhich .= MessageSelectState (recentMsg >>= _mMessageId)

-- | Tell the server that the message we currently have selected
-- should have its flagged state toggled.
flagSelectedMessage :: SimpleGetter ChatState MessageSelectState
                    -> Traversal' ChatState Messages
                    -> MH ()
flagSelectedMessage selWhich msgsWhich = do
  selected <- use (to (getSelectedMessage selWhich msgsWhich))
  case selected of
    Just msg
      | isFlaggable msg, Just pId <- messagePostId msg ->
        flagMessage pId (not (msg^.mFlagged))
    _        -> return ()

-- | Tell the server that the message we currently have selected
-- should have its pinned state toggled.
pinSelectedMessage :: SimpleGetter ChatState MessageSelectState
                   -> Traversal' ChatState Messages
                   -> MH ()
pinSelectedMessage selWhich msgsWhich = do
  selected <- use (to (getSelectedMessage selWhich msgsWhich))
  case selected of
    Just msg
      | isPinnable msg, Just pId <- messagePostId msg ->
        pinMessage pId (not (msg^.mPinned))
    _ -> return ()

viewSelectedMessage :: TeamId
                    -> SimpleGetter ChatState MessageSelectState
                    -> Traversal' ChatState Messages
                    -> MH ()
viewSelectedMessage tId selWhich msgsWhich = do
  selected <- use (to (getSelectedMessage selWhich msgsWhich))
  case selected of
    Just msg
      | not (isGap msg) -> viewMessage tId msg
    _        -> return ()

-- TODO: this should only work for channel message selection, since
-- there will never be gap entries in the thread view. But this is
-- generalized enough that it looks like it should work for thread
-- views, but it won't because asyncFetchMessagesForGap only works for
-- channel message selection (and should).
fillSelectedGap :: TeamId
                -> SimpleGetter ChatState MessageSelectState
                -> Traversal' ChatState Messages
                -> MH ()
fillSelectedGap tId selWhich msgsWhich = do
    withCurrentChannel tId $ \cId _ -> do
        selected <- use (to (getSelectedMessage selWhich msgsWhich))
        case selected of
          Just msg
            | isGap msg -> asyncFetchMessagesForGap cId msg
          _        -> return ()

copyPostLink :: TeamId
             -> SimpleGetter ChatState MessageSelectState
             -> Traversal' ChatState Messages
             -> MH ()
copyPostLink tId selWhich msgsWhich = do
  selected <- use (to (getSelectedMessage selWhich msgsWhich))
  case selected of
    Just msg | isPostMessage msg -> do
        baseUrl <- getServerBaseUrl tId
        let pId = fromJust (messageIdPostId =<< _mMessageId msg)
        copyToClipboard $ makePermalink baseUrl pId
        popMode tId
    _ -> return ()

viewMessage :: TeamId -> Message -> MH ()
viewMessage tId m = do
    let w = tabbedWindow VMTabMessage (viewMessageWindowTemplate tId) (78, 25)
    csTeam(tId).tsViewedMessage .= Just (m, w)
    runTabShowHandlerFor (twValue w) w
    pushMode tId ViewMessage

yankSelectedMessageVerbatim :: TeamId
                            -> SimpleGetter ChatState MessageSelectState
                            -> Traversal' ChatState Messages
                            -> MH ()
yankSelectedMessageVerbatim tId selWhich msgsWhich = do
    selectedMessage <- use (to (getSelectedMessage selWhich msgsWhich))
    case selectedMessage of
        Nothing -> return ()
        Just m -> do
            popMode tId
            case findVerbatimChunk (m^.mText) of
                Just txt -> copyToClipboard txt
                Nothing  -> return ()

yankSelectedMessage :: TeamId
                    -> SimpleGetter ChatState MessageSelectState
                    -> Traversal' ChatState Messages
                    -> MH ()
yankSelectedMessage tId selWhich msgsWhich = do
    selectedMessage <- use (to (getSelectedMessage selWhich msgsWhich))
    case selectedMessage of
        Nothing -> return ()
        Just m -> do
            popMode tId
            copyToClipboard $ m^.mMarkdownSource

openSelectedMessageURLs :: SimpleGetter ChatState MessageSelectState
                        -> Traversal' ChatState Messages
                        -> MH ()
openSelectedMessageURLs selWhich msgsWhich = do
    mCurMsg <- use (to (getSelectedMessage selWhich msgsWhich))
    case mCurMsg of
        Nothing -> return ()
        Just curMsg -> do
            let urls = msgURLs curMsg
            when (not (null urls)) $ do
                mapM_ (openLinkTarget . _linkTarget) urls

beginConfirmDeleteSelectedMessage :: TeamId
                                  -> SimpleGetter ChatState MessageSelectState
                                  -> Traversal' ChatState Messages
                                  -> MH ()
beginConfirmDeleteSelectedMessage tId selWhich msgsWhich = do
    st <- use id
    selected <- use (to (getSelectedMessage selWhich msgsWhich))
    case selected of
        Just msg | isDeletable msg && isMine st msg ->
            pushMode tId MessageSelectDeleteConfirm
        _ -> return ()

messageSelectUp :: Lens' ChatState MessageSelectState
                -> Traversal' ChatState Messages
                -> MH ()
messageSelectUp selWhich msgsWhich = do
    selected <- use (selWhich.to selectMessageId)
    case selected of
        Just _ -> do
            msgs <- use msgsWhich
            let nextMsgId = getPrevMessageId selected msgs
            selWhich .= MessageSelectState (nextMsgId <|> selected)
        _ -> return ()

messageSelectDown :: Lens' ChatState MessageSelectState
                  -> Traversal' ChatState Messages
                  -> MH ()
messageSelectDown selWhich msgsWhich = do
    selected <- use (selWhich.to selectMessageId)
    case selected of
        Just _ -> do
            msgs <- use msgsWhich
            let nextMsgId = getNextMessageId selected msgs
            selWhich .= MessageSelectState (nextMsgId <|> selected)
        _ -> return ()

messageSelectDownBy :: Lens' ChatState MessageSelectState
                    -> Traversal' ChatState Messages
                    -> Int
                    -> MH ()
messageSelectDownBy selWhich msgsWhich amt
    | amt <= 0 = return ()
    | otherwise =
        messageSelectDown selWhich msgsWhich >> messageSelectDownBy selWhich msgsWhich (amt - 1)

messageSelectUpBy :: Lens' ChatState MessageSelectState
                  -> Traversal' ChatState Messages
                  -> Int
                  -> MH ()
messageSelectUpBy selWhich msgsWhich amt
    | amt <= 0 = return ()
    | otherwise =
      messageSelectUp selWhich msgsWhich >> messageSelectUpBy selWhich msgsWhich (amt - 1)

messageSelectFirst :: Lens' ChatState MessageSelectState
                   -> Traversal' ChatState Messages
                   -> MH ()
messageSelectFirst selWhich msgsWhich = do
    selected <- use (selWhich.to selectMessageId)
    case selected of
        Just _ -> do
            msgs <- use msgsWhich
            case getEarliestSelectableMessage msgs of
              Just firstMsg ->
                selWhich .= MessageSelectState (firstMsg^.mMessageId <|> selected)
              Nothing -> mhLog LogError "No first message found from current message?!"
        _ -> return ()

messageSelectLast :: Lens' ChatState MessageSelectState
                  -> Traversal' ChatState Messages
                  -> MH ()
messageSelectLast selWhich msgsWhich = do
    selected <- use (selWhich.to selectMessageId)
    case selected of
        Just _ -> do
            msgs <- use msgsWhich
            case getLatestSelectableMessage msgs of
              Just lastSelMsg ->
                selWhich .= MessageSelectState (lastSelMsg^.mMessageId <|> selected)
              Nothing -> mhLog LogError "No last message found from current message?!"
        _ -> return ()

deleteSelectedMessage :: TeamId
                      -> SimpleGetter ChatState MessageSelectState
                      -> Traversal' ChatState Messages
                      -> Lens' ChatState EditState
                      -> MH ()
deleteSelectedMessage tId selWhich msgsWhich editWhich = do
    selectedMessage <- use (to (getSelectedMessage selWhich msgsWhich))
    st <- use id
    case selectedMessage of
        Just msg | isMine st msg && isDeletable msg ->
            case msg^.mOriginalPost of
              Just p ->
                  doAsyncMM Preempt
                      (\s -> MM.mmDeletePost (postId p) s)
                      (\_ -> Just $ do
                          editWhich.cedEditMode .= NewPost
                          popMode tId)
              Nothing -> return ()
        _ -> return ()

beginReplyCompose :: TeamId
                  -> SimpleGetter ChatState MessageSelectState
                  -> Traversal' ChatState Messages
                  -> Lens' ChatState EditState
                  -> MH ()
beginReplyCompose tId selWhich msgsWhich editWhich = do
    selected <- use (to (getSelectedMessage selWhich msgsWhich))
    case selected of
        Just msg | isReplyable msg -> do
            rootMsg <- getReplyRootMessage msg
            let p = fromJust $ rootMsg^.mOriginalPost
            popMode tId
            editWhich.cedEditMode .= Replying rootMsg p
        _ -> return ()

beginEditMessage :: TeamId
                 -> SimpleGetter ChatState MessageSelectState
                 -> Traversal' ChatState Messages
                 -> Lens' ChatState EditState
                 -> MH ()
beginEditMessage tId selWhich msgsWhich editWhich = do
    selected <- use (to (getSelectedMessage selWhich msgsWhich))
    st <- use id
    case selected of
        Just msg | isMine st msg && isEditable msg -> do
            let p = fromJust $ msg^.mOriginalPost
            popMode tId
            editWhich.cedEditMode .= Editing p (msg^.mType)
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
            editWhich.cedEditor %= applyEdit (insertMany toEdit . clearZipper)
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
