{-# LANGUAGE RankNTypes #-}
module Matterhorn.State.MessageListing
  (
  -- * Message selection mode
    beginMessageSelect
  , flagSelectedMessage
  , pinSelectedMessage
  , viewSelectedMessage
  , copyPostLink
  , yankSelectedMessageVerbatim
  , yankSelectedMessage
  , openSelectedMessageURLs
  , messageSelectUp
  , messageSelectUpBy
  , messageSelectDown
  , messageSelectDownBy
  , messageSelectFirst
  , messageSelectLast
  , getListingSelectedMessage
  , withListingSelectedMessage
  , exitMessageSelect
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick ( invalidateCache )
import           Control.Monad ( replicateM_ )
import           Data.Maybe ( fromJust )
import           Lens.Micro.Platform

import           Network.Mattermost.Types

import           Matterhorn.Clipboard ( copyToClipboard )
import           Matterhorn.State.Links
import {-# SOURCE #-} Matterhorn.State.Messages ( flagMessage, pinMessage )
import           Matterhorn.Types
import           Matterhorn.Types.RichText ( findVerbatimChunk, makePermalink )
import           Matterhorn.Windows.ViewMessage


getListingSelectedMessage :: Lens' ChatState (MessageListing n)
                          -> ChatState
                          -> Maybe Message
getListingSelectedMessage which st = do
    selMsgId <- selectMessageId $ st^.which.mlMessageSelect
    let chanMsgs = st^.which.mlMessages
    findMessage selMsgId chanMsgs

withListingSelectedMessage :: Lens' ChatState (MessageListing n)
                           -> (Message -> MH ())
                           -> MH ()
withListingSelectedMessage which act = do
    selectedMessage <- use (to (getListingSelectedMessage which))
    case selectedMessage of
        Nothing -> return ()
        Just m -> act m

beginMessageSelect :: Lens' ChatState (MessageListing n)
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
    msgs <- use (which.mlMessages)
    let recentMsg = getLatestSelectableMessage msgs

    when (isJust recentMsg) $ do
        which.mlMode .= MessageSelect
        which.mlMessageSelect .= MessageSelectState (recentMsg >>= _mMessageId)

exitMessageSelect :: Lens' ChatState (MessageListing n) -> MH ()
exitMessageSelect which = do
    m <- use (which.mlMode)
    when (m == MessageSelect) $ do
        which.mlMode .= ShowingTail

-- | Tell the server that the message we currently have selected
-- should have its flagged state toggled.
flagSelectedMessage :: Lens' ChatState (MessageListing n)
                    -> MH ()
flagSelectedMessage which =
    withListingSelectedMessage which $ \msg ->
        when (isFlaggable msg) $ do
            case messagePostId msg of
                Just pId -> flagMessage pId (not (msg^.mFlagged))
                Nothing -> return ()

-- | Tell the server that the message we currently have selected
-- should have its pinned state toggled.
pinSelectedMessage :: Lens' ChatState (MessageListing n)
                   -> MH ()
pinSelectedMessage which =
    withListingSelectedMessage which $ \msg -> do
        when (isPinnable msg) $ do
            case messagePostId msg of
                Just pId -> pinMessage pId (not (msg^.mPinned))
                Nothing -> return ()

viewSelectedMessage :: TeamId
                    -> Lens' ChatState (MessageListing n)
                    -> MH ()
viewSelectedMessage tId which =
    withListingSelectedMessage which $ \msg ->
        when (not (isGap msg)) $ viewMessage tId msg

copyPostLink :: TeamId
             -> Lens' ChatState (MessageListing n)
             -> MH ()
copyPostLink tId which =
    withListingSelectedMessage which $ \msg ->
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

yankSelectedMessageVerbatim :: Lens' ChatState (MessageListing n)
                            -> MH ()
yankSelectedMessageVerbatim which =
    withListingSelectedMessage which $ \msg -> do
        exitMessageSelect which
        case findVerbatimChunk (msg^.mText) of
            Just txt -> copyToClipboard txt
            Nothing  -> return ()

yankSelectedMessage :: Lens' ChatState (MessageListing n)
                    -> MH ()
yankSelectedMessage which =
    withListingSelectedMessage which $ \msg -> do
        exitMessageSelect which
        copyToClipboard $ msg^.mMarkdownSource

openSelectedMessageURLs :: Lens' ChatState (MessageListing n)
                        -> MH ()
openSelectedMessageURLs which =
    withListingSelectedMessage which $ \msg -> do
        let urls = msgURLs msg
        when (not (null urls)) $ do
            mapM_ (openLinkTarget . _linkTarget) urls

messageSelectUp :: Lens' ChatState (MessageListing n)
                -> MH ()
messageSelectUp which =
    withListingSelectedMessage which $ \msg -> do
        let selected = _mMessageId msg
        msgs <- use (which.mlMessages)
        let nextMsgId = getPrevMessageId selected msgs
        which.mlMessageSelect .= MessageSelectState (nextMsgId <|> selected)

messageSelectDown :: Lens' ChatState (MessageListing n)
                  -> MH ()
messageSelectDown which =
    withListingSelectedMessage which $ \msg -> do
        let selected = _mMessageId msg
        msgs <- use (which.mlMessages)
        let nextMsgId = getNextMessageId selected msgs
        which.mlMessageSelect .= MessageSelectState (nextMsgId <|> selected)

messageSelectDownBy :: Lens' ChatState (MessageListing n)
                    -> Int
                    -> MH ()
messageSelectDownBy which amt =
    replicateM_ amt $ messageSelectDown which

messageSelectUpBy :: Lens' ChatState (MessageListing n)
                  -> Int
                  -> MH ()
messageSelectUpBy which amt =
    replicateM_ amt $ messageSelectUp which

messageSelectFirst :: Lens' ChatState (MessageListing n)
                   -> MH ()
messageSelectFirst which =
    withListingSelectedMessage which $ \msg -> do
        let selected = _mMessageId msg
        msgs <- use (which.mlMessages)
        case getEarliestSelectableMessage msgs of
          Just firstMsg ->
            which.mlMessageSelect .= MessageSelectState (firstMsg^.mMessageId <|> selected)
          Nothing -> mhLog LogError "No first message found from current message?!"

messageSelectLast :: Lens' ChatState (MessageListing n)
                  -> MH ()
messageSelectLast which =
    withListingSelectedMessage which $ \msg -> do
        let selected = _mMessageId msg
        msgs <- use (which.mlMessages)
        case getLatestSelectableMessage msgs of
          Just lastSelMsg ->
            which.mlMessageSelect .= MessageSelectState (lastSelMsg^.mMessageId <|> selected)
          Nothing -> mhLog LogError "No last message found from current message?!"
