{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
module State
  (
  -- * Working with channels
    loadMoreMessages
  , getNewMessageCutoff
  , getEditedMessageCutoff
  , refreshClientConfig

  -- * Working with messages
  , fetchVisibleIfNeeded

  -- * Help
  , showHelpScreen
  )
where

import           Prelude ()
import           Prelude.MH

import           Brick.Main ( viewportScroll, vScrollToBeginning )
import           Lens.Micro.Platform

import qualified Network.Mattermost.Endpoints as MM
import           Network.Mattermost.Types

import           Types

import           State.Common
import           State.Messages


-- | Refresh client-accessible server configuration information. This
-- is usually triggered when a reconnect event for the WebSocket to the
-- server occurs.
refreshClientConfig :: MH ()
refreshClientConfig = do
    session <- getSession
    doAsyncWith Preempt $ do
        cfg <- MM.mmGetClientConfiguration (Just "old") session
        return (csClientConfig .= Just cfg)

loadMoreMessages :: MH ()
loadMoreMessages = whenMode ChannelScroll asyncFetchMoreMessages

getNewMessageCutoff :: ChannelId -> ChatState -> Maybe NewMessageIndicator
getNewMessageCutoff cId st = do
    cc <- st^?csChannel(cId)
    return $ cc^.ccInfo.cdNewMessageIndicator

getEditedMessageCutoff :: ChannelId -> ChatState -> Maybe ServerTime
getEditedMessageCutoff cId st = do
    cc <- st^?csChannel(cId)
    cc^.ccInfo.cdEditedMessageThreshold

showHelpScreen :: HelpTopic -> MH ()
showHelpScreen topic = do
    mh $ vScrollToBeginning (viewportScroll HelpViewport)
    setMode $ ShowHelp topic
