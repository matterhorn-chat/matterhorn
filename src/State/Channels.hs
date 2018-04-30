{-# LANGUAGE LambdaCase #-}
module State.Channels
  ( updateViewed
  , updateViewedChan
  )
where

import           Prelude ()
import           Prelude.MH

import           Lens.Micro.Platform

import qualified Network.Mattermost.Endpoints as MM
import           Network.Mattermost.Types

import           State.Common
import           Types


updateViewed :: MH ()
updateViewed = do
    csCurrentChannel.ccInfo.cdMentionCount .= 0
    updateViewedChan =<< use csCurrentChannelId

-- | When a new channel has been selected for viewing, this will
-- notify the server of the change, and also update the local channel
-- state to set the last-viewed time for the previous channel and
-- update the viewed time to now for the newly selected channel.
updateViewedChan :: ChannelId -> MH ()
updateViewedChan cId = use csConnectionStatus >>= \case
    Connected -> do
        -- Only do this if we're connected to avoid triggering noisy
        -- exceptions.
        pId <- use csRecentChannel
        doAsyncChannelMM Preempt cId
          (\s _ c -> MM.mmViewChannel UserMe c pId s)
          (\c () -> setLastViewedFor pId c)
    Disconnected ->
        -- Cannot update server; make no local updates to avoid getting
        -- out-of-sync with the server. Assumes that this is a temporary
        -- break in connectivity and that after the connection is
        -- restored, the user's normal activities will update state as
        -- appropriate. If connectivity is permanently lost, managing
        -- this state is irrelevant.
        return ()

-- | Called on async completion when the currently viewed channel has
-- been updated (i.e., just switched to this channel) to update local
-- state.
setLastViewedFor :: Maybe ChannelId -> ChannelId -> MH ()
setLastViewedFor prevId cId = do
    chan <- use (csChannels.to (findChannelById cId))
    -- Update new channel's viewed time, creating the channel if needed
    case chan of
        Nothing ->
            -- It's possible for us to get spurious WMChannelViewed
            -- events from the server, e.g. for channels that have been
            -- deleted. So here we ignore the request since it's hard to
            -- detect it before this point.
            return ()
        Just _  ->
          -- The server has been sent a viewed POST update, but there is
          -- no local information on what timestamp the server actually
          -- recorded. There are a couple of options for setting the
          -- local value of the viewed time:
          --
          --   1. Attempting to locally construct a value, which would
          --      involve scanning all (User) messages in the channel
          --      to find the maximum of the created date, the modified
          --      date, or the deleted date, and assuming that maximum
          --      mostly matched the server's viewed time.
          --
          --   2. Issuing a channel metadata request to get the server's
          --      new concept of the viewed time.
          --
          --   3. Having the "chan/viewed" POST that was just issued
          --      return a value from the server. See
          --      https://github.com/mattermost/platform/issues/6803.
          --
          -- Method 3 would be the best and most lightweight. Until that
          -- is available, Method 2 will be used. The downside to Method
          -- 2 is additional client-server messaging, and a delay in
          -- updating the client data, but it's also immune to any new
          -- or removed Message date fields, or anything else that would
          -- contribute to the viewed/updated times on the server.
          doAsyncChannelMM Preempt cId (\ s _ _ ->
                                           (,) <$> MM.mmGetChannel cId s
                                               <*> MM.mmGetChannelMember cId UserMe s)
          (\pcid (cwd, member) -> csChannel(pcid).ccInfo %= channelInfoFromChannelWithData cwd member)

    -- Update the old channel's previous viewed time (allows tracking of
    -- new messages)
    case prevId of
      Nothing -> return ()
      Just p -> csChannels %= (channelByIdL p %~ (clearNewMessageIndicator . clearEditedThreshold))
