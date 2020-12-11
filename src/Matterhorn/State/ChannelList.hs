module Matterhorn.State.ChannelList
  ( updateSidebar
  , updateWindowTitle
  , toggleChannelListVisibility
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick.Main ( getVtyHandle, invalidateCache, invalidateCacheEntry )
import qualified Data.HashMap.Strict as HM
import           Data.Time.Clock ( getCurrentTime )
import qualified Graphics.Vty as Vty
import           Lens.Micro.Platform

import           Network.Mattermost.Types

import {-# SOURCE #-} Matterhorn.State.Messages ( fetchVisibleIfNeeded )
import           Matterhorn.Types
import qualified Matterhorn.Zipper as Z


-- | Update the sidebar for the specified team state only, or all team
-- states if not given a specific team ID.
updateSidebar :: Maybe TeamId -> MH ()
updateSidebar mTid = do
    case mTid of
        Nothing -> do
            ts <- use csTeams
            forM_ (HM.keys ts) updateTeamSidebar
        Just tId -> do
            updateTeamSidebar tId

    updateWindowTitle

updateWindowTitle :: MH ()
updateWindowTitle = do
    -- Update the window title based on the unread status of the
    -- channels in all teams.
    ts <- use csTeams
    unreadCounts <- forM (HM.keys ts) $ \tId -> do
        z <- use (csTeam(tId).tsFocus)
        return $ sum $ (channelListGroupUnread . fst) <$> Z.toList z

    let title = "matterhorn" <> if unread > 0 then "(" <> show unread <> ")" else ""
        unread = sum unreadCounts

    vty <- mh getVtyHandle
    liftIO $ Vty.setWindowTitle vty title

updateTeamSidebar :: TeamId -> MH ()
updateTeamSidebar tId = do
    -- Invalidate the cached sidebar rendering since we are about to
    -- change the underlying state
    mh $ invalidateCacheEntry $ ChannelSidebar tId

    -- Get the currently-focused channel ID so we can compare after the
    -- zipper is rebuilt
    cconfig <- use csClientConfig
    oldCid <- use (csCurrentChannelId tId)

    -- Update the zipper
    cs <- use csChannels
    us <- getUsers
    prefs <- use (csResources.crUserPreferences)
    now <- liftIO getCurrentTime
    config <- use (csResources.crConfiguration)

    let zl = mkChannelZipperList now config tId cconfig prefs cs us
    csTeam(tId).tsFocus %= Z.updateList zl

    -- Schedule the current sidebar for user status updates at the end
    -- of this MH action.
    newZ <- use (csTeam(tId).tsFocus)
    myId <- gets myUserId
    scheduleUserStatusFetches $ myId : userIdsFromZipper newZ

    -- If the zipper rebuild caused the current channel to change, such
    -- as when the previously-focused channel was removed, we need to
    -- call fetchVisibleIfNeeded on the newly-focused channel to ensure
    -- that it gets loaded.
    newCid <- use (csCurrentChannelId tId)
    when (newCid /= oldCid) $
        fetchVisibleIfNeeded

toggleChannelListVisibility :: MH ()
toggleChannelListVisibility = do
    mh invalidateCache
    csResources.crConfiguration.configShowChannelListL %= not
