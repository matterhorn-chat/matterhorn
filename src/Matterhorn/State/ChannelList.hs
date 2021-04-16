module Matterhorn.State.ChannelList
  ( updateSidebar
  , updateWindowTitle
  , toggleChannelListVisibility
  , showChannelInSidebar
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick.Main ( getVtyHandle, invalidateCache, invalidateCacheEntry )
import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as Seq
import           Data.Time.Clock ( getCurrentTime )
import qualified Graphics.Vty as Vty
import           Lens.Micro.Platform

import           Network.Mattermost.Types
import           Network.Mattermost.Lenses
import qualified Network.Mattermost.Endpoints as MM

import {-# SOURCE #-} Matterhorn.State.Messages ( fetchVisibleIfNeeded )
import           Matterhorn.Types
import           Matterhorn.State.Async
import qualified Matterhorn.Zipper as Z


-- | Update the sidebar for the specified team state only, or all team
-- states if not given a specific team ID.
--
-- In either case, schedule user status fetches for all users mentioned
-- in the current team's sidebar. (This should be safe because all
-- sidebars should contain the same user list.)
updateSidebar :: Maybe TeamId -> MH ()
updateSidebar mTid = do
    case mTid of
        Nothing -> do
            ts <- use csTeams
            forM_ (HM.keys ts) updateTeamSidebar
        Just tId -> do
            updateTeamSidebar tId

    -- Schedule the current team's sidebar for user status updates at
    -- the end of this MH action. This is okay because all team sidebars
    -- should include the same set of DM channels.
    z <- use (csCurrentTeam.tsFocus)
    myId <- gets myUserId
    scheduleUserStatusFetches $ myId : userIdsFromZipper z

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
        compareEntries mOld new = (channelListEntryChannelId <$> mOld) == Just (channelListEntryChannelId new)
    csTeam(tId).tsFocus %= Z.updateListBy compareEntries zl

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

showChannelInSidebar :: ChannelId -> Bool -> MH ()
showChannelInSidebar cId setPending = do
    mChan <- preuse $ csChannel cId
    me <- gets myUser
    prefs <- use (csResources.crUserPreferences)
    session <- getSession

    case mChan of
        Nothing ->
          -- The requested channel doesn't actually exist yet, so no
          -- action can be taken.  It's likely that this is a
          -- pendingChannel situation and not all of the operations to
          -- locally define the channel have completed, in which case
          -- this code will be re-entered later and the mChan will be
          -- known.
          return ()
        Just ch -> do

            -- Able to successfully switch to a known channel.  This
            -- should clear any pending channel intention.  If the
            -- intention was for this channel, then: done.  If the
            -- intention was for a different channel, reaching this
            -- point means that the pending is still outstanding but
            -- that the user identified a new channel which *was*
            -- displayable, and the UI should always prefer to SATISFY
            -- the user's latest request over any pending/background
            -- task.
            csCurrentTeam.tsPendingChannelChange .= Nothing

            now <- liftIO getCurrentTime
            csChannel(cId).ccInfo.cdSidebarShowOverride .= Just now
            updateSidebar (ch^.ccInfo.cdTeamId)

            curTid <- use csCurrentTeamId
            let tId = fromMaybe curTid (ch^.ccInfo.cdTeamId)

            case ch^.ccInfo.cdType of
                Direct -> do
                    let Just uId = ch^.ccInfo.cdDMUserId
                    case dmChannelShowPreference prefs uId of
                        Just False -> do
                            let pref = showDirectChannelPref (me^.userIdL) uId True
                            when setPending $
                                csCurrentTeam.tsPendingChannelChange .=
                                    Just (ChangeByChannelId tId (ch^.ccInfo.cdChannelId) Nothing)
                            doAsyncWith Preempt $ do
                                MM.mmSaveUsersPreferences UserMe (Seq.singleton pref) session
                                return Nothing
                        _ -> return ()

                Group ->
                    case groupChannelShowPreference prefs cId of
                        Just False -> do
                            let pref = showGroupChannelPref cId (me^.userIdL)
                            when setPending $
                                csCurrentTeam.tsPendingChannelChange .=
                                    Just (ChangeByChannelId tId (ch^.ccInfo.cdChannelId) Nothing)
                            doAsyncWith Preempt $ do
                                MM.mmSaveUsersPreferences UserMe (Seq.singleton pref) session
                                return Nothing
                        _ -> return ()

                _ -> return ()
