module Matterhorn.State.Teams
  ( nextTeam
  , prevTeam
  , handleJoinTeam
  , handleLeaveTeam
  , handleUpdateTeam
  , buildTeamState
  , moveCurrentTeamLeft
  , moveCurrentTeamRight
  , setTeam
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick.Main ( invalidateCache, hScrollToBeginning, viewportScroll )
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import           Data.Time.Clock ( getCurrentTime )
import qualified Data.HashMap.Strict as HM
import           Lens.Micro.Platform ( (%=), (.=), at )

import           Network.Mattermost.Lenses ( userIdL )
import           Network.Mattermost.Types ( TeamId, Team, User, userId
                                          , getId, channelId, teamId, UserParam(..)
                                          , teamOrderPref
                                          )
import qualified Network.Mattermost.Endpoints as MM

import           Matterhorn.Types
import           Matterhorn.LastRunState
import           Matterhorn.State.Async
import           Matterhorn.State.ChannelList
import           Matterhorn.State.Channels
import           Matterhorn.State.Messages
import           Matterhorn.State.Setup.Threads ( maybeStartSpellChecker )
import qualified Matterhorn.Zipper as Z


-- | Move right in the channel list to select the next team.
nextTeam :: MH ()
nextTeam = setTeamFocusWith Z.right

-- | Move left in the channel list to select the previous team.
prevTeam :: MH ()
prevTeam = setTeamFocusWith Z.left

-- | Set the current team directly
setTeam :: TeamId -> MH ()
setTeam tId = setTeamFocusWith $ Z.findRight (== tId)

-- | Change the selected team with the specified team zipper
-- transformation. This function also takes care of book-keeping
-- necessary during team switching.
setTeamFocusWith :: (Z.Zipper () TeamId -> Z.Zipper () TeamId) -> MH ()
setTeamFocusWith f = do
    -- Before we leave this team to view another one, indicate that
    -- we've viewed the current team's currently-selected channel so
    -- that this team doesn't get left with an unread indicator once we
    -- are looking at the other team. We do this when switching channels
    -- within a team in the same way.
    updateViewed True

    csTeamZipper %= f
    postChangeTeamCommon

-- | Book-keeping common to all team selection changes.
postChangeTeamCommon :: MH ()
postChangeTeamCommon = do
    updateViewed False
    fetchVisibleIfNeeded
    mh $ hScrollToBeginning (viewportScroll TeamList)

-- | Fetch the specified team and add it to the application state.
--
-- This is called in response to a server event indicating that the
-- current user was added to the team.
handleJoinTeam :: TeamId -> MH ()
handleJoinTeam tId = do
    session <- getSession
    cr <- use csResources
    me <- use csMe

    mhLog LogGeneral $ T.pack $ "Joining team " <> show tId
    doAsyncWith Normal $ do
        t <- MM.mmGetTeam tId session
        (ts, chans) <- buildTeamState cr me t
        return $ Just $ do
            curTs <- use csTeams
            let myTIds = HM.keys curTs
            when (not $ tId `elem` myTIds) $ do
                addTeamState ts chans
                updateSidebar $ Just tId
                updateWindowTitle
                refreshTeamZipper

-- | Remove the specified team to the application state.
--
-- This is called in response to a server event indicating that the
-- current user was removed from the team.
handleLeaveTeam :: TeamId -> MH ()
handleLeaveTeam tId =
    doAsyncWith Normal $ return $ Just $ do
        mhLog LogGeneral $ T.pack $ "Leaving team " <> show tId
        removeTeam tId
        updateWindowTitle
        -- Invalidating the cache here expunges any cached message
        -- renderings from the team we are leaving.
        mh invalidateCache

-- | Fetch the specified team's metadata and update it in the
-- application state.
--
-- This is called in response to a server event indicating that the
-- specified team was updated in some way.
handleUpdateTeam :: TeamId -> MH ()
handleUpdateTeam tId = do
    session <- getSession
    mhLog LogGeneral $ T.pack $ "Updating team " <> show tId
    doAsyncWith Normal $ do
        t <- MM.mmGetTeam tId session
        return $ Just $ do
            updateTeam t
            -- Invalidate the cache since we happen to know that the
            -- team name is in the cached sidebar.
            mh invalidateCache

-- | Set the team zipper ordering with the specified transformation,
-- which is expected to be either 'moveLeft' or 'moveRight'.
setTeamOrderWith :: (TeamId -> [TeamId] -> [TeamId]) -> MH ()
setTeamOrderWith sortFunc = do
    session <- getSession
    me <- use csMe

    tId <- use csCurrentTeamId
    z <- use csTeamZipper
    let tIds = teamZipperIds z
        newList = sortFunc tId tIds

    doAsyncWith Normal $ do
        let pref = teamOrderPref (me^.userIdL) newList
        MM.mmSaveUsersPreferences UserMe (Seq.singleton pref) session
        return Nothing

-- | Move the selected team left in the team list.
moveCurrentTeamLeft :: MH ()
moveCurrentTeamLeft = setTeamOrderWith moveLeft

-- | Move the selected team right in the team list.
moveCurrentTeamRight :: MH ()
moveCurrentTeamRight = setTeamOrderWith moveRight

-- | Build a new 'TeamState' for the specified team.
--
-- This function starts a new spell checker thread for the team's
-- message editor, loads the last-run state for the team (to ensure that
-- the initially-selected channel is honored), and fetches the channel
-- metadata for the team.
--
-- This returns the resulting team state as well as the channels
-- associated with the team. The caller is responsible for adding the
-- channels and the team state to the application state.
buildTeamState :: ChatResources -> User -> Team -> IO (TeamState, ClientChannels)
buildTeamState cr me team = do
    let tId = teamId team
        session = getResourceSession cr

    -- Create a predicate to find the last selected channel by reading
    -- the run state file. If unable to read or decode or validate the
    -- file, this predicate is just `isTownSquare`.
    isLastSelectedChannel <- do
        result <- readLastRunState tId
        case result of
            Right lrs | isValidLastRunState cr me lrs -> return $ \c ->
                 channelId c == lrs^.lrsSelectedChannelId
            _ -> return isTownSquare

    -- Get all channels, but filter down to just the one we want
    -- to start in. We get all, rather than requesting by name or
    -- ID, because we don't know whether the server will give us a
    -- last-viewed preference. We first try to find a channel matching
    -- with the last selected channel ID, failing which we look for the
    -- Town Square channel by name.
    userChans <- MM.mmGetChannelsForUser UserMe tId session
    let lastSelectedChans = Seq.filter isLastSelectedChannel userChans
        chans = if Seq.null lastSelectedChans
                  then Seq.filter isTownSquare userChans
                  else lastSelectedChans

    -- Since the only channel we are dealing with is by construction the
    -- last channel, we don't have to consider other cases here:
    chanPairs <- forM (toList chans) $ \c -> do
        cChannel <- makeClientChannel (userId me) c
        return (getId c, cChannel)

    -- Start the spell checker and spell check timer, if configured
    spResult <- maybeStartSpellChecker (cr^.crConfiguration) (cr^.crEventQueue)

    now <- getCurrentTime
    let chanIds = mkChannelZipperList now (cr^.crConfiguration) tId
                                          Nothing (cr^.crUserPreferences)
                                          clientChans noUsers
        chanZip = Z.fromList chanIds
        clientChans = foldr (uncurry addChannel) noChannels chanPairs

    return (newTeamState team chanZip spResult, clientChans)

-- | Add a new 'TeamState' and corresponding channels to the application
-- state.
addTeamState :: TeamState -> ClientChannels -> MH ()
addTeamState ts chans = do
    let tId = teamId $ _tsTeam ts
    csTeams.at tId .= Just ts
    csChannels %= (chans <>)

-- | Update the specified team metadata in the application state (only
-- if we are already a member of that team).
updateTeam :: Team -> MH ()
updateTeam t = do
    let tId = teamId t
    ts <- use csTeams
    when (tId `elem` HM.keys ts) $ do
        csTeam(tId).tsTeam .= t

-- | Remove the specified team from the application state.
removeTeam :: TeamId -> MH ()
removeTeam tId = do
    csTeams.at tId .= Nothing
    setTeamFocusWith $ Z.filterZipper (/= tId)
