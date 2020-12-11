module Matterhorn.State.Teams
  ( nextTeam
  , prevTeam
  , joinTeam
  , leaveTeam
  , updateTeam
  , buildTeamState
  , moveCurrentTeamLeft
  , moveCurrentTeamRight
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick.Main ( invalidateCache )
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
import           Matterhorn.State.Channels
import           Matterhorn.State.Messages
import           Matterhorn.State.Setup.Threads ( maybeStartSpellChecker )
import qualified Matterhorn.Zipper as Z


nextTeam :: MH ()
nextTeam = setTeamFocusWith Z.right

prevTeam :: MH ()
prevTeam = setTeamFocusWith Z.left

setTeamFocusWith :: (Z.Zipper () TeamId -> Z.Zipper () TeamId) -> MH ()
setTeamFocusWith f = do
    csTeamZipper %= f
    postChangeTeamCommon

postChangeTeamCommon :: MH ()
postChangeTeamCommon = do
    updateViewed False
    fetchVisibleIfNeeded

joinTeam :: TeamId -> MH ()
joinTeam tId = do
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
                csTeams.at tId .= Just ts
                teams <- use csTeams
                curTid <- use csCurrentTeamId
                csTeamZipper .= (Z.findRight (== curTid) $ mkTeamZipper teams)
                csChannels %= (chans <>)
                mh invalidateCache

leaveTeam :: TeamId -> MH ()
leaveTeam tId =
    doAsyncWith Normal $ return $ Just $ do
        mhLog LogGeneral $ T.pack $ "Leaving team " <> show tId
        csTeams.at tId .= Nothing
        setTeamFocusWith $ Z.filterZipper (/= tId)
        mh invalidateCache

updateTeam :: TeamId -> MH ()
updateTeam tId = do
    session <- getSession
    mhLog LogGeneral $ T.pack $ "Updating team " <> show tId
    doAsyncWith Normal $ do
        t <- MM.mmGetTeam tId session
        return $ Just $ do
            csTeam(tId).tsTeam .= t
            mh invalidateCache

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

moveCurrentTeamLeft :: MH ()
moveCurrentTeamLeft = setTeamOrderWith moveLeft

moveCurrentTeamRight :: MH ()
moveCurrentTeamRight = setTeamOrderWith moveRight

buildTeamState :: ChatResources -> User -> Team -> IO (TeamState, ClientChannels)
buildTeamState cr me team = do
    let tId = teamId team
        session = getResourceSession cr

    -- Create a predicate to find the last selected channel by reading the
    -- run state file. If unable to read or decode or validate the file, this
    -- predicate is just `isTownSquare`.
    isLastSelectedChannel <- do
        result <- readLastRunState tId
        case result of
            Right lrs | isValidLastRunState cr me lrs -> return $ \c ->
                 channelId c == lrs^.lrsSelectedChannelId
            _ -> return isTownSquare

    -- Get all channels, but filter down to just the one we want to start
    -- in. We get all, rather than requesting by name or ID, because
    -- we don't know whether the server will give us a last-viewed preference.
    -- We first try to find a channel matching with the last selected channel ID,
    -- failing which we look for the Town Square channel by name.
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

    --  * Spell checker and spell check timer, if configured
    spResult <- maybeStartSpellChecker (cr^.crConfiguration) (cr^.crEventQueue)

    now <- getCurrentTime
    let chanIds = mkChannelZipperList now (cr^.crConfiguration) tId
                                          Nothing (cr^.crUserPreferences)
                                          clientChans noUsers
        chanZip = Z.fromList chanIds
        clientChans = foldr (uncurry addChannel) noChannels chanPairs

    return (newTeamState team chanZip spResult, clientChans)
