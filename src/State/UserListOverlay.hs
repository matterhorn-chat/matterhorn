module State.UserListOverlay
  ( enterChannelMembersUserList
  , enterChannelInviteUserList
  , enterDMSearchUserList
  , resetUserListSearch
  , exitUserListMode

  , userListActivateCurrent
  , userListSelectDown
  , userListSelectUp
  , userListPageDown
  , userListPageUp

  , userListSearchString
  )
where

import           Prelude ()
import           Prelude.MH

import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Text.Zipper as Z
import qualified Data.Vector as Vec
import           Lens.Micro.Platform ( (.=), (%=), (.~) )

import qualified Network.Mattermost.Endpoints as MM
import qualified Network.Mattermost.Types.Config as MM
import           Network.Mattermost.Types

import           State.Channels ( createOrFocusDMChannel, addUserToCurrentChannel )
import           State.Common
import           Types


-- | Show the user list overlay for searching/showing members of the
-- current channel.
enterChannelMembersUserList :: MH ()
enterChannelMembersUserList = do
  cId <- use csCurrentChannelId
  myId <- gets myUserId
  myTId <- gets myTeamId
  enterUserListMode (ChannelMembers cId myTId)
    (\u -> case u^.uiId /= myId of
      True -> createOrFocusDMChannel u Nothing >> return True
      False -> return False
    )

-- | Show the user list overlay for showing users that are not members
-- of the current channel for the purpose of adding them to the
-- channel.
enterChannelInviteUserList :: MH ()
enterChannelInviteUserList = do
  cId <- use csCurrentChannelId
  myId <- gets myUserId
  myTId <- gets myTeamId
  enterUserListMode (ChannelNonMembers cId myTId)
    (\u -> case u^.uiId /= myId of
      True -> addUserToCurrentChannel u >> return True
      False -> return False
    )

-- | Show the user list overlay for showing all users for the purpose of
-- starting a direct message channel with another user.
enterDMSearchUserList :: MH ()
enterDMSearchUserList = do
  myId <- gets myUserId
  myTId <- gets myTeamId
  config <- use csClientConfig
  let restrictTeam = case MM.clientConfigRestrictDirectMessage <$> config of
          Just MM.RestrictTeam -> Just myTId
          _ -> Nothing
  enterUserListMode (AllUsers restrictTeam)
    (\u -> case u^.uiId /= myId of
      True -> createOrFocusDMChannel u Nothing >> return True
      False -> return False
    )

-- | Interact with the currently-selected user (depending on how the
-- overlay is configured).
userListActivateCurrent :: MH ()
userListActivateCurrent = do
  mItem <- L.listSelectedElement <$> use (csUserListOverlay.userListSearchResults)
  case mItem of
      Nothing -> return ()
      Just (_, user) -> do
          handler <- use (csUserListOverlay.userListEnterHandler)
          activated <- handler user
          if activated
             then setMode Main
             else return ()

-- | Show the user list overlay with the given search scope, and issue a
-- request to gather the first search results.
enterUserListMode :: UserSearchScope -> (UserInfo -> MH Bool) -> MH ()
enterUserListMode scope enterHandler = do
  csUserListOverlay.userListSearchScope .= scope
  csUserListOverlay.userListSearchInput.E.editContentsL %= Z.clearZipper
  csUserListOverlay.userListEnterHandler .= enterHandler
  csUserListOverlay.userListSearching .= False
  csUserListOverlay.userListHasAllResults .= False
  setMode UserListOverlay
  resetUserListSearch

resetUserListSearch :: MH ()
resetUserListSearch = do
  searchPending <- use (csUserListOverlay.userListSearching)

  when (not searchPending) $ do
      searchString <- userListSearchString
      csUserListOverlay.userListSearching .= True
      csUserListOverlay.userListHasAllResults .= False
      csUserListOverlay.userListSearchResults .= listFromUserSearchResults mempty
      session <- getSession
      scope <- use (csUserListOverlay.userListSearchScope)
      doAsyncWith Preempt $ do
          results <- fetchInitialResults scope session searchString
          return $ Just $ do
              let lst = listFromUserSearchResults results
              csUserListOverlay.userListSearchResults .= lst
              -- NOTE: Disabled for now. See the hack note below for
              -- details.
              --
              -- csUserListOverlay.userListHasAllResults .= (length results < searchResultsChunkSize)
              csUserListOverlay.userListSearching .= False

              -- Now that the results are available, check to see if the
              -- search string changed since this request was submitted.
              -- If so, issue another search.
              afterSearchString <- userListSearchString
              when (searchString /= afterSearchString) resetUserListSearch

userInfoFromPair :: User -> Text -> UserInfo
userInfoFromPair u status =
    userInfoFromUser u True & uiStatus .~ statusFromText status

-- | Clear out the state of the user list overlay and return to the Main
-- mode.
exitUserListMode :: MH ()
exitUserListMode = do
  csUserListOverlay.userListSearchResults .= listFromUserSearchResults mempty
  csUserListOverlay.userListEnterHandler .= (const $ return False)
  setMode Main

-- | Move the selection up in the user list overlay by one user.
userListSelectUp :: MH ()
userListSelectUp = userListMove L.listMoveUp

-- | Move the selection down in the user list overlay by one user.
userListSelectDown :: MH ()
userListSelectDown = userListMove L.listMoveDown

-- | Move the selection up in the user list overlay by a page of users
-- (userListPageSize).
userListPageUp :: MH ()
userListPageUp = userListMove (L.listMoveBy (-1 * userListPageSize))

-- | Move the selection down in the user list overlay by a page of users
-- (userListPageSize).
userListPageDown :: MH ()
userListPageDown = userListMove (L.listMoveBy userListPageSize)

-- | Transform the user list results in some way, e.g. by moving the
-- cursor, and then check to see whether the modification warrants a
-- prefetch of more search results.
userListMove :: (L.List Name UserInfo -> L.List Name UserInfo) -> MH ()
userListMove f = do
  csUserListOverlay.userListSearchResults %= f
  -- NOTE! Do not enable this. See the docs for maybePrefetchNextChunk.
  -- For now we want to keep the code around in case it can be
  -- reinstated in the future.
  -- maybePrefetchNextChunk

-- | We'll attempt to prefetch the next page of results if the cursor
-- gets within this many positions of the last result we have.
-- selectionPrefetchDelta :: Int
-- selectionPrefetchDelta = 10

-- Prefetch the next chunk of user list search results if all of the
-- following are true:
--
--  * the search string is empty (because we can't paginate searches,
--    just fetches for all users), and
--  * cursor is within selectionPrefetchDelta positions of the end of
--    list, and
--  * the length of the current results list is exactly a multiple of
--    fetching chunk size (thus indicating a very high probability that
--    there are more results to be fetched).
--
-- NOTE: this function should be reinstated and called in 'userListMove'
-- if we start using the /users endpoint again in the future. See the
-- hack note in the getUserSearchResultsPage below for details. In the
-- mean time, no pagination of results is possible so no prefetching
-- should be done.
-- _maybePrefetchNextChunk :: MH ()
-- _maybePrefetchNextChunk = do
--   gettingMore <- use (csUserListOverlay.userListRequestingMore)
--   hasAll <- use (csUserListOverlay.userListHasAllResults)
--   searchString <- userListSearchString
--   curIdx <- use (csUserListOverlay.userListSearchResults.L.listSelectedL)
--   numResults <- use (csUserListOverlay.userListSearchResults.to F.length)
--
--   let selectionNearEnd = case curIdx of
--           Nothing -> False
--           Just i -> numResults - (i + 1) < selectionPrefetchDelta
--
--   when (not hasAll && T.null searchString && not gettingMore && selectionNearEnd) $ do
--       let pageNum = numResults `div` searchResultsChunkSize
--
--       csUserListOverlay.userListRequestingMore .= True
--       session <- getSession
--       scope <- use (csUserListOverlay.userListSearchScope)
--       myTId <- gets myTeamId
--       doAsyncWith Preempt $ do
--           newChunk <- getUserSearchResultsPage pageNum myTId scope session searchString
--           return $ Just $ do
--               -- Because we only ever append, this is safe to do w.r.t.
--               -- the selected index of the list. If we ever prepended or
--               -- removed, we'd also need to manage the selection index
--               -- to ensure it stays in bounds.
--               csUserListOverlay.userListSearchResults.L.listElementsL %= (<> newChunk)
--               csUserListOverlay.userListRequestingMore .= False
--
--               -- If we got fewer results than we asked for, then we have
--               -- them all!
--               --
--               -- NOTE: disabled for now, see the hack note below.
--               --
--               -- csUserListOverlay.userListHasAllResults .=
--               --     (length newChunk < searchResultsChunkSize)

-- | The number of users in a "page" for cursor movement purposes.
userListPageSize :: Int
userListPageSize = 10

-- | Perform an initial request for search results in the specified
-- scope.
fetchInitialResults :: UserSearchScope -> Session -> Text -> IO (Vec.Vector UserInfo)
fetchInitialResults = getUserSearchResultsPage 0

-- searchResultsChunkSize :: Int
-- searchResultsChunkSize = 40

getUserSearchResultsPage :: Int
                         -- ^ The page number of results to fetch, starting at zero.
                         -> UserSearchScope
                         -- ^ The scope to search
                         -> Session
                         -- ^ The connection session
                         -> Text
                         -- ^ The search string
                         -> IO (Vec.Vector UserInfo)
getUserSearchResultsPage _pageNum scope s searchString = do
    -- Unfortunately, we don't get pagination control when there is a
    -- search string in effect. We'll get at most 100 results from a
    -- search.
    let query = UserSearch { userSearchTerm = if T.null searchString then " " else searchString
                           -- Hack alert: Searching with the string " "
                           -- above is a hack to use the search
                           -- endpoint to get "all users" instead of
                           -- those matching a particular non-empty
                           -- non-whitespace string. This is because
                           -- only the search endpoint provides a
                           -- control to eliminate deleted users from
                           -- the results. If we don't do this, and
                           -- use the /users endpoint instead, we'll
                           -- get deleted users in those results and
                           -- then those deleted users will disappear
                           -- from the results once the user enters a
                           -- non-empty string string.
                           , userSearchAllowInactive = False
                           , userSearchWithoutTeam = False
                           , userSearchInChannelId = case scope of
                               ChannelMembers cId _ -> Just cId
                               _                    -> Nothing
                           , userSearchNotInTeamId = Nothing
                           , userSearchNotInChannelId = case scope of
                               ChannelNonMembers cId _ -> Just cId
                               _                       -> Nothing
                           , userSearchTeamId = case scope of
                               AllUsers tId            -> tId
                               ChannelMembers _ tId    -> Just tId
                               ChannelNonMembers _ tId -> Just tId
                           }
    users <- MM.mmSearchUsers query s

    let uList = toList users
        uIds = userId <$> uList

    -- Now fetch status info for the users we got.
    case null uList of
        False -> do
            statuses <- MM.mmGetUserStatusByIds (Seq.fromList uIds) s
            let statusMap = HM.fromList [ (statusUserId e, statusStatus e) | e <- toList statuses ]
                usersWithStatus = [ userInfoFromPair u (fromMaybe "" $ HM.lookup (userId u) statusMap)
                                  | u <- uList
                                  ]

            return $ Vec.fromList usersWithStatus
        True -> return mempty

userListSearchString :: MH Text
userListSearchString =
    (head . E.getEditContents) <$> use (csUserListOverlay.userListSearchInput)
