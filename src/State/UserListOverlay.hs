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

-- TODO
--
-- * implement UI for DMing new users using this module (need new
--   command for this, and also maybe /focus with no args)
-- * try out single-line, multi-column listing for results

import Control.Monad (when)
import Data.Monoid ((<>))
import qualified Data.Vector as Vec
import qualified Data.Foldable as F
import qualified Data.Text as T
import qualified Data.Sequence as Seq
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import Lens.Micro.Platform
import qualified Network.Mattermost.Endpoints as MM
import Network.Mattermost.Types
import qualified Data.Text.Zipper as Z

import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Edit as E

import Types
import Types.Users
import State.Common
import State (changeChannel, addUserToCurrentChannel)

-- | Show the user list overlay for searching/showing members of the
-- current channel.
enterChannelMembersUserList :: MH ()
enterChannelMembersUserList = do
  cId <- use csCurrentChannelId
  enterUserListMode (ChannelMembers cId) (\u -> changeChannel (u^.uiName))

-- | Show the user list overlay for showing users that are not members
-- of the current channel for the purpose of adding them to the
-- channel.
enterChannelInviteUserList :: MH ()
enterChannelInviteUserList = do
  cId <- use csCurrentChannelId
  enterUserListMode (ChannelNonMembers cId) (\u -> addUserToCurrentChannel (u^.uiName))

-- | Show the user list overlay for showing all users for the purpose of
-- starting a direct message channel with another user.
enterDMSearchUserList :: MH ()
enterDMSearchUserList = do
  enterUserListMode AllUsers (\u -> changeChannel (u^.uiName))

-- | Interact with the currently-selected user (depending on how the
-- overlay is configured).
userListActivateCurrent :: MH ()
userListActivateCurrent = do
  mItem <- L.listSelectedElement <$> use (csUserListOverlay.userListSearchResults)
  case mItem of
      Nothing -> return ()
      Just (_, user) -> do
          handler <- use (csUserListOverlay.userListEnterHandler)
          setMode Main
          handler user

-- | Show the user list overlay with the given search scope, and issue a
-- request to gather the first search results.
enterUserListMode :: UserSearchScope -> (UserInfo -> MH ()) -> MH ()
enterUserListMode scope enterHandler = do
  csUserListOverlay.userListSearchScope .= scope
  csUserListOverlay.userListSelected .= Nothing
  csUserListOverlay.userListSearchInput.E.editContentsL %= Z.clearZipper
  csUserListOverlay.userListEnterHandler .= enterHandler
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
      session <- use (csResources.crSession)
      scope <- use (csUserListOverlay.userListSearchScope)
      myId <- use (csMe.to userId)
      myTeamId <- use (csMyTeam.to teamId)
      doAsyncWith Preempt $ do
          results <- removeSelf myId <$> fetchInitialResults myTeamId scope session searchString
          return $ do
              let lst = listFromUserSearchResults results
              csUserListOverlay.userListSearchResults .= lst
              csUserListOverlay.userListHasAllResults .= (length results < searchResultsChunkSize)
              csUserListOverlay.userListSearching .= False

              -- Now that the results are available, check to see if the
              -- search string changed since this request was submitted.
              -- If so, issue another search.
              afterSearchString <- userListSearchString
              when (searchString /= afterSearchString) resetUserListSearch

userInfoFromPair :: User -> T.Text -> UserInfo
userInfoFromPair u status =
    userInfoFromUser u True & uiStatus .~ statusFromText status

-- | Clear out the state of the user list overlay and return to the Main
-- mode.
exitUserListMode :: MH ()
exitUserListMode = do
  csUserListOverlay.userListSearchResults .= listFromUserSearchResults mempty
  csUserListOverlay.userListSelected .= Nothing
  csUserListOverlay.userListEnterHandler .= (const $ return ())
  setMode Main

-- | Move the selection up in the user list overlay by one user.
userListSelectUp :: MH ()
userListSelectUp = do
  csUserListOverlay.userListSearchResults %= L.listMoveUp
  prefetchNextPage

-- | Move the selection down in the user list overlay by one user.
userListSelectDown :: MH ()
userListSelectDown = do
  csUserListOverlay.userListSearchResults %= L.listMoveDown
  prefetchNextPage

-- | Move the selection up in the user list overlay by a page of users
-- (userListPageSize).
userListPageUp :: MH ()
userListPageUp = do
  csUserListOverlay.userListSearchResults %= L.listMoveBy (-1 * userListPageSize)
  prefetchNextPage

-- | Move the selection down in the user list overlay by a page of users
-- (userListPageSize).
userListPageDown :: MH ()
userListPageDown = do
  csUserListOverlay.userListSearchResults %= L.listMoveBy userListPageSize
  prefetchNextPage

-- | We'll attempt to prefetch the next page of results if the cursor
-- gets within this many positions of the last result we have.
selectionPrefetchDelta :: Int
selectionPrefetchDelta = 10

-- Prefetch next page if:
--  * the search string is empty (because we can't paginate searches,
--    just fetches for all users)
--  * cursor is within acceptable delta of end of list
--  * length of list is exactly a multiple of fetching chunk size
--    (indicating a very high probability that there are more results to
--    be fetched)
prefetchNextPage :: MH ()
prefetchNextPage = do
  gettingMore <- use (csUserListOverlay.userListRequestingMore)
  hasAll <- use (csUserListOverlay.userListHasAllResults)
  searchString <- userListSearchString
  when (not hasAll && T.null searchString && not gettingMore) $ do
      numResults <- use (csUserListOverlay.userListSearchResults.to F.length)
      curIdx <- use (csUserListOverlay.userListSearchResults.L.listSelectedL)
      let chunkMultiple = numResults `mod` searchResultsChunkSize == 0
          selectionNearEnd = case curIdx of
            Nothing -> False
            Just i -> numResults - (i + 1) < selectionPrefetchDelta

      when (selectionNearEnd && chunkMultiple) $ do
          -- The page number should be the number of the *new* page to
          -- fetch, which is (numResults / chunk size) exactly when
          -- numResults % chunk size = 0.
          let pageNum = numResults `div` searchResultsChunkSize
          csUserListOverlay.userListRequestingMore .= True
          session <- use (csResources.crSession)
          scope <- use (csUserListOverlay.userListSearchScope)
          myId <- use (csMe.to userId)
          myTeamId <- use (csMyTeam.to teamId)
          doAsyncWith Preempt $ do
              newChunk <- removeSelf myId <$> getUserSearchResultsPage pageNum myTeamId scope session searchString
              return $ do
                  -- Because we only ever append, this is safe to do
                  -- w.r.t. the selected index of the list. If we ever
                  -- prepended or removed, we'd also need to manage the
                  -- selection index to ensure it stays in bounds.
                  csUserListOverlay.userListSearchResults.L.listElementsL %= (<> newChunk)
                  csUserListOverlay.userListRequestingMore .= False

                  -- If we got fewer results than we asked for, then we
                  -- have them all!
                  csUserListOverlay.userListHasAllResults .=
                      (length newChunk < searchResultsChunkSize)

removeSelf :: UserId -> Vec.Vector UserInfo -> Vec.Vector UserInfo
removeSelf myId = Vec.filter ((/= myId) . _uiId)

-- | The number of users in a "page" for cursor movement purposes.
userListPageSize :: Int
userListPageSize = 10

-- | Perform an initial request for search results in the specified
-- scope.
fetchInitialResults :: TeamId -> UserSearchScope -> Session -> T.Text -> IO (Vec.Vector UserInfo)
fetchInitialResults = getUserSearchResultsPage 0

searchResultsChunkSize :: Int
searchResultsChunkSize = 40

getUserSearchResultsPage :: Int -> TeamId -> UserSearchScope -> Session -> T.Text -> IO (Vec.Vector UserInfo)
getUserSearchResultsPage pageNum myTeamId scope s searchString = do
    users <- case T.null searchString of
        True -> do
            let query = MM.defaultUserQuery
                  { MM.userQueryPage = Just pageNum
                  , MM.userQueryPerPage = Just searchResultsChunkSize
                  , MM.userQueryInChannel = case scope of
                      ChannelMembers cId -> Just cId
                      _                  -> Nothing
                  , MM.userQueryNotInChannel = case scope of
                      ChannelNonMembers cId -> Just cId
                      _                     -> Nothing
                  , MM.userQueryInTeam = case scope of
                      ChannelNonMembers _ -> Just myTeamId
                      _                   -> Nothing
                  }
            MM.mmGetUsers query s
        False -> do
            let query = UserSearch { userSearchTerm = searchString
                                   , userSearchAllowInactive = False
                                   , userSearchWithoutTeam = False
                                   , userSearchInChannelId = case scope of
                                       ChannelMembers cId -> Just cId
                                       _                  -> Nothing
                                   , userSearchNotInTeamId = Nothing
                                   , userSearchNotInChannelId = case scope of
                                       ChannelNonMembers cId -> Just cId
                                       _                     -> Nothing
                                   , userSearchTeamId = case scope of
                                       ChannelNonMembers _ -> Just myTeamId
                                       _                   -> Nothing
                                   }
            MM.mmSearchUsers query s

    let uList = F.toList users
        uIds = userId <$> uList

    -- Now fetch status info for the users we got.
    case null uList of
        False -> do
            statuses <- MM.mmGetUserStatusByIds (Seq.fromList uIds) s
            let statusMap = HM.fromList [ (statusUserId e, statusStatus e) | e <- F.toList statuses ]
                usersWithStatus = [ userInfoFromPair u (fromMaybe "" $ HM.lookup (userId u) statusMap)
                                  | u <- uList
                                  ]

            return $ Vec.fromList usersWithStatus
        True -> return mempty

userListSearchString :: MH T.Text
userListSearchString =
    (head . E.getEditContents) <$> use (csUserListOverlay.userListSearchInput)
