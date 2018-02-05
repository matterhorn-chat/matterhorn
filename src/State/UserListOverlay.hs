module State.UserListOverlay
  ( enterChannelMembersUserList
  , resetUserListSearch
  , exitUserListMode

  , userListSelectDown
  , userListSelectUp
  , userListPageDown
  , userListPageUp

  , userListSearchString
  )
where

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

-- | Show the user list overlay for searching/showing members of the
-- current channel.
enterChannelMembersUserList :: MH ()
enterChannelMembersUserList = do
  cId <- use csCurrentChannelId
  enterUserListMode (ChannelMembers cId)

-- | Show the user list overlay with the given search scope, and issue a
-- request to gather the first search results.
enterUserListMode :: UserSearchScope -> MH ()
enterUserListMode scope = do
  csUserListOverlay.userListSearchScope .= scope
  csUserListOverlay.userListSelected .= Nothing
  csUserListOverlay.userListSearchInput.E.editContentsL %= Z.clearZipper
  csMode .= UserListOverlay
  resetUserListSearch

resetUserListSearch :: MH ()
resetUserListSearch = do
  searchPending <- use (csUserListOverlay.userListSearching)

  when (not searchPending) $ do
      searchString <- userListSearchString
      csUserListOverlay.userListSearching .= True
      session <- use (csResources.crSession)
      scope <- use (csUserListOverlay.userListSearchScope)
      doAsyncWith Preempt $ do
          results <- fetchInitialResults scope session searchString
          return $ do
              let lst = listFromUserSearchResults results
              csUserListOverlay.userListSearchResults .= lst
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
  csMode .= Main

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
  searchString <- userListSearchString
  when (T.null searchString && not gettingMore) $ do
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
          doAsyncWith Preempt $ do
              newChunk <- getUserSearchResultsPage pageNum scope session searchString
              return $ do
                  -- Because we only ever append, this is safe to do
                  -- w.r.t. the selected index of the list. If we ever
                  -- prepended or removed, we'd also need to manage the
                  -- selection index to ensure it stays in bounds.
                  csUserListOverlay.userListSearchResults.L.listElementsL %= (<> newChunk)
                  csUserListOverlay.userListRequestingMore .= False

-- | The number of users in a "page" for cursor movement purposes.
userListPageSize :: Int
userListPageSize = 10

-- | Perform an initial request for search results in the specified
-- scope.
fetchInitialResults :: UserSearchScope -> Session -> T.Text -> IO (Vec.Vector UserInfo)
fetchInitialResults = getUserSearchResultsPage 0

searchResultsChunkSize :: Int
searchResultsChunkSize = 40

getUserSearchResultsPage :: Int -> UserSearchScope -> Session -> T.Text -> IO (Vec.Vector UserInfo)
getUserSearchResultsPage pageNum scope s searchString = do
    let chanScope = case scope of
            ChannelMembers cId -> Just cId
            AllUsers -> Nothing

    users <- case T.null searchString of
        True -> do
            let query = MM.defaultUserQuery
                  { MM.userQueryPage = Just pageNum
                  , MM.userQueryPerPage = Just searchResultsChunkSize
                  , MM.userQueryInChannel = chanScope
                  }
            MM.mmGetUsers query s
        False -> do
            let query = UserSearch { userSearchTerm = searchString
                                   , userSearchAllowInactive = False
                                   , userSearchWithoutTeam = False
                                   , userSearchInChannelId = case scope of
                                       ChannelMembers cId -> Just cId
                                       AllUsers           -> Nothing
                                   , userSearchNotInTeamId = Nothing
                                   , userSearchNotInChannelId = Nothing -- TODO: use this for /add-user
                                   , userSearchTeamId = Nothing -- TODO: when should we omit or provide this?
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
