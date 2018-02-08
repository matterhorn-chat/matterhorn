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
  myId <- gets myUserId
  enterUserListMode (ChannelMembers cId)
    (\u -> case u^.uiId /= myId of
      True -> changeChannel (u^.uiName) >> return True
      False -> return False
    )

-- | Show the user list overlay for showing users that are not members
-- of the current channel for the purpose of adding them to the
-- channel.
enterChannelInviteUserList :: MH ()
enterChannelInviteUserList = do
  cId <- use csCurrentChannelId
  myId <- gets myUserId
  enterUserListMode (ChannelNonMembers cId)
    (\u -> case u^.uiId /= myId of
      True -> addUserToCurrentChannel (u^.uiName) >> return True
      False -> return False
    )

-- | Show the user list overlay for showing all users for the purpose of
-- starting a direct message channel with another user.
enterDMSearchUserList :: MH ()
enterDMSearchUserList = do
  myId <- gets myUserId
  enterUserListMode AllUsers
    (\u -> case u^.uiId /= myId of
      True -> changeChannel (u^.uiName) >> return True
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
  csUserListOverlay.userListSelected .= Nothing
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
      myTId <- gets myTeamId
      doAsyncWith Preempt $ do
          results <- fetchInitialResults myTId scope session searchString
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
  maybePrefetchNextChunk

-- | We'll attempt to prefetch the next page of results if the cursor
-- gets within this many positions of the last result we have.
selectionPrefetchDelta :: Int
selectionPrefetchDelta = 10

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
maybePrefetchNextChunk :: MH ()
maybePrefetchNextChunk = do
  gettingMore <- use (csUserListOverlay.userListRequestingMore)
  hasAll <- use (csUserListOverlay.userListHasAllResults)
  searchString <- userListSearchString
  curIdx <- use (csUserListOverlay.userListSearchResults.L.listSelectedL)
  numResults <- use (csUserListOverlay.userListSearchResults.to F.length)

  let selectionNearEnd = case curIdx of
          Nothing -> False
          Just i -> numResults - (i + 1) < selectionPrefetchDelta

  when (not hasAll && T.null searchString && not gettingMore && selectionNearEnd) $ do
      let pageNum = numResults `div` searchResultsChunkSize

      csUserListOverlay.userListRequestingMore .= True
      session <- getSession
      scope <- use (csUserListOverlay.userListSearchScope)
      myTId <- gets myTeamId
      doAsyncWith Preempt $ do
          newChunk <- getUserSearchResultsPage pageNum myTId scope session searchString
          return $ do
              -- Because we only ever append, this is safe to do w.r.t.
              -- the selected index of the list. If we ever prepended or
              -- removed, we'd also need to manage the selection index
              -- to ensure it stays in bounds.
              csUserListOverlay.userListSearchResults.L.listElementsL %= (<> newChunk)
              csUserListOverlay.userListRequestingMore .= False

              -- If we got fewer results than we asked for, then we have
              -- them all!
              csUserListOverlay.userListHasAllResults .=
                  (length newChunk < searchResultsChunkSize)

-- | The number of users in a "page" for cursor movement purposes.
userListPageSize :: Int
userListPageSize = 10

-- | Perform an initial request for search results in the specified
-- scope.
fetchInitialResults :: TeamId -> UserSearchScope -> Session -> T.Text -> IO (Vec.Vector UserInfo)
fetchInitialResults = getUserSearchResultsPage 0

searchResultsChunkSize :: Int
searchResultsChunkSize = 40

getUserSearchResultsPage :: Int
                         -- ^ The page number of results to fetch, starting at zero.
                         -> TeamId
                         -- ^ My team ID.
                         -> UserSearchScope
                         -- ^ The scope to search
                         -> Session
                         -- ^ The connection session
                         -> T.Text
                         -- ^ The search string
                         -> IO (Vec.Vector UserInfo)
getUserSearchResultsPage pageNum myTId scope s searchString = do
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
                      ChannelNonMembers _ -> Just myTId
                      _                   -> Nothing
                  }
            MM.mmGetUsers query s
        False -> do
            -- Unfortunately, we don't get pagination control when there
            -- is a search string in effect. We'll get at most 100
            -- results from a search.
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
                                       ChannelNonMembers _ -> Just myTId
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
