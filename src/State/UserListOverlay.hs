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

import Data.Sequence (Seq)
import qualified Data.Vector as Vec
import qualified Data.Foldable as F
import qualified Data.Text as T
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
  searchString <- userListSearchString
  csUserListOverlay.userListSearching .= True
  session <- use (csResources.crSession)
  scope <- use (csUserListOverlay.userListSearchScope)
  doAsyncWith Preempt $ do
      chanUsers <- fetchInitialResults scope session searchString
      return $ do
          let lst = listFromUserSearchResults $
                      (\u -> userInfoFromUser u True) <$> (Vec.fromList $ F.toList chanUsers)
          csUserListOverlay.userListSearchResults .= lst
          csUserListOverlay.userListSearching .= False

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

-- | Move the selection down in the user list overlay by one user.
userListSelectDown :: MH ()
userListSelectDown = do
  csUserListOverlay.userListSearchResults %= L.listMoveDown

-- | Move the selection up in the user list overlay by a page of users
-- (userListPageSize).
userListPageUp :: MH ()
userListPageUp = do
  csUserListOverlay.userListSearchResults %= L.listMoveBy (-1 * userListPageSize)

-- | Move the selection down in the user list overlay by a page of users
-- (userListPageSize).
userListPageDown :: MH ()
userListPageDown = do
  csUserListOverlay.userListSearchResults %= L.listMoveBy userListPageSize

-- | The number of users in a "page" for cursor movement purposes.
userListPageSize :: Int
userListPageSize = 10

-- | Perform an initial request for search results in the specified
-- scope.
fetchInitialResults :: UserSearchScope -> Session -> T.Text -> IO (Seq User)
fetchInitialResults scope s searchString = do
    let chanScope = case scope of
            ChannelMembers cId -> Just cId
            AllUsers -> Nothing
        pageSize = 100
        pageNum = 0

    case T.null searchString of
        True -> do
            let query = MM.defaultUserQuery
                  { MM.userQueryPage = Just pageNum
                  , MM.userQueryPerPage = Just pageSize
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

userListSearchString :: MH T.Text
userListSearchString =
    (T.unlines . E.getEditContents) <$> use (csUserListOverlay.userListSearchInput)
