module Matterhorn.State.UserListWindow
  ( enterChannelMembersUserList
  , enterChannelInviteUserList
  , enterDMSearchUserList

  , userListSelectDown
  , userListSelectUp
  , userListPageDown
  , userListPageUp
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import qualified Brick.Widgets.List as L
import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Vector as Vec
import           Lens.Micro.Platform ( (.~), (.=) )

import qualified Network.Mattermost.Endpoints as MM
import qualified Network.Mattermost.Types.Config as MM
import           Network.Mattermost.Types

import           Matterhorn.State.Async ( doAsyncWith, AsyncPriority(Preempt) )
import           Matterhorn.State.Channels ( createOrFocusDMChannel, addUserToCurrentChannel )
import           Matterhorn.State.ListWindow
import           Matterhorn.Types


-- | Show the user list window for searching/showing members of the
-- current channel.
enterChannelMembersUserList :: TeamId -> MH ()
enterChannelMembersUserList myTId = do
    withCurrentChannel myTId $ \cId _ -> do
        myId <- gets myUserId
        session <- getSession

        doAsyncWith Preempt $ do
            stats <- MM.mmGetChannelStatistics cId session
            return $ Just $ do
                enterUserListMode myTId (ChannelMembers cId myTId) (Just $ channelStatsMemberCount stats)
                  (\u -> case u^.uiId /= myId of
                    True -> createOrFocusDMChannel myTId u Nothing >> return True
                    False -> return False
                  )

-- | Show the user list window for showing users that are not members
-- of the current channel for the purpose of adding them to the
-- channel.
enterChannelInviteUserList :: TeamId -> MH ()
enterChannelInviteUserList myTId = do
    withCurrentChannel myTId $ \cId _ -> do
        myId <- gets myUserId
        enterUserListMode myTId (ChannelNonMembers cId myTId) Nothing
          (\u -> case u^.uiId /= myId of
            True -> addUserToCurrentChannel myTId u >> return True
            False -> return False
          )

-- | Show the user list window for showing all users for the purpose of
-- starting a direct message channel with another user.
enterDMSearchUserList :: TeamId -> MH ()
enterDMSearchUserList myTId = do
    myId <- gets myUserId
    config <- use csClientConfig
    let restrictTeam = case MM.clientConfigRestrictDirectMessage <$> config of
            Just MM.RestrictTeam -> Just myTId
            _ -> Nothing
    enterUserListMode myTId (AllUsers restrictTeam) Nothing
      (\u -> case u^.uiId /= myId of
        True -> createOrFocusDMChannel myTId u Nothing >> return True
        False -> return False
      )

-- | Show the user list window with the given search scope, and issue a
-- request to gather the first search results.
enterUserListMode :: TeamId -> UserSearchScope -> Maybe Int -> (UserInfo -> MH Bool) -> MH ()
enterUserListMode tId scope resultCount enterHandler = do
    csTeam(tId).tsUserListWindow.listWindowRecordCount .= resultCount
    enterListWindowMode tId (csTeam(tId).tsUserListWindow) UserListWindow scope enterHandler getUserSearchResults

userInfoFromPair :: User -> Text -> UserInfo
userInfoFromPair u status =
    userInfoFromUser u True & uiStatus .~ statusFromText status

-- | Move the selection up in the user list window by one user.
userListSelectUp :: TeamId -> MH ()
userListSelectUp tId = userListMove tId L.listMoveUp

-- | Move the selection down in the user list window by one user.
userListSelectDown :: TeamId -> MH ()
userListSelectDown tId = userListMove tId L.listMoveDown

-- | Move the selection up in the user list window by a page of users
-- (userListPageSize).
userListPageUp :: TeamId -> MH ()
userListPageUp tId = userListMove tId (L.listMoveBy (-1 * userListPageSize))

-- | Move the selection down in the user list window by a page of users
-- (userListPageSize).
userListPageDown :: TeamId -> MH ()
userListPageDown tId = userListMove tId (L.listMoveBy userListPageSize)

-- | Transform the user list results in some way, e.g. by moving the
-- cursor, and then check to see whether the modification warrants a
-- prefetch of more search results.
userListMove :: TeamId -> (L.List Name UserInfo -> L.List Name UserInfo) -> MH ()
userListMove tId = listWindowMove (csTeam(tId).tsUserListWindow)

-- | The number of users in a "page" for cursor movement purposes.
userListPageSize :: Int
userListPageSize = 10

getUserSearchResults :: UserSearchScope
                     -- ^ The scope to search
                     -> Session
                     -- ^ The connection session
                     -> Text
                     -- ^ The search string
                     -> IO (Vec.Vector UserInfo)
getUserSearchResults scope s searchString = do
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
