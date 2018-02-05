module State.UserListOverlay
  ( enterChannelMembersUserList
  , exitUserListMode
  , userListSelectDown
  , userListSelectUp
  )
where

import Data.Sequence (Seq)
import Lens.Micro.Platform
import qualified Network.Mattermost.Endpoints as MM
import Network.Mattermost.Types

import Types
import Types.Users
import State.Common

-- | Create a PostListOverlay with the given content description and
-- with a specified list of messages.
enterUserListMode :: UserSearchScope -> MH ()
enterUserListMode scope = do
  -- Kick off a search in the specified scope
  csUserListOverlay.userListSearchScope .= scope
  csUserListOverlay.userListSelected .= Nothing
  asyncBeginSearch scope
  csMode .= UserListOverlay

asyncBeginSearch :: UserSearchScope -> MH ()
asyncBeginSearch scope = do
  csUserListOverlay.userListSearching .= True
  case scope of
      ChannelMembers cId -> doAsyncChannelMM Preempt cId
          fetchChannelMembers $ \ _ chanUsers -> do
              csUserListOverlay.userListSearchResults .= ((\u -> userInfoFromUser u True) <$> chanUsers)
              csUserListOverlay.userListSearching .= False
      AllUsers -> return ()

-- | Clear out the state of a PostListOverlay
exitUserListMode :: MH ()
exitUserListMode = do
  csUserListOverlay.userListSearchResults .= mempty
  csUserListOverlay.userListSelected .= Nothing
  csMode .= Main

-- | Move the selection up in the PostListOverlay, which corresponds
-- to finding a chronologically /newer/ message.
userListSelectUp :: MH ()
userListSelectUp = do
  return ()

-- | Move the selection down in the PostListOverlay, which corresponds
-- to finding a chronologically /old/ message.
userListSelectDown :: MH ()
userListSelectDown = do
  return ()

fetchChannelMembers :: Session -> TeamId -> ChannelId -> IO (Seq User)
fetchChannelMembers s _ c = do
    let query = MM.defaultUserQuery
          { MM.userQueryPage = Just 0
          , MM.userQueryPerPage = Just 10000
          , MM.userQueryInChannel = Just c
          }
    chanUserMap <- MM.mmGetUsers query s
    return chanUserMap

enterChannelMembersUserList :: MH ()
enterChannelMembersUserList = do
  cId <- use csCurrentChannelId
  enterUserListMode (ChannelMembers cId)
