module State.UserListOverlay where

import Control.Monad
import Data.Text (Text)
import Data.Sequence (Seq)
import Lens.Micro.Platform
import qualified Network.Mattermost.Endpoints as MM
import Network.Mattermost.Types

import State
import State.Common
import Types
import Types.Users

-- | Create a PostListOverlay with the given content description and
-- with a specified list of messages.
enterUserListMode :: UserListContents -> Seq User -> MH ()
enterUserListMode contents msgs = do
  csUserListOverlay.userListUsers .= fmap (flip userInfoFromUser True) msgs
  csUserListOverlay.userListSelected .= Nothing -- join ((^.mPostId) <$> getLatestPostMsg msgs)
  csMode .= UserListOverlay contents

-- | Clear out the state of a PostListOverlay
exitUserListMode :: MH ()
exitUserListMode = do
  csUserListOverlay.userListUsers .= mempty
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
  doAsyncChannelMM Preempt cId fetchChannelMembers $ \ _ chanUsers -> do
    enterUserListMode UserListChannelMembers chanUsers
