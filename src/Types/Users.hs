{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveFunctor #-}

module Types.Users
  ( UserInfo(..)
  , UserStatus(..)
  , Users   -- constructor remains internal
  -- * Lenses created for accessing UserInfo fields
  , uiName, uiId, uiStatus, uiInTeam, uiNickName, uiFirstName, uiLastName, uiEmail
  , uiDeleted
  -- * Various operations on UserInfo
  -- * Creating UserInfo objects
  , userInfoFromUser
  -- * Miscellaneous
  , statusFromText
  , findUserById
  , findUserByName
  , findUserByDMChannelName
  , findUserByNickname
  , noUsers, addUser, allUsers
  , modifyUserById
  , getDMChannelName
  , userIdForDMChannel
  , userDeleted
  , TypingUsers
  , noTypingUsers
  , addTypingUser
  , allTypingUsers
  , expireTypingUsers
  , getAllUserIds
  )
where

import           Data.Semigroup (Max(..))
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import           Data.Time (UTCTime)
import           Lens.Micro.Platform
import           Network.Mattermost.Types (Id(Id), UserId(..), User(..), idString)
import           Prelude ()
import           Prelude.MH

-- * 'UserInfo' Values

-- | A 'UserInfo' value represents everything we need to know at
--   runtime about a user
data UserInfo = UserInfo
  { _uiName      :: T.Text
  , _uiId        :: UserId
  , _uiStatus    :: UserStatus
  , _uiInTeam    :: Bool
  , _uiNickName  :: Maybe T.Text
  , _uiFirstName :: T.Text
  , _uiLastName  :: T.Text
  , _uiEmail     :: T.Text
  , _uiDeleted   :: Bool
  } deriving (Eq, Show)

-- | Is this user deleted?
userDeleted :: User -> Bool
userDeleted u = userDeleteAt u > userCreateAt u

-- | Create a 'UserInfo' value from a Mattermost 'User' value
userInfoFromUser :: User -> Bool -> UserInfo
userInfoFromUser up inTeam = UserInfo
  { _uiName      = userUsername up
  , _uiId        = userId up
  , _uiStatus    = Offline
  , _uiInTeam    = inTeam
  , _uiNickName  = if T.null (userNickname up)
                   then Nothing
                   else Just $ userNickname up
  , _uiFirstName = userFirstName up
  , _uiLastName  = userLastName up
  , _uiEmail     = userEmail up
  , _uiDeleted   = userDeleted up
  }

-- | The 'UserStatus' value represents possible current status for
--   a user
data UserStatus
  = Online
  | Away
  | Offline
  | DoNotDisturb
  | Other T.Text
    deriving (Eq, Show)

statusFromText :: T.Text -> UserStatus
statusFromText t = case t of
  "online"  -> Online
  "offline" -> Offline
  "away"    -> Away
  "dnd"     -> DoNotDisturb
  _         -> Other t

-- ** 'UserInfo' lenses

makeLenses ''UserInfo

-- ** Manage the collection of all Users

-- | Define a binary kinded type to allow derivation of functor.
newtype AllMyUsers a = AllUsers { _ofUsers :: HM.HashMap UserId a }
    deriving Functor

makeLenses ''AllMyUsers

-- | Define the exported typename which universally binds the
-- collection to the UserInfo type.
type Users = AllMyUsers UserInfo

-- | Initial collection of Users with no members
noUsers :: Users
noUsers = AllUsers HM.empty

getAllUserIds :: Users -> [UserId]
getAllUserIds = HM.keys . _ofUsers

-- | Add a member to the existing collection of Users
addUser :: UserInfo -> Users -> Users
addUser userinfo = AllUsers . HM.insert (userinfo^.uiId) userinfo . _ofUsers

-- | Get a list of all known users
allUsers :: Users -> [UserInfo]
allUsers = HM.elems . _ofUsers

-- | Define the exported typename to represent the collection of users
-- | who are currently typing. The values kept against the user id keys are the
-- | latest timestamps of typing events from the server.
type TypingUsers = AllMyUsers (Max UTCTime)

-- | Initial collection of TypingUsers with no members
noTypingUsers :: TypingUsers
noTypingUsers = AllUsers HM.empty

-- | Add a member to the existing collection of TypingUsers
addTypingUser :: UserId -> UTCTime -> TypingUsers -> TypingUsers
addTypingUser uId ts = AllUsers . HM.insertWith (<>) uId (Max ts) . _ofUsers

-- | Get a list of all typing users
allTypingUsers :: TypingUsers -> [UserId]
allTypingUsers = HM.keys . _ofUsers

-- | Remove all the expired users from the collection of TypingUsers.
-- | Expiry is decided by the given timestamp.
expireTypingUsers :: UTCTime -> TypingUsers -> TypingUsers
expireTypingUsers expiryTimestamp =
  AllUsers . HM.filter (\(Max ts') -> ts' >= expiryTimestamp) . _ofUsers

-- | Get the User information given the UserId
findUserById :: UserId -> Users -> Maybe UserInfo
findUserById uId = HM.lookup uId . _ofUsers

-- | Get the User information given the user's name.  This is an exact
-- match on the username field, not necessarly the presented name.
findUserByName :: Users -> T.Text -> Maybe (UserId, UserInfo)
findUserByName allusers name =
  case filter ((== name) . _uiName . snd) $ HM.toList $ _ofUsers allusers of
    (usr : []) -> Just usr
    _ -> Nothing

-- | Get the User information given the user's name.  This is an exact
-- match on the nickname field, not necessarily the presented name.
findUserByNickname :: [UserInfo] -> T.Text -> Maybe UserInfo
findUserByNickname uList nick =
  find (nickCheck nick) uList
    where
        nickCheck n = maybe False (== n) . _uiNickName

-- | Extract a specific user from the collection and perform an
-- endomorphism operation on it, then put it back into the collection.
modifyUserById :: UserId -> (UserInfo -> UserInfo) -> Users -> Users
modifyUserById uId f = ofUsers.ix(uId) %~ f

getDMChannelName :: UserId -> UserId -> T.Text
getDMChannelName me you = cname
  where
  [loUser, hiUser] = sort $ idString <$> [ you, me ]
  cname = loUser <> "__" <> hiUser

-- | Extract the corresponding other user from a direct channel name.
-- Returns Nothing if the string is not a direct channel name or if it
-- is but neither user ID in the name matches the current user's ID.
userIdForDMChannel :: UserId
                   -- ^ My user ID
                   -> T.Text
                   -- ^ The channel name
                   -> Maybe UserId
userIdForDMChannel me chanName =
    -- Direct channel names are of the form "UID__UID" where one of the
    -- UIDs is mine and the other is the other channel participant.
    let vals = T.splitOn "__" chanName
    in case vals of
        [u1, u2] -> if | (UI $ Id u1) == me  -> Just $ UI $ Id u2
                       | (UI $ Id u2) == me  -> Just $ UI $ Id u1
                       | otherwise        -> Nothing
        _ -> Nothing

findUserByDMChannelName :: Users
                        -> T.Text -- ^ the dm channel name
                        -> UserId -- ^ me
                        -> Maybe UserInfo -- ^ you
findUserByDMChannelName users dmchan me = listToMaybe
  [ user
  | u <- HM.keys $ _ofUsers users
  , getDMChannelName me u == dmchan
  , user <- maybeToList (HM.lookup u $ _ofUsers users)
  ]
