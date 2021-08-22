{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}

module Matterhorn.Types.Users
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
  , getUsernameSet
  , trimUserSigil
  , addUserSigil
  , statusFromText
  , findUserById
  , findUserByUsername
  , findUserByNickname
  , noUsers, addUser, allUsers
  , modifyUserById
  , userDeleted
  , TypingUsers
  , noTypingUsers
  , addTypingUser
  , allTypingUsers
  , expireTypingUsers
  , getAllUserIds
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import qualified Data.HashMap.Strict as HM
import qualified Data.Set as S
import           Data.Semigroup ( Max(..) )
import qualified Data.Text as T
import           Lens.Micro.Platform ( (%~), makeLenses, ix )

import           Network.Mattermost.Types ( UserId(..), User(..) )

import           Matterhorn.Types.Common
import           Matterhorn.Constants ( userSigil )

-- * 'UserInfo' Values

-- | A 'UserInfo' value represents everything we need to know at
--   runtime about a user
data UserInfo = UserInfo
  { _uiName      :: Text
  , _uiId        :: UserId
  , _uiStatus    :: UserStatus
  , _uiInTeam    :: Bool
  , _uiNickName  :: Maybe Text
  , _uiFirstName :: Text
  , _uiLastName  :: Text
  , _uiEmail     :: Text
  , _uiDeleted   :: Bool
  } deriving (Eq, Show)

-- | Is this user deleted?
userDeleted :: User -> Bool
userDeleted u =
    case userCreateAt u of
        Nothing -> False
        Just c -> userDeleteAt u > c

-- | Create a 'UserInfo' value from a Mattermost 'User' value
userInfoFromUser :: User -> Bool -> UserInfo
userInfoFromUser up inTeam = UserInfo
  { _uiName      = userUsername up
  , _uiId        = userId up
  , _uiStatus    = Offline
  , _uiInTeam    = inTeam
  , _uiNickName  =
      let nick = sanitizeUserText $ userNickname up
      in if T.null nick then Nothing else Just nick
  , _uiFirstName = sanitizeUserText $ userFirstName up
  , _uiLastName  = sanitizeUserText $ userLastName up
  , _uiEmail     = sanitizeUserText $ userEmail up
  , _uiDeleted   = userDeleted up
  }

-- | The 'UserStatus' value represents possible current status for
--   a user
data UserStatus
  = Online
  | Away
  | Offline
  | DoNotDisturb
  | Other Text
    deriving (Eq, Show)

statusFromText :: Text -> UserStatus
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
data AllMyUsers a =
    AllUsers { _ofUsers :: HashMap UserId a
             , _usernameSet :: S.Set Text
             }
             deriving Functor

makeLenses ''AllMyUsers

-- | Define the exported typename which universally binds the
-- collection to the UserInfo type.
type Users = AllMyUsers UserInfo

getUsernameSet :: Users -> S.Set Text
getUsernameSet = _usernameSet

-- | Initial collection of Users with no members
noUsers :: Users
noUsers = AllUsers HM.empty mempty

getAllUserIds :: Users -> [UserId]
getAllUserIds = HM.keys . _ofUsers

-- | Add a member to the existing collection of Users
addUser :: UserInfo -> Users -> Users
addUser userinfo u =
    u & ofUsers %~ HM.insert (userinfo^.uiId) userinfo
      & usernameSet %~ S.insert (userinfo^.uiName)

-- | Get a list of all known users
allUsers :: Users -> [UserInfo]
allUsers = HM.elems . _ofUsers

-- | Define the exported typename to represent the collection of users
-- | who are currently typing. The values kept against the user id keys are the
-- | latest timestamps of typing events from the server.
type TypingUsers = AllMyUsers (Max UTCTime)

-- | Initial collection of TypingUsers with no members
noTypingUsers :: TypingUsers
noTypingUsers = AllUsers HM.empty mempty

-- | Add a member to the existing collection of TypingUsers
addTypingUser :: UserId -> UTCTime -> TypingUsers -> TypingUsers
addTypingUser uId ts = ofUsers %~ HM.insertWith (<>) uId (Max ts)

-- | Get a list of all typing users
allTypingUsers :: TypingUsers -> [UserId]
allTypingUsers = HM.keys . _ofUsers

-- | Remove all the expired users from the collection of TypingUsers.
-- | Expiry is decided by the given timestamp.
expireTypingUsers :: UTCTime -> TypingUsers -> TypingUsers
expireTypingUsers expiryTimestamp =
    ofUsers %~ HM.filter (\(Max ts') -> ts' >= expiryTimestamp)

-- | Get the User information given the UserId
findUserById :: UserId -> Users -> Maybe UserInfo
findUserById uId = HM.lookup uId . _ofUsers

-- | Get the User information given the user's name. This is an exact
-- match on the username field. It will automatically trim a user sigil
-- from the input.
findUserByUsername :: Text -> Users -> Maybe (UserId, UserInfo)
findUserByUsername name allusers =
  case filter ((== trimUserSigil name) . _uiName . snd) $ HM.toList $ _ofUsers allusers of
    (usr : []) -> Just usr
    _ -> Nothing

-- | Get the User information given the user's name. This is an exact
-- match on the nickname field, not necessarily the presented name. It
-- will automatically trim a user sigil from the input.
findUserByNickname:: Text -> Users -> Maybe (UserId, UserInfo)
findUserByNickname nick us =
  case filter ((== (Just $ trimUserSigil nick)) . _uiNickName . snd) $ HM.toList $ _ofUsers us of
    (pair : []) -> Just pair
    _ -> Nothing

trimUserSigil :: Text -> Text
trimUserSigil n
    | userSigil `T.isPrefixOf` n = T.tail n
    | otherwise                  = n

addUserSigil :: T.Text -> T.Text
addUserSigil t | userSigil `T.isPrefixOf` t = t
               | otherwise                  = userSigil <> t

-- | Extract a specific user from the collection and perform an
-- endomorphism operation on it, then put it back into the collection.
modifyUserById :: UserId -> (UserInfo -> UserInfo) -> Users -> Users
modifyUserById uId f = ofUsers.ix(uId) %~ f
