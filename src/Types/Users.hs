{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}

module Types.Users
  ( UserInfo(..)
  , UserStatus(..)
  , Users   -- constructor remains internal
  -- * Lenses created for accessing UserInfo fields
  , uiName, uiId, uiStatus, uiInTeam, uiNickName, uiFirstName, uiLastName, uiEmail
  -- * Various operations on UserInfo
  -- * Creating UserInfo objects
  , userInfoFromUser
  -- * Miscellaneous
  , statusFromText
  , findUserById
  , findUserByName
  , findUserByDMChannelName
  , noUsers, addUser, allUsers
  , modifyUserById
  , getDMChannelName
  )
where

import qualified Data.HashMap.Strict as HM
import           Data.List (sort)
import           Data.Maybe (listToMaybe, maybeToList)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Lens.Micro.Platform
import           Network.Mattermost.Types (UserId, User(..), idString)

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
  } deriving (Eq, Show)

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
  }

-- | The 'UserStatus' value represents possible current status for
--   a user
data UserStatus
  = Online
  | Away
  | Offline
  | Other T.Text
    deriving (Eq, Show)

statusFromText :: T.Text -> UserStatus
statusFromText t = case t of
  "online"  -> Online
  "offline" -> Offline
  "away"    -> Away
  _         -> Other t

-- ** 'UserInfo' lenses

makeLenses ''UserInfo

instance Ord UserInfo where
  u1 `compare` u2
    | u1^.uiStatus == Offline && u2^.uiStatus /= Offline =
      GT
    | u1^.uiStatus /= Offline && u2^.uiStatus == Offline =
      LT
    | otherwise =
      (u1^.uiName) `compare` (u2^.uiName)

-- ** Manage the collection of all Users

-- | Define a binary kinded type to allow derivation of functor.
newtype AllMyUsers a = AllUsers { _ofUsers :: HM.HashMap UserId a }
    deriving Functor

-- | Define the exported typename which universally binds the
-- collection to the UserInfo type.
type Users = AllMyUsers UserInfo

makeLenses ''AllMyUsers

-- | Initial collection of Users with no members
noUsers :: Users
noUsers = AllUsers HM.empty

-- | Add a member to the existing collection of Users
addUser :: UserId -> UserInfo -> Users -> Users
addUser uId userinfo = AllUsers . HM.insert uId userinfo . _ofUsers

-- | Get a list of all known users
allUsers :: Users -> [UserInfo]
allUsers = HM.elems . _ofUsers

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

-- | Extract a specific user from the collection and perform an
-- endomorphism operation on it, then put it back into the collection.
modifyUserById :: UserId -> (UserInfo -> UserInfo) -> Users -> Users
modifyUserById uId f = ofUsers.ix(uId) %~ f

getDMChannelName :: UserId -> UserId -> T.Text
getDMChannelName me you = cname
  where
  [loUser, hiUser] = sort $ idString <$> [ you, me ]
  cname = loUser <> "__" <> hiUser

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
