{-# LANGUAGE TemplateHaskell #-}

module Types.Users
  ( UserInfo(..)
  , UserStatus(..)
  -- * Lenses created for accessing UserInfo fields
  , uiName, uiId, uiStatus, uiInTeam, uiNickName, uiFirstName, uiLastName, uiEmail
  -- * Various operations on UserInfo
  -- * Creating UserInfo objects
  , userInfoFromUser
  -- * Miscellaneous
  , statusFromText
  )
where

import qualified Data.Text as T
import           Lens.Micro.Platform
import           Network.Mattermost.Types (UserId, User(..))

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
