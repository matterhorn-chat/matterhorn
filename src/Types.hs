{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}

module Types where

import           Brick (EventM)
import           Brick.AttrMap (AttrMap)
import           Brick.Widgets.Edit (Editor)
import           Cheapskate (Blocks)
import           Control.Concurrent.Chan (Chan)
import           Data.HashMap.Strict (HashMap)
import           Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import           Data.Time.Clock (UTCTime)
import           Data.Time.LocalTime (TimeZone)
import qualified Data.HashMap.Strict as HM
import qualified Graphics.Vty as Vty
import           Lens.Micro.Platform (makeLenses)
import           Network.Mattermost
import           Network.Mattermost.WebSocket.Types
import qualified Cheapskate as C
import qualified Data.Text as T

import           Zipper (Zipper)

import           InputHistory

data MMNames = MMNames
  { _cnChans    :: [String]
  , _cnDMs      :: [String]
  , _cnToChanId :: HashMap String ChannelId
  , _cnUsers    :: [String]
  , _cnToUserId :: HashMap String UserId
  }

makeLenses ''MMNames

data Name = ChannelMessages ChannelId
          | MessageInput
          | NormalChannelList
          | DMChannelList
          | HelpViewport
          deriving (Eq, Show, Ord)

-- We want to continue referring to posts by their IDs, but we don't want to
-- have to synthesize new valid IDs for messages from the client itself. To
-- that end, a PostRef can be either a PostId or a newly-generated client ID
data PostRef
  = MMId PostId
  | CLId Int
    deriving (Eq, Show)

data ClientMessageType =
    Informative
    | Error
    | DateTransition
    deriving (Eq, Show)

-- A ClientMessage is a message given to us by our client, like help text
-- or an error message.
data ClientMessage = ClientMessage
  { _cmText :: String
  , _cmDate :: UTCTime
  , _cmType :: ClientMessageType
  } deriving (Eq, Show)

makeLenses ''ClientMessage

data PostType =
    NormalPost
    | Emote
    | Join
    | Leave
    deriving (Eq, Show)

data ClientPost = ClientPost
  { _cpText        :: String
  , _cpUser        :: UserId
  , _cpDate        :: UTCTime
  , _cpType        :: PostType
  , _cpPending     :: Bool
  , _cpDeleted     :: Bool
  , _cpAttachments :: [String]
  } deriving (Eq, Show)

makeLenses ''ClientPost

data MessageType = C ClientMessageType
                 | CP PostType
                 deriving (Eq, Show)

-- This represents any message we might want to render.
data Message = Message
  { _mText        :: Blocks
  , _mUserName    :: Maybe String
  , _mDate        :: UTCTime
  , _mType        :: MessageType
  , _mPending     :: Bool
  , _mDeleted     :: Bool
  , _mAttachments :: [String]
  } deriving (Show)

makeLenses ''Message

getBlocks :: String -> Blocks
getBlocks s = bs where C.Doc _ bs = C.markdown C.def (T.pack s)

clientPostToMessage :: ClientPost -> String -> Message
clientPostToMessage cp user = Message
  { _mText     = getBlocks (_cpText cp)
  , _mUserName = Just user
  , _mDate     = _cpDate cp
  , _mType     = CP $ _cpType cp
  , _mPending  = _cpPending cp
  , _mDeleted  = _cpDeleted cp
  , _mAttachments = _cpAttachments cp
  }

clientMessageToMessage :: ClientMessage -> Message
clientMessageToMessage cm = Message
  { _mText        = getBlocks (_cmText cm)
  , _mUserName    = Nothing
  , _mDate        = _cmDate cm
  , _mType        = C $ _cmType cm
  , _mPending     = False
  , _mDeleted     = False
  , _mAttachments = []
  }

postClientPostType :: Post -> PostType
postClientPostType cp =
    if | postIsEmote cp -> Emote
       | postIsJoin  cp -> Join
       | postIsLeave cp -> Leave
       | otherwise      -> NormalPost

postIsEmote :: Post -> Bool
postIsEmote p =
    and [ HM.lookup "override_icon_url" (postProps p) == Just (""::String)
        , HM.lookup "override_username" (postProps p) == Just ("webhook"::String)
        , ("*" `isPrefixOf` postMessage p)
        , ("*" `isSuffixOf` postMessage p)
        ]

postIsJoin :: Post -> Bool
postIsJoin p = "has left the channel" `isInfixOf` postMessage p

postIsLeave :: Post -> Bool
postIsLeave p = "has left the channel" `isInfixOf` postMessage p

toClientPost :: Post -> ClientPost
toClientPost p = ClientPost
  { _cpText        = postMessage p
  , _cpUser        = postUserId p
  , _cpDate        = postCreateAt p
  , _cpType        = postClientPostType p
  , _cpPending     = False
  , _cpDeleted     = False
  , _cpAttachments = postFilenames p
  }

-- Our ChannelContents is roughly equivalent to the Post structure we get from
-- the MM API, but we also map integers to ClientMessage values, which are
-- bits out debug output from the client itself.
data ChannelContents = ChannelContents
  { _cdOrder   :: [PostRef]
  , _cdPosts   :: HashMap PostId ClientPost
  , _cdCMsgs   :: HashMap Int ClientMessage
  }

emptyChannelContents :: ChannelContents
emptyChannelContents = ChannelContents
  { _cdOrder = []
  , _cdPosts = HM.empty
  , _cdCMsgs = HM.empty
  }

makeLenses ''ChannelContents

data ChannelInfo = ChannelInfo
  { _cdViewed  :: UTCTime
  , _cdUpdated :: UTCTime
  , _cdName    :: String
  , _cdPurpose :: String
  , _cdType    :: Type
  , _cdLoaded  :: Bool
  }

makeLenses ''ChannelInfo

data ClientChannel = ClientChannel
  { _ccContents :: ChannelContents
  , _ccInfo     :: ChannelInfo
  }

makeLenses ''ClientChannel

type RequestChan = Chan (IO (ChatState -> EventM Name ChatState))

data Mode =
    Main
    | ShowHelp
    deriving (Eq)

data ChatState = ChatState
  { _csTok      :: Token
  , _csConn     :: ConnectionData
  , _csFocus    :: Zipper ChannelId
  , _csNames    :: MMNames
  , _csMe       :: User
  , _csMyTeam   :: Team
  , _msgMap     :: HashMap ChannelId ClientChannel
  , _usrMap     :: HashMap UserId UserProfile
  , _cmdLine    :: Editor Name
  , _timeZone   :: TimeZone
  , _timeFormat :: Maybe String
  , _csInputHistory :: InputHistory
  , _csInputHistoryPosition :: HM.HashMap ChannelId (Maybe Int)
  , _csLastChannelInput :: HM.HashMap ChannelId String
  , _csCurrentCompletion :: Maybe String
  , _csRequestQueue :: RequestChan
  , _csTheme    :: AttrMap
  , _csMode     :: Mode
  }

makeLenses ''ChatState

data Event
  = VtyEvent Vty.Event
    -- ^ For events that arise from VTY
  | WSEvent WebsocketEvent
    -- ^ For events that arise from the websocket
  | RespEvent (ChatState -> EventM Name ChatState)
    -- ^ For the result values of async IO operations
