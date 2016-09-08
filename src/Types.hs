{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}

module Types where

import           Brick (EventM)
import           Brick.AttrMap (AttrMap)
import           Brick.Widgets.Edit (Editor)
import           Cheapskate (Blocks)
import           Control.Concurrent.Chan (Chan)
import           Data.HashMap.Strict (HashMap)
import           Data.Time.Clock (UTCTime)
import           Data.Time.LocalTime (TimeZone)
import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as Seq
import qualified Graphics.Vty as Vty
import           Lens.Micro.Platform (makeLenses, (^.), (^?), ix)
import           Network.Mattermost
import           Network.Mattermost.Lenses
import           Network.Mattermost.WebSocket.Types
import qualified Cheapskate as C
import qualified Data.Text as T

import           Zipper (Zipper)

import           InputHistory

data MMNames = MMNames
  { _cnChans    :: [T.Text]
  , _cnDMs      :: [T.Text]
  , _cnToChanId :: HashMap T.Text ChannelId
  , _cnUsers    :: [T.Text]
  , _cnToUserId :: HashMap T.Text UserId
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
  { _cmText :: T.Text
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
  { _cpText        :: Blocks
  , _cpUser        :: UserId
  , _cpDate        :: UTCTime
  , _cpType        :: PostType
  , _cpPending     :: Bool
  , _cpDeleted     :: Bool
  , _cpAttachments :: Seq.Seq T.Text
  , _cpPostId      :: PostId
  } deriving (Show)

makeLenses ''ClientPost

data MessageType = C ClientMessageType
                 | CP PostType
                 deriving (Eq, Show)

-- This represents any message we might want to render.
data Message = Message
  { _mText        :: Blocks
  , _mUserName    :: Maybe T.Text
  , _mDate        :: UTCTime
  , _mType        :: MessageType
  , _mPending     :: Bool
  , _mDeleted     :: Bool
  , _mAttachments :: Seq.Seq T.Text
  , _mPostId      :: Maybe PostId
  } deriving (Show)

makeLenses ''Message

getBlocks :: T.Text -> Blocks
getBlocks s = bs where C.Doc _ bs = C.markdown C.def s

postClientPostType :: Post -> PostType
postClientPostType cp =
    if | postIsEmote cp -> Emote
       | postIsJoin  cp -> Join
       | postIsLeave cp -> Leave
       | otherwise      -> NormalPost

postIsEmote :: Post -> Bool
postIsEmote p =
    and [ HM.lookup "override_icon_url" (postProps p) == Just (""::T.Text)
        , HM.lookup "override_username" (postProps p) == Just ("webhook"::T.Text)
        , ("*" `T.isPrefixOf` postMessage p)
        , ("*" `T.isSuffixOf` postMessage p)
        ]

postIsJoin :: Post -> Bool
postIsJoin p = "has left the channel" `T.isInfixOf` postMessage p

postIsLeave :: Post -> Bool
postIsLeave p = "has left the channel" `T.isInfixOf` postMessage p

toClientPost :: Post -> ClientPost
toClientPost p = ClientPost
  { _cpText        = getBlocks $ postMessage p
  , _cpUser        = postUserId p
  , _cpDate        = postCreateAt p
  , _cpType        = postClientPostType p
  , _cpPending     = False
  , _cpDeleted     = False
  , _cpAttachments = postFilenames p
  , _cpPostId      = p^.postIdL
  }

data ChannelContents = ChannelContents
  { _cdMessages :: Seq.Seq Message
  }

emptyChannelContents :: ChannelContents
emptyChannelContents = ChannelContents
  { _cdMessages = mempty
  }

makeLenses ''ChannelContents

data ChannelInfo = ChannelInfo
  { _cdViewed  :: UTCTime
  , _cdUpdated :: UTCTime
  , _cdName    :: T.Text
  , _cdHeader  :: T.Text
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
  { _csTok                  :: Token
  , _csConn                 :: ConnectionData
  , _csFocus                :: Zipper ChannelId
  , _csNames                :: MMNames
  , _csMe                   :: User
  , _csMyTeam               :: Team
  , _msgMap                 :: HashMap ChannelId ClientChannel
  , _usrMap                 :: HashMap UserId UserProfile
  , _cmdLine                :: Editor Name
  , _timeZone               :: TimeZone
  , _timeFormat             :: Maybe T.Text
  , _csInputHistory         :: InputHistory
  , _csInputHistoryPosition :: HM.HashMap ChannelId (Maybe Int)
  , _csLastChannelInput     :: HM.HashMap ChannelId T.Text
  , _csCurrentCompletion    :: Maybe T.Text
  , _csRequestQueue         :: RequestChan
  , _csTheme                :: AttrMap
  , _csMode                 :: Mode
  }

makeLenses ''ChatState

getUsernameForUserId :: ChatState -> UserId -> Maybe T.Text
getUsernameForUserId st uId = st^.usrMap ^? ix uId.userProfileUsernameL

clientPostToMessage :: ChatState -> ClientPost -> Message
clientPostToMessage st cp = Message
  { _mText     = _cpText cp
  , _mUserName = getUsernameForUserId st (_cpUser cp)
  , _mDate     = _cpDate cp
  , _mType     = CP $ _cpType cp
  , _mPending  = _cpPending cp
  , _mDeleted  = _cpDeleted cp
  , _mAttachments = _cpAttachments cp
  , _mPostId   = Just $ cp^.cpPostId
  }

clientMessageToMessage :: ClientMessage -> Message
clientMessageToMessage cm = Message
  { _mText        = getBlocks (_cmText cm)
  , _mUserName    = Nothing
  , _mDate        = _cmDate cm
  , _mType        = C $ _cmType cm
  , _mPending     = False
  , _mDeleted     = False
  , _mAttachments = Seq.empty
  , _mPostId      = Nothing
  }

data Event
  = VtyEvent Vty.Event
    -- ^ For events that arise from VTY
  | WSEvent WebsocketEvent
    -- ^ For events that arise from the websocket
  | RespEvent (ChatState -> EventM Name ChatState)
    -- ^ For the result values of async IO operations
