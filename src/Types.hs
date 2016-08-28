{-# LANGUAGE TemplateHaskell #-}

module Types where

import           Brick.Widgets.Edit (Editor)
import           Control.Concurrent.Chan (Chan)
import           Data.HashMap.Strict (HashMap)
import           Data.Time.Clock (UTCTime)
import           Data.Time.LocalTime (TimeZone)
import qualified Data.HashMap.Strict as HM
import qualified Graphics.Vty as Vty
import           Lens.Micro.Platform (makeLenses)
import           Network.Mattermost
import           Network.Mattermost.WebSocket.Types

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
          deriving (Eq, Show, Ord)

-- We want to continue referring to posts by their IDs, but we don't want to
-- have to synthesize new valid IDs for messages from the client itself. To
-- that end, a PostRef can be either a PostId or a newly-generated client ID
data PostRef
  = MMId PostId
  | CLId Int
    deriving (Eq, Show)

-- A ClientMessage is a message given to us by our client, like help text
-- or an error message.
data ClientMessage = ClientMessage
  { _cmText :: String
  , _cmDate :: UTCTime
  }

makeLenses ''ClientMessage

-- Our ChannelContents is roughly equivalent to the Post structure we get from
-- the MM API, but we also map integers to ClientMessage values, which are
-- bits out debug output from the client itself.
data ChannelContents = ChannelContents
  { _cdOrder   :: [PostRef]
  , _cdPosts   :: HashMap PostId Post
  , _cdCMsgs   :: HashMap Int ClientMessage
  , _cdViewed  :: UTCTime
  , _cdUpdated :: UTCTime
  }

makeLenses ''ChannelContents

data ChatState = ChatState
  { _csTok          :: Token
  , _csConn         :: ConnectionData
  , _csFocus        :: Zipper ChannelId
  , _csNames        :: MMNames
  , _csMe           :: User
  , _csMyTeam       :: Team
  , _chnMap         :: HashMap ChannelId Channel
  , _msgMap         :: HashMap ChannelId ChannelContents
  , _usrMap         :: HashMap UserId UserProfile
  , _cmdLine        :: Editor Name
  , _timeZone       :: TimeZone
  , _csRequestQueue :: Chan (IO (ChatState -> ChatState))
  , _timeFormat     :: Maybe String
  , _csInputHistory :: InputHistory
  , _csInputHistoryPosition :: HM.HashMap ChannelId (Maybe Int)
  }

type RequestChan = Chan (IO (ChatState -> ChatState))

makeLenses ''ChatState

data Event
  = VtyEvent Vty.Event -- ^ For events that arise from VTY
  | WSEvent WebsocketEvent -- ^ For events that arise from the websocket
  | RespEvent (ChatState -> ChatState) -- ^ For the result values of async
                                       -- IO operations
