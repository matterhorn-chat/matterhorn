-- {-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Messages where

import           Cheapskate (Blocks)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import           Data.Time.Clock (UTCTime)
import           Lens.Micro.Platform (makeLenses)
import           Network.Mattermost
import           Types.Posts


-- * Messages

-- | A 'Message' is any message we might want to render, either from
--   Mattermost itself or from a client-internal source.
data Message = Message
  { _mText          :: Blocks
  , _mUserName      :: Maybe T.Text
  , _mDate          :: UTCTime
  , _mType          :: MessageType
  , _mPending       :: Bool
  , _mDeleted       :: Bool
  , _mAttachments   :: Seq.Seq Attachment
  , _mInReplyToMsg  :: ReplyState
  , _mPostId        :: Maybe PostId
  , _mReactions     :: Map.Map T.Text Int
  , _mOriginalPost  :: Maybe Post
  } deriving (Show)

isDeletable :: Message -> Bool
isDeletable m = _mType m `elem` [CP NormalPost, CP Emote]

isReplyable :: Message -> Bool
isReplyable m = _mType m `elem` [CP NormalPost, CP Emote]

isEditable :: Message -> Bool
isEditable m = _mType m `elem` [CP NormalPost, CP Emote]

-- | A 'Message' is the representation we use for storage and
--   rendering, so it must be able to represent either a
--   post from Mattermost or an internal message. This represents
--   the union of both kinds of post types.
data MessageType = C ClientMessageType
                 | CP PostType
                 deriving (Eq, Show)

-- | The 'ReplyState' of a message represents whether a message
--   is a reply, and if so, to what message
data ReplyState =
    NotAReply
    | ParentLoaded PostId Message
    | ParentNotLoaded PostId
    deriving (Show)

-- | Convert a 'ClientMessage' to a 'Message'
clientMessageToMessage :: ClientMessage -> Message
clientMessageToMessage cm = Message
  { _mText          = getBlocks (_cmText cm)
  , _mUserName      = Nothing
  , _mDate          = _cmDate cm
  , _mType          = C $ _cmType cm
  , _mPending       = False
  , _mDeleted       = False
  , _mAttachments   = Seq.empty
  , _mInReplyToMsg  = NotAReply
  , _mPostId        = Nothing
  , _mReactions     = Map.empty
  , _mOriginalPost  = Nothing
  }

-- ** 'Message' Lenses

makeLenses ''Message
