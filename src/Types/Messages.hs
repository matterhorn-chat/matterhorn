-- {-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Messages where

import           Cheapskate (Blocks)
import           Control.Applicative
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import           Data.Time.Clock (UTCTime)
import           Lens.Micro.Platform
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

-- | A wrapper for an ordered, unique list of 'Message' values
type Messages = Seq.Seq Message

-- ** 'Message' Lenses

makeLenses ''Message

-- ** Common operations on Messages

noMessages :: Messages
noMessages = mempty

appendMessage :: Message -> Messages -> Messages
appendMessage = flip (Seq.|>)

-- filterMessages :: (Message -> Bool) -> Messages -> Messages
filterMessages :: (Message -> Bool) -> Messages -> Seq.Seq Message
filterMessages = Seq.filter

countMessages :: Messages -> Int
countMessages = Seq.length


-- | findMessage searches for a specific message as identified by the
-- PostId.  The search starts from the most recent messages because
-- that is the most likely place the message will occur.
findMessage :: PostId -> Messages -> Maybe Message
findMessage pid msgs = Seq.findIndexR (\m -> m^.mPostId == Just pid) msgs
                       >>= Just . Seq.index msgs

-- | Look forward for the first Message that corresponds to a user
-- Post (i.e. has a post ID) that follows the specified PostId
getNextPostId :: Maybe PostId -> Messages -> Maybe PostId
getNextPostId = getRelPostId foldl


-- | Look backwards for the first Message that corresponds to a user
-- Post (i.e. has a post ID) that comes before the specified PostId.
getPrevPostId :: Maybe PostId -> Messages -> Maybe PostId
getPrevPostId = getRelPostId $ foldr . flip

-- | Find the next PostId after the specified PostId (if there is one)
-- by folding in the specified direction
getRelPostId :: ((Either PostId (Maybe PostId)
                      -> Message
                      -> Either PostId (Maybe PostId))
                -> Either PostId (Maybe PostId)
                -> Messages
                -> Either PostId (Maybe PostId))
             -> Maybe PostId
             -> Messages
             -> Maybe PostId
getRelPostId folD jp = case jp of
                         Nothing -> getLastPost
                         Just p -> either (const Nothing) id . folD fnd (Left p)
    where fnd = either fndp fndnext
          fndp c v = if v^.mPostId == Just c then Right Nothing else Left c
          idOfPost m = if m^.mDeleted then Nothing else m^.mPostId
          fndnext n m = Right (n <|> idOfPost m)

-- | Find the most recent message that is a user Post (if any).
getLastPost :: Messages -> Maybe PostId
getLastPost msgs = Seq.findIndexR (\m -> not (m^.mDeleted)) msgs >>=
                   _mPostId <$> Seq.index msgs
