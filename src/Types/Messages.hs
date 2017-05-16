-- {-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Messages where

import           Cheapskate (Blocks)
import           Control.Applicative
import qualified Data.Map.Strict as Map
import           Data.Maybe (isJust)
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

-- ** 'Message' Lenses

makeLenses ''Message

-- * Message Collections

-- | A wrapper for an ordered, unique list of 'Message' values.
--
-- This type has the following instances: Show, Functor, Monoid, Foldable
type Messages = Seq.Seq Message

-- | There are also cases where the list of 'Message' values are kept
-- in reverse order (most recent -> oldest); these cases are
-- represented by the `ReverseMessages` type.
type ReverseMessages = Seq.Seq Message


-- ** Common operations on Messages


noMessages :: Messages
noMessages = mempty

appendMessage :: Message -> Messages -> Messages
appendMessage = flip (Seq.|>)

-- | Filters the message list to only those matching the specified
-- filterMessages :: (Message -> Bool) -> Messages -> Messages
filterMessages :: (Message -> Bool) -> Messages -> Seq.Seq Message
filterMessages = Seq.filter

countMessages :: Messages -> Int
countMessages = Seq.length

emptyMessages :: Messages -> Bool
emptyMessages = Seq.null


-- | Searches for the specified PostId and returns a tuple of (Maybe
-- Message, (Messages, Messages)) where the first element is the
-- Message associated with the PostId (if it exists), the first
-- element of the second is all the messages from the beginning of the
-- list to the message just before the PostId message (or all messages
-- if not found) *in reverse order*, and the second element of the
-- second are all the messages that follow the found message (none if
-- the message was never found) in *forward* order.
splitMessages :: Maybe PostId -> Messages -> (Maybe Message,
                                              (ReverseMessages, Messages))
splitMessages Nothing msgs = (Nothing, (Seq.reverse msgs, noMessages))
splitMessages pid msgs =
    -- n.b. searches from the end as that is usually where the message
    -- is more likely to be found.  There is usually < 1000 messages
    -- total, so this does not need hyper efficiency.
    case Seq.viewr msgs of
      Seq.EmptyR  -> (Nothing, (noMessages, noMessages))
      ms Seq.:> m -> if m^.mPostId == pid
                     then (Just m, (Seq.reverse ms, noMessages))
                     else let (a, (b,c)) = splitMessages pid ms
                          in case a of
                               Nothing -> (a, (m Seq.<| b, c))
                               Just _  -> (a, (b, c Seq.|> m))

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
                         Nothing -> getLastPostId
                         Just p -> either (const Nothing) id . folD fnd (Left p)
    where fnd = either fndp fndnext
          fndp c v = if v^.mPostId == Just c then Right Nothing else Left c
          idOfPost m = if m^.mDeleted then Nothing else m^.mPostId
          fndnext n m = Right (n <|> idOfPost m)

-- | Find the most recent message that is a Post (as opposed to a
-- local message) (if any).
getLastPostId :: Messages -> Maybe PostId
getLastPostId msgs = Seq.findIndexR valid msgs >>= _mPostId <$> Seq.index msgs
    where valid m = not (m^.mDeleted) && isJust (m^.mPostId)

-- | Find the most recent message that is a message posted by a user
-- that matches the (if any), skipping local client messages and any
-- user event that is not a message (i.e. find a normal message or an
-- emote).
findLatestUserMessage :: (Message -> Bool) -> Messages -> Maybe Message
findLatestUserMessage f msgs = case getLastPostId msgs of
                                  Nothing -> Nothing
                                  Just pid -> findUserMessageFrom pid msgs
    where findUserMessageFrom p ms =
              let Just msg = findMessage p ms
              in if f msg
                 then Just msg
                 else case getPrevPostId (msg^.mPostId) msgs of
                        Nothing -> Nothing
                        Just p' -> findUserMessageFrom p' msgs

-- | Return all messages that were posted after the specified date/time.
messagesAfter :: UTCTime -> Messages -> Messages
messagesAfter viewTime = Seq.takeWhileL (\m -> m^.mDate > viewTime)

-- | Reverse the order of the messages
reverseMessages :: Messages -> ReverseMessages
reverseMessages = foldl (flip (Seq.<|)) Seq.empty
