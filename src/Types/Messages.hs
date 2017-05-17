{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Messages
  ( Message(..)
  , isDeletable, isReplyable, isEditable
  , mText, mUserName, mDate, mType, mPending, mDeleted
  , mAttachments, mInReplyToMsg, mPostId, mReactions
  , mOriginalPost
  , MessageType(..)
  , ReplyState(..)
  , clientMessageToMessage
  , Messages
  , ChronologicalMessages
  , RetrogradeMessages
  , MessageOps (..)
  , noMessages
  , splitMessages
  , findMessage
  , getNextPostId
  , getPrevPostId
  , getLatestPostId
  , findLatestUserMessage
  , messagesAfter
  , reverseMessages
  , unreverseMessages
  )
where

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

-- ----------------------------------------------------------------------

-- These declarations allow the use of a DirectionalSeq, which is a Seq
-- that uses a phantom type to identify the ordering of the elements
-- in the sequence (Forward or Reverse).  The constructors are not
-- exported from this module so that a DirectionalSeq can only be
-- constructed by the functions in this module.

data Chronological
data Retrograde
class SeqDirection a
instance SeqDirection Chronological
instance SeqDirection Retrograde

data SeqDirection dir => DirectionalSeq dir a =
    DSeq { dseq :: Seq.Seq a }
         deriving (Show, Functor, Foldable, Traversable)

instance SeqDirection a => Monoid (DirectionalSeq a Message) where
    mempty = DSeq mempty
    mappend a b = DSeq $ mappend (dseq a) (dseq b)

onDirectedSeq :: SeqDirection dir => (Seq.Seq a -> Seq.Seq b)
                 -> DirectionalSeq dir a -> DirectionalSeq dir b
onDirectedSeq f = DSeq . f . dseq

-- ----------------------------------------------------------------------

-- * Message Collections

-- | A wrapper for an ordered, unique list of 'Message' values.
--
-- This type has (and promises) the following instances: Show,
-- Functor, Monoid, Foldable, Traversable
type ChronologicalMessages = DirectionalSeq Chronological Message
type Messages = ChronologicalMessages

-- | There are also cases where the list of 'Message' values are kept
-- in reverse order (most recent -> oldest); these cases are
-- represented by the `RetrogradeMessages` type.
type RetrogradeMessages = DirectionalSeq Retrograde Message

-- ** Common operations on Messages

class MessageOps a where
    addMessage :: Message -> a -> a

instance MessageOps ChronologicalMessages where
    addMessage m ml =
        case Seq.viewr (dseq ml) of
            Seq.EmptyR -> DSeq $ Seq.singleton m
            _ Seq.:> l ->
                case compare (m^.mDate) (l^.mDate) of
                  GT -> DSeq $ dseq ml Seq.|> m
                  EQ -> if m^.mPostId == l^.mPostId && isJust (m^.mPostId)
                        then ml
                        else dirDateInsert m ml
                  LT -> dirDateInsert m ml

dirDateInsert :: Message -> ChronologicalMessages -> ChronologicalMessages
dirDateInsert m = onDirectedSeq $ finalize . foldr insAfter initial
   where initial = (Just m, mempty)
         insAfter c (Nothing, l) = (Nothing, c Seq.<| l)
         insAfter c (Just n, l) =
             case compare (n^.mDate) (c^.mDate) of
               GT -> (Nothing, c Seq.<| (n Seq.<| l))
               EQ -> if n^.mPostId == c^.mPostId && isJust (c^.mPostId)
                     then (Nothing, c Seq.<| l)
                     else (Just n, c Seq.<| l)
               LT -> (Just n, c Seq.<| l)
         finalize (Just n, l) = n Seq.<| l
         finalize (_, l) = l

noMessages :: Messages
noMessages = DSeq mempty


-- | Searches for the specified PostId and returns a tuple where the
-- first element is the Message associated with the PostId (if it
-- exists), and the second element is another tuple: the first element
-- of the second is all the messages from the beginning of the list to
-- the message just before the PostId message (or all messages if not
-- found) *in reverse order*, and the second element of the second are
-- all the messages that follow the found message (none if the message
-- was never found) in *forward* order.
splitMessages :: Maybe PostId -> Messages -> (Maybe Message,
                                              (RetrogradeMessages, Messages))
splitMessages Nothing msgs = (Nothing,
                              (DSeq $ Seq.reverse $ dseq msgs, noMessages))
splitMessages pid msgs =
    -- n.b. searches from the end as that is usually where the message
    -- is more likely to be found.  There is usually < 1000 messages
    -- total, so this does not need hyper efficiency.
    case Seq.viewr (dseq msgs) of
      Seq.EmptyR  -> (Nothing, (reverseMessages noMessages, noMessages))
      ms Seq.:> m -> if m^.mPostId == pid
                     then (Just m, (DSeq $ Seq.reverse ms, noMessages))
                     else let (a, (b,c)) = splitMessages pid $ DSeq ms
                          in case a of
                               Nothing -> (a, (DSeq $ m Seq.<| (dseq b), c))
                               Just _  -> (a, (b, DSeq $ (dseq c) Seq.|> m))

-- | findMessage searches for a specific message as identified by the
-- PostId.  The search starts from the most recent messages because
-- that is the most likely place the message will occur.
findMessage :: PostId -> Messages -> Maybe Message
findMessage pid msgs = Seq.findIndexR (\m -> m^.mPostId == Just pid) (dseq msgs)
                       >>= Just . Seq.index (dseq msgs)

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
                         Nothing -> getLatestPostId
                         Just p -> either (const Nothing) id . folD fnd (Left p)
    where fnd = either fndp fndnext
          fndp c v = if v^.mPostId == Just c then Right Nothing else Left c
          idOfPost m = if m^.mDeleted then Nothing else m^.mPostId
          fndnext n m = Right (n <|> idOfPost m)

-- | Find the most recent message that is a Post (as opposed to a
-- local message) (if any).
getLatestPostId :: Messages -> Maybe PostId
getLatestPostId msgs = Seq.findIndexR valid (dseq msgs)
                     >>= _mPostId <$> Seq.index (dseq msgs)
    where valid m = not (m^.mDeleted) && isJust (m^.mPostId)

-- | Find the most recent message that is a message posted by a user
-- that matches the (if any), skipping local client messages and any
-- user event that is not a message (i.e. find a normal message or an
-- emote).
findLatestUserMessage :: (Message -> Bool) -> Messages -> Maybe Message
findLatestUserMessage f msgs = case getLatestPostId msgs of
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
messagesAfter viewTime = onDirectedSeq $ Seq.takeWhileL (\m -> m^.mDate > viewTime)

-- | Reverse the order of the messages
reverseMessages :: Messages -> RetrogradeMessages
reverseMessages = DSeq . Seq.reverse . dseq

-- | Unreverse the order of the messages
unreverseMessages :: RetrogradeMessages -> Messages
unreverseMessages = DSeq . Seq.reverse . dseq
