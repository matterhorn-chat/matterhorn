{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Message_QCA where

import Cheapskate_QCA
import Control.Monad (replicateM)
import Data.Map hiding (foldr)
import Data.Maybe (fromMaybe)
import Data.UUID (UUID, fromByteString)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Set as S
import Network.Mattermost.QuickCheck
import Network.Mattermost.Types
import Test.Tasty.QuickCheck
import Types.Messages
import Types.Posts

genMap :: Ord key => Gen key -> Gen value -> Gen (Map key value)
genMap gk gv = let kv = (,) <$> gk <*> gv in fromList <$> listOf kv

genSet :: (Ord v) => Gen v -> Gen (S.Set v)
genSet gv = S.fromList <$> listOf gv

genUserRef :: Gen UserRef
genUserRef = oneof [ return NoUser
                   , UserI <$> arbitrary <*> genUserId
                   , UserOverride <$> arbitrary <*> genText
                   ]

genMessage :: Gen Message
genMessage = Message
             <$> genBlocks
             <*> genText
             <*> genUserRef
             <*> genTime
             <*> genMessageType
             <*> arbitrary
             <*> arbitrary
             <*> genSeq genAttachment
             <*> genReplyState
             <*> (fmap MessagePostId <$> genMaybe genPostId)
             <*> genMap genText (genSet genUserId)
             <*> genMaybe genPost
             <*> arbitrary
             <*> arbitrary
             <*> (Just <$> genChannelId)

genUUID :: Gen UUID
genUUID = (fromMaybe (error "BUG: invalid genUUID result") . fromByteString . BSL.pack) <$>
          replicateM 16 arbitrary

-- Some tests specifically want deleted or non-deleted messages, so
-- make an easy way to specify these.
newtype Message__DeletedPost  = Message__DeletedPost { delMsg :: Message }
    deriving Show

genMessage__DeletedPost :: Gen Message__DeletedPost
genMessage__DeletedPost = Message__DeletedPost
                          <$> (Message
                              <$> genBlocks
                              <*> genText
                              <*> genUserRef
                              <*> genTime
                              <*> genMessageType
                              <*> arbitrary
                              <*> return True  -- mDeleted
                              <*> genSeq genAttachment
                              <*> genReplyState
                              <*> (Just <$> MessagePostId <$> genPostId)  -- must have been Posted if deleted
                              <*> genMap genText (genSet genUserId)
                              <*> genMaybe genPost
                              <*> arbitrary
                              <*> arbitrary
                              <*> (Just <$> genChannelId))

newtype Message__Posted = Message__Posted { postMsg :: Message }
    deriving Show

genMessage__Posted :: Gen Message__Posted
genMessage__Posted = Message__Posted
                     <$> (Message
                         <$> genBlocks
                         <*> genText
                         <*> genUserRef
                         <*> genTime
                         <*> genMessageType
                         <*> arbitrary
                         <*> return False  -- mDeleted
                         <*> genSeq genAttachment
                         <*> genReplyState
                         <*> (Just <$> MessagePostId <$> genPostId)
                         <*> genMap genText (genSet genUserId)
                         <*> genMaybe genPost
                         <*> arbitrary
                         <*> arbitrary
                         <*> (Just <$> genChannelId))


genMessageType :: Gen MessageType
genMessageType = oneof [ C <$> genClientMessageType
                      , CP <$> genClientPostType
                      ]

genClientMessageType :: Gen ClientMessageType
genClientMessageType = elements [ Informative
                         , Error
                         , DateTransition
                         , NewMessagesTransition
                         ]

genClientPostType :: Gen ClientPostType
genClientPostType = elements [ NormalPost
                             , Emote
                             , Join
                             , Leave
                             , TopicChange
                             ]

genReplyState :: Gen ReplyState
genReplyState = oneof [ return NotAReply
                      , InReplyTo <$> genPostId
                      ]

genAttachment :: Gen Attachment
genAttachment = mkAttachment
                <$> genText
                <*> genText
                <*> genFileId


instance Arbitrary Message where arbitrary = genMessage
instance Arbitrary Message__DeletedPost where arbitrary = genMessage__DeletedPost
instance Arbitrary Message__Posted where arbitrary = genMessage__Posted
instance Arbitrary PostId where arbitrary = genPostId
instance Arbitrary MessageId where arbitrary = oneof [ MessagePostId <$> genPostId
                                                     , MessageUUID <$> genUUID
                                                     ]

instance Arbitrary Messages where
    arbitrary = sized $ \s -> foldr addMessage noMessages <$> vectorOf s arbitrary

instance Arbitrary RetrogradeMessages where
    arbitrary = reverseMessages <$> arbitrary
