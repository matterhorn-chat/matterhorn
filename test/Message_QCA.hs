{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Message_QCA where

import Cheapskate_QCA
import Data.Map hiding (foldr)
import Network.Mattermost.QuickCheck
import Network.Mattermost.Types
import Test.Tasty.QuickCheck
import Types.Messages
import Types.Posts

genMap :: Ord key => Gen key -> Gen value -> Gen (Map key value)
genMap gk gv = let kv = (,) <$> gk <*> gv in fromList <$> listOf kv

genMessage :: Gen Message
genMessage = Message
             <$> genBlocks
             <*> genMaybe genText
             <*> genTime
             <*> genMessageType
             <*> arbitrary
             <*> arbitrary
             <*> genSeq genAttachment
             <*> genReplyState
             <*> genMaybe genPostId
             <*> genMap genText arbitrary
             <*> genMaybe genPost
             <*> arbitrary
             <*> (Just <$> genChannelId)

-- Some tests specifically want deleted or non-deleted messages, so
-- make an easy way to specify these.
newtype Message__DeletedPost  = Message__DeletedPost { delMsg :: Message }
    deriving Show

genMessage__DeletedPost :: Gen Message__DeletedPost
genMessage__DeletedPost = Message__DeletedPost
                          <$> (Message
                               <$> genBlocks
                              <*> genMaybe genText
                              <*> genTime
                              <*> genMessageType
                              <*> arbitrary
                              <*> return True  -- mDeleted
                              <*> genSeq genAttachment
                              <*> genReplyState
                              <*> (Just <$> genPostId)  -- must have been Posted if deleted
                              <*> genMap genText arbitrary
                              <*> genMaybe genPost
                              <*> arbitrary
                              <*> (Just <$> genChannelId))

newtype Message__Posted = Message__Posted { postMsg :: Message }
    deriving Show

genMessage__Posted :: Gen Message__Posted
genMessage__Posted = Message__Posted
                     <$> (Message
                          <$> genBlocks
                         <*> genMaybe genText
                         <*> genTime
                         <*> genMessageType
                         <*> arbitrary
                         <*> return False  -- mDeleted
                         <*> genSeq genAttachment
                         <*> genReplyState
                         <*> (Just <$> genPostId)
                         <*> genMap genText arbitrary
                         <*> genMaybe genPost
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
                      , ParentLoaded <$> genPostId <*> genMessage
                      , ParentNotLoaded <$> genPostId
                      ]

genAttachment :: Gen Attachment
genAttachment = Attachment
                <$> genText
                <*> genText
                <*> genFileId


instance Arbitrary Message where arbitrary = genMessage
instance Arbitrary Message__DeletedPost where arbitrary = genMessage__DeletedPost
instance Arbitrary Message__Posted where arbitrary = genMessage__Posted
instance Arbitrary PostId where arbitrary = genPostId

instance Arbitrary Messages where
    arbitrary = sized $ \s -> foldr addMessage noMessages <$> vectorOf s arbitrary

instance Arbitrary RetrogradeMessages where
    arbitrary = reverseMessages <$> arbitrary
