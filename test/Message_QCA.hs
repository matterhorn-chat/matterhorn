module Message_QCA where

import Cheapskate_QCA ()
import Network.Mattermost.QuickCheck ()
import Test.Tasty.QuickCheck
import Types
import Types.Posts

instance Arbitrary Message where
    arbitrary = Message <$> arbitrary <*> arbitrary <*> arbitrary
                <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

-- Some tests specifically want deleted or non-deleted messages, so
-- make an easy way to specify these.
newtype Message__DeletedPost  = Message__DeletedPost { delMsg :: Message }
    deriving Show

instance Arbitrary Message__DeletedPost where
    arbitrary = Message__DeletedPost <$>
                (Message
                 <$> arbitrary <*> arbitrary <*> arbitrary
                 <*> arbitrary <*> arbitrary
                 <*> return True  -- mDeleted
                 <*> arbitrary
                 <*> arbitrary
                 <*> (Just <$> arbitrary)  -- must have been Posted if deleted
                 <*> arbitrary <*> arbitrary)

newtype Message__Posted = Message__Posted { postMsg :: Message }
    deriving Show

instance Arbitrary Message__Posted where
    arbitrary = Message__Posted <$>
                (Message
                 <$> arbitrary <*> arbitrary <*> arbitrary
                 <*> arbitrary <*> arbitrary
                 <*> return False  -- mDeleted
                 <*> arbitrary
                 <*> arbitrary
                 <*> (Just <$> arbitrary)  -- mPostId
                 <*> arbitrary <*> arbitrary)


instance Arbitrary MessageType where
    arbitrary = oneof [ C <$> arbitrary
                      , CP <$> arbitrary
                      ]

instance Arbitrary ClientMessageType where
    arbitrary = elements [ Informative
                         , Error
                         , DateTransition
                         , NewMessagesTransition
                         ]

instance Arbitrary PostType where
    arbitrary = elements [ NormalPost
                         , Emote
                         , Join
                         , Leave
                         , TopicChange
                         ]

instance Arbitrary ReplyState where
    arbitrary = oneof [ return NotAReply
                      , ParentLoaded <$> arbitrary <*> arbitrary
                      , ParentNotLoaded <$> arbitrary
                      ]

instance Arbitrary Attachment where
    arbitrary = Attachment <$> arbitrary <*> arbitrary <*> arbitrary
