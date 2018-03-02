{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}

module Types.Channels
  ( ClientChannel(..)
  , ChannelContents(..)
  , ChannelInfo(..)
  , ClientChannels -- constructor remains internal
  , NewMessageIndicator(..)
  -- * Lenses created for accessing ClientChannel fields
  , ccContents, ccInfo
  -- * Lenses created for accessing ChannelInfo fields
  , cdViewed, cdNewMessageIndicator, cdEditedMessageThreshold, cdUpdated
  , cdName, cdHeader, cdPurpose, cdType
  , cdMentionCount, cdTypingUsers
  -- * Lenses created for accessing ChannelContents fields
  , cdMessages, cdFetchPending
  -- * Creating ClientChannel objects
  , makeClientChannel
  -- * Managing ClientChannel collections
  , noChannels, addChannel, findChannelById, modifyChannelById
  , channelByIdL, maybeChannelByIdL
  , filteredChannelIds
  , filteredChannels
  -- * Creating ChannelInfo objects
  , channelInfoFromChannelWithData
  -- * Channel State management
  , clearNewMessageIndicator
  , clearEditedThreshold
  , adjustUpdated
  , adjustEditedThreshold
  , updateNewMessageIndicator
  , addChannelTypingUser
  -- * Notification settings
  , notifyPreference
  -- * Miscellaneous channel-related operations
  , canLeaveChannel
  , preferredChannelName
  , isTownSquare
  , channelDeleted
  )
where

import           Control.Monad.IO.Class (MonadIO)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import           Data.Time.Clock (UTCTime)
import           Lens.Micro.Platform
import           Network.Mattermost.Lenses hiding (Lens')
import           Network.Mattermost.Types ( Channel(..), UserId, ChannelId
                                          , ChannelMember(..)
                                          , Type(..)
                                          , Post
                                          , User(userNotifyProps)
                                          , ChannelNotifyProps
                                          , NotifyOption(..)
                                          , WithDefault(..)
                                          , ServerTime
                                          , emptyChannelNotifyProps
                                          )
import           Types.Messages (Messages, noMessages, addMessage, clientMessageToMessage)
import           Types.Posts (ClientMessageType(UnknownGap), newClientMessage)
import           Types.Users (TypingUsers, noTypingUsers, addTypingUser)

-- * Channel representations

-- | A 'ClientChannel' contains both the message
--   listing and the metadata about a channel
data ClientChannel = ClientChannel
  { _ccContents :: ChannelContents
    -- ^ A list of 'Message's in the channel
  , _ccInfo     :: ChannelInfo
    -- ^ The 'ChannelInfo' for the channel
  }

-- Get a channel's name, depending on its type
preferredChannelName :: Channel -> T.Text
preferredChannelName ch
    | channelType ch == Group = channelDisplayName ch
    | otherwise = channelName ch

data NewMessageIndicator =
    Hide
    | NewPostsAfterServerTime ServerTime
    | NewPostsStartingAt ServerTime
    deriving (Eq, Show)

initialChannelInfo :: Channel -> ChannelInfo
initialChannelInfo chan =
    let updated  = chan ^. channelLastPostAtL
    in ChannelInfo { _cdViewed                 = Nothing
                   , _cdNewMessageIndicator    = Hide
                   , _cdEditedMessageThreshold = Nothing
                   , _cdMentionCount           = 0
                   , _cdUpdated                = updated
                   , _cdName                   = preferredChannelName chan
                   , _cdHeader                 = chan^.channelHeaderL
                   , _cdPurpose                = chan^.channelPurposeL
                   , _cdType                   = chan^.channelTypeL
                   , _cdNotifyProps            = emptyChannelNotifyProps
                   , _cdTypingUsers            = noTypingUsers
                   }

channelInfoFromChannelWithData :: Channel -> ChannelMember -> ChannelInfo -> ChannelInfo
channelInfoFromChannelWithData chan chanMember ci =
    let viewed   = chanMember ^. to channelMemberLastViewedAt
        updated  = chan ^. channelLastPostAtL
    in ci { _cdViewed           = Just viewed
          , _cdNewMessageIndicator = case _cdNewMessageIndicator ci of
              Hide -> if updated > viewed then NewPostsAfterServerTime viewed else Hide
              v -> v
          , _cdUpdated          = updated
          , _cdName             = preferredChannelName chan
          , _cdHeader           = (chan^.channelHeaderL)
          , _cdPurpose          = (chan^.channelPurposeL)
          , _cdType             = (chan^.channelTypeL)
          , _cdMentionCount     = chanMember^.to channelMemberMentionCount
          , _cdNotifyProps      = chanMember^.to channelMemberNotifyProps
          }

-- | The 'ChannelContents' is a wrapper for a list of
--   'Message' values
data ChannelContents = ChannelContents
  { _cdMessages :: Messages
  , _cdFetchPending :: Bool
  }

-- | An initial empty 'ChannelContents' value.  This also contains an
-- UnknownGap, which is a signal that causes actual content fetching.
-- The initial Gap's timestamp is the local client time, but
-- subsequent fetches will synchronize with the server (and eventually
-- eliminate this Gap as well).
emptyChannelContents :: MonadIO m => m ChannelContents
emptyChannelContents = do
  gapMsg <- clientMessageToMessage <$> newClientMessage UnknownGap "--Fetching messages--"
  return $ ChannelContents { _cdMessages = addMessage gapMsg noMessages
                           , _cdFetchPending = False
                           }


------------------------------------------------------------------------

-- | The 'ChannelInfo' record represents metadata
--   about a channel
data ChannelInfo = ChannelInfo
  { _cdViewed           :: Maybe ServerTime
    -- ^ The last time we looked at a channel
  , _cdNewMessageIndicator :: NewMessageIndicator
    -- ^ The state of the channel's new message indicator.
  , _cdEditedMessageThreshold :: Maybe ServerTime
    -- ^ The channel's edited message threshold.
  , _cdMentionCount     :: Int
    -- ^ The current number of unread mentions
  , _cdUpdated          :: ServerTime
    -- ^ The last time a message showed up in the channel
  , _cdName             :: T.Text
    -- ^ The name of the channel
  , _cdHeader           :: T.Text
    -- ^ The header text of a channel
  , _cdPurpose          :: T.Text
    -- ^ The stated purpose of the channel
  , _cdType             :: Type
    -- ^ The type of a channel: public, private, or DM
  , _cdNotifyProps      :: ChannelNotifyProps
    -- ^ The user's notification settings for this channel
  , _cdTypingUsers      :: TypingUsers
    -- ^ The users who are currently typing in this channel
  }

-- ** Channel-related Lenses

makeLenses ''ChannelContents
makeLenses ''ChannelInfo
makeLenses ''ClientChannel

notifyPreference :: User -> ClientChannel -> NotifyOption
notifyPreference u cc =
    case cc^.ccInfo.cdNotifyProps.channelNotifyPropsDesktopL of
        IsValue v -> v
        Default   -> (userNotifyProps u)^.userNotifyPropsDesktopL

-- ** Miscellaneous channel operations

makeClientChannel :: (MonadIO m) => Channel -> m ClientChannel
makeClientChannel nc = emptyChannelContents >>= \contents ->
  return ClientChannel
  { _ccContents = contents
  , _ccInfo = initialChannelInfo nc
  }

canLeaveChannel :: ChannelInfo -> Bool
canLeaveChannel cInfo = not $ cInfo^.cdType `elem` [Direct]

-- ** Manage the collection of all Channels

-- | Define a binary kinded type to allow derivation of functor.
newtype AllMyChannels a = AllChannels { _ofChans :: HM.HashMap ChannelId a }
    deriving (Functor, Foldable, Traversable)

-- | Define the exported typename which universally binds the
-- collection to the ChannelInfo type.
type ClientChannels = AllMyChannels ClientChannel

makeLenses ''AllMyChannels

-- | Initial collection of Channels with no members
noChannels :: ClientChannels
noChannels = AllChannels HM.empty

-- | Add a channel to the existing collection.
addChannel :: ChannelId -> ClientChannel -> ClientChannels -> ClientChannels
addChannel cId cinfo = AllChannels . HM.insert cId cinfo . _ofChans

-- | Get the ChannelInfo information given the ChannelId
findChannelById :: ChannelId -> ClientChannels -> Maybe ClientChannel
findChannelById cId = HM.lookup cId . _ofChans

-- | Extract a specific channel from the collection and perform an
-- endomorphism operation on it, then put it back into the collection.
modifyChannelById :: ChannelId -> (ClientChannel -> ClientChannel)
                  -> ClientChannels -> ClientChannels
modifyChannelById cId f = ofChans.ix(cId) %~ f

-- | A 'Traversal' that will give us the 'ClientChannel' in a
-- 'ClientChannels' structure if it exists
channelByIdL :: ChannelId -> Traversal' ClientChannels ClientChannel
channelByIdL cId = ofChans . ix cId

-- | A 'Lens' that will give us the 'ClientChannel' in a
-- 'ClientChannels' wrapped in a 'Maybe'
maybeChannelByIdL :: ChannelId -> Lens' ClientChannels (Maybe ClientChannel)
maybeChannelByIdL cId = ofChans . at cId

-- | Apply a filter to each ClientChannel and return a list of the
-- ChannelId values for which the filter matched.
filteredChannelIds :: (ClientChannel -> Bool) -> ClientChannels -> [ChannelId]
filteredChannelIds f cc = fst <$> filter (f . snd) (HM.toList (cc^.ofChans))

-- | Filter the ClientChannel collection, keeping only those for which
-- the provided filter test function returns True.
filteredChannels :: ((ChannelId, ClientChannel) -> Bool)
                 -> ClientChannels -> ClientChannels
filteredChannels f cc =
    AllChannels . HM.fromList . filter f $ cc^.ofChans.to HM.toList

------------------------------------------------------------------------

-- * Channel State management


-- | Add user to the list of users in this channel who are currently typing.
addChannelTypingUser :: UserId -> UTCTime -> ClientChannel -> ClientChannel
addChannelTypingUser uId ts = ccInfo.cdTypingUsers %~ (addTypingUser uId ts)

-- | Clear the new message indicator for the specified channel
clearNewMessageIndicator :: ClientChannel -> ClientChannel
clearNewMessageIndicator c = c & ccInfo.cdNewMessageIndicator .~ Hide

-- | Clear the edit threshold for the specified channel
clearEditedThreshold :: ClientChannel -> ClientChannel
clearEditedThreshold c = c & ccInfo.cdEditedMessageThreshold .~ Nothing

-- | Adjust updated time based on a message, ensuring that the updated
-- time does not move backward.
adjustUpdated :: Post -> ClientChannel -> ClientChannel
adjustUpdated m =
    ccInfo.cdUpdated %~ max (maxPostTimestamp m)

adjustEditedThreshold :: Post -> ClientChannel -> ClientChannel
adjustEditedThreshold m c =
    if m^.postUpdateAtL <= m^.postCreateAtL
    then c
    else c & ccInfo.cdEditedMessageThreshold %~ (\mt -> case mt of
        Just t -> Just $ min (m^.postUpdateAtL) t
        Nothing -> Just $ m^.postUpdateAtL
        )

maxPostTimestamp :: Post -> ServerTime
maxPostTimestamp m = max (m^.postDeleteAtL . non (m^.postUpdateAtL)) (m^.postCreateAtL)

updateNewMessageIndicator :: Post -> ClientChannel -> ClientChannel
updateNewMessageIndicator m =
    ccInfo.cdNewMessageIndicator %~
        (\old ->
          case old of
              Hide ->
                  NewPostsStartingAt $ m^.postCreateAtL
              NewPostsStartingAt ts ->
                  NewPostsStartingAt $ min (m^.postCreateAtL) ts
              NewPostsAfterServerTime ts ->
                  if m^.postCreateAtL <= ts
                  then NewPostsStartingAt $ m^.postCreateAtL
                  else NewPostsAfterServerTime ts
              )

-- | Town Square is special in that its non-display name cannot be
-- changed and is a hard-coded constant server-side according to the
-- developers (as of 8/2/17). So this is a reliable way to check for
-- whether a channel is in fact that channel, even if the user has
-- changed its display name.
isTownSquare :: Channel -> Bool
isTownSquare c = c^.channelNameL == "town-square"

channelDeleted :: Channel -> Bool
channelDeleted c = c^.channelDeleteAtL > c^.channelCreateAtL
