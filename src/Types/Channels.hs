{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}

module Types.Channels
  ( ClientChannel(..)
  , ChannelContents(..)
  , ChannelInfo(..)
  , ChannelState(..)
  , ClientChannels -- constructor remains internal
  -- * Lenses created for accessing ClientChannel fields
  , ccContents, ccInfo
  -- * Lenses created for accessing ChannelInfo fields
  , cdViewed, cdUpdated, cdName, cdHeader, cdType, cdCurrentState
  , cdNewMessageCutoff, cdHasMentions
  -- * Lenses created for accessing ChannelContents fields
  , cdMessages
  -- * Creating ClientChannel objects
  , makeClientChannel
  -- * Managing ClientChannel collections
  , noChannels, addChannel, findChannelById, modifyChannelById
  , channelByIdL, maybeChannelByIdL
  , filteredChannelIds
  , filteredChannels
  -- * Creating ChannelInfo objects
  , channelInfoFromChannelWithData
  -- * Miscellaneous channel-related operations
  , canLeaveChannel
  , preferredChannelName
  )
where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import           Data.Time.Clock (UTCTime)
import           Lens.Micro.Platform
import           Network.Mattermost.Lenses hiding (Lens')
import           Network.Mattermost.Types ( Channel(..), ChannelId
                                          , ChannelWithData(..)
                                          , Type(..)
                                          )
import           Types.Messages (Messages, noMessages)

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

initialChannelInfo :: Channel -> ChannelInfo
initialChannelInfo chan =
    let updated  = chan ^. channelLastPostAtL
    in ChannelInfo { _cdViewed           = Nothing
                   , _cdHasMentions      = False
                   , _cdUpdated          = updated
                   , _cdName             = preferredChannelName chan
                   , _cdHeader           = chan^.channelHeaderL
                   , _cdType             = chan^.channelTypeL
                   , _cdCurrentState     = ChanUnloaded
                   , _cdNewMessageCutoff = Nothing
                   }

channelInfoFromChannelWithData :: ChannelWithData -> ChannelInfo -> ChannelInfo
channelInfoFromChannelWithData (ChannelWithData chan chanData) ci =
    let viewed   = chanData ^. channelDataLastViewedAtL
        updated  = chan ^. channelLastPostAtL
    in ci { _cdViewed           = Just viewed
          , _cdUpdated          = updated
          , _cdName             = preferredChannelName chan
          , _cdHeader           = (chan^.channelHeaderL)
          , _cdType             = (chan^.channelTypeL)
          }

-- | The 'ChannelContents' is a wrapper for a list of
--   'Message' values
data ChannelContents = ChannelContents
  { _cdMessages :: Messages
  }

-- | An initial empty 'ChannelContents' value
emptyChannelContents :: ChannelContents
emptyChannelContents = ChannelContents
  { _cdMessages = noMessages
  }

-- | The 'ChannelState' represents our internal state
--   of the channel with respect to our knowledge (or
--   lack thereof) about the server's information
--   about the channel.
data ChannelState
  = ChanUnloaded
  | ChanLoaded
  | ChanLoadPending
  | ChanRefreshing
    deriving (Eq, Show)

-- | The 'ChannelInfo' record represents metadata
--   about a channel
data ChannelInfo = ChannelInfo
  { _cdViewed           :: Maybe UTCTime
    -- ^ The last time we looked at a channel
  , _cdHasMentions      :: Bool
    -- ^ True if there are unread user mentions
  , _cdUpdated          :: UTCTime
    -- ^ The last time a message showed up in the channel
  , _cdName             :: T.Text
    -- ^ The name of the channel
  , _cdHeader           :: T.Text
    -- ^ The header text of a channel
  , _cdType             :: Type
    -- ^ The type of a channel: public, private, or DM
  , _cdCurrentState     :: ChannelState
    -- ^ The current state of the channel
  , _cdNewMessageCutoff :: Maybe UTCTime
    -- ^ The last time we looked at the new messages in
    --   this channel, if ever
  }

-- ** Channel-related Lenses

makeLenses ''ChannelContents
makeLenses ''ChannelInfo
makeLenses ''ClientChannel

-- ** Miscellaneous channel operations

makeClientChannel :: Channel -> ClientChannel
makeClientChannel nc = ClientChannel
  { _ccContents = emptyChannelContents
  , _ccInfo = initialChannelInfo nc
  }

canLeaveChannel :: ChannelInfo -> Bool
canLeaveChannel cInfo = not $ cInfo^.cdType `elem` [Direct, Group]

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

-- | Extract a specific user from the collection and perform an
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
