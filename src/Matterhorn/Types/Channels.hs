{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}

module Matterhorn.Types.Channels
  ( ClientChannel(..)
  , ChannelInfo(..)
  , ClientChannels -- constructor remains internal
  , NewMessageIndicator(..)
  -- * Lenses created for accessing ClientChannel fields
  , ccMessages, ccInfo, ccEphemeralEditState
  -- * Lenses created for accessing ChannelInfo fields
  , cdViewed, cdNewMessageIndicator, cdEditedMessageThreshold, cdUpdated
  , cdName, cdDisplayName, cdHeader, cdPurpose, cdType
  , cdMentionCount, cdDMUserId, cdChannelId
  , cdSidebarShowOverride, cdNotifyProps, cdTeamId, cdFetchPending
  -- * Creating ClientChannel objects
  , makeClientChannel
  -- * Managing ClientChannel collections
  , noChannels, addChannel, removeChannel, findChannelById, modifyChannelById
  , channelByIdL, maybeChannelByIdL
  , allTeamIds
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
  -- * Notification settings
  , notifyPreference
  , isMuted
  , channelNotifyPropsMarkUnreadL
  , channelNotifyPropsIgnoreChannelMentionsL
  , channelNotifyPropsDesktopL
  , channelNotifyPropsPushL
  -- * Miscellaneous channel-related operations
  , canLeaveChannel
  , preferredChannelName
  , isTownSquare
  , channelDeleted
  , getDmChannelFor
  , allDmChannelMappings
  , getChannelNameSet
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import qualified Data.HashMap.Strict as HM
import qualified Data.Set as S
import           Lens.Micro.Platform ( (%~), (.~), Traversal', Lens'
                                     , makeLenses, ix, at
                                     , to, non )

import           Network.Mattermost.Lenses hiding ( Lens' )
import           Network.Mattermost.Types ( Channel(..), UserId, ChannelId
                                          , ChannelMember(..)
                                          , Type(..)
                                          , Post
                                          , User(userNotifyProps)
                                          , ChannelNotifyProps
                                          , NotifyOption(..)
                                          , WithDefault(..)
                                          , ServerTime
                                          , TeamId
                                          , emptyChannelNotifyProps
                                          )

import           Matterhorn.Types.Messages ( Messages, noMessages, addMessage
                                           , clientMessageToMessage )
import           Matterhorn.Types.Posts ( ClientMessageType(UnknownGapBefore)
                                        , newClientMessage )
import           Matterhorn.Types.EditState
import           Matterhorn.Types.Common


-- * Channel representations

-- | A 'ClientChannel' contains both the message
--   listing and the metadata about a channel
data ClientChannel = ClientChannel
  { _ccMessages :: Messages
    -- ^ A list of 'Message's in the channel
  , _ccInfo :: ChannelInfo
    -- ^ The 'ChannelInfo' for the channel
  , _ccEphemeralEditState :: EphemeralEditState
    -- ^ Editor state that we swap in and out as the current channel is
    -- changed.
  }

-- Get a channel's name, depending on its type
preferredChannelName :: Channel -> Text
preferredChannelName ch
    | channelType ch == Group = sanitizeUserText $ channelDisplayName ch
    | otherwise               = sanitizeUserText $ channelName ch

data NewMessageIndicator =
    Hide
    | NewPostsAfterServerTime ServerTime
    | NewPostsStartingAt ServerTime
    deriving (Eq, Show)

initialChannelInfo :: UserId -> Channel -> ChannelInfo
initialChannelInfo myId chan =
    let updated  = chan ^. channelLastPostAtL
    in ChannelInfo { _cdChannelId              = chan^.channelIdL
                   , _cdTeamId                 = chan^.channelTeamIdL
                   , _cdViewed                 = Nothing
                   , _cdNewMessageIndicator    = Hide
                   , _cdEditedMessageThreshold = Nothing
                   , _cdMentionCount           = 0
                   , _cdUpdated                = updated
                   , _cdName                   = preferredChannelName chan
                   , _cdDisplayName            = sanitizeUserText $ channelDisplayName chan
                   , _cdHeader                 = sanitizeUserText $ chan^.channelHeaderL
                   , _cdPurpose                = sanitizeUserText $ chan^.channelPurposeL
                   , _cdType                   = chan^.channelTypeL
                   , _cdNotifyProps            = emptyChannelNotifyProps
                   , _cdDMUserId               = if chan^.channelTypeL == Direct
                                                 then userIdForDMChannel myId $
                                                      sanitizeUserText $ channelName chan
                                                 else Nothing
                   , _cdSidebarShowOverride    = Nothing
                   , _cdFetchPending           = False
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
          , _cdDisplayName      = sanitizeUserText $ channelDisplayName chan
          , _cdHeader           = (sanitizeUserText $ chan^.channelHeaderL)
          , _cdPurpose          = (sanitizeUserText $ chan^.channelPurposeL)
          , _cdType             = (chan^.channelTypeL)
          , _cdMentionCount     = chanMember^.to channelMemberMentionCount
          , _cdNotifyProps      = chanMember^.to channelMemberNotifyProps
          }

-- | An initial empty channel message list. This also contains an
-- UnknownGapBefore, which is a signal that causes actual content
-- fetching. The initial Gap's timestamp is the local client time, but
-- subsequent fetches will synchronize with the server (and eventually
-- eliminate this Gap as well).
emptyChannelMessages :: MonadIO m => m Messages
emptyChannelMessages = do
  gapMsg <- clientMessageToMessage <$> newClientMessage UnknownGapBefore "--Fetching messages--"
  return $ addMessage gapMsg noMessages

------------------------------------------------------------------------

-- | The 'ChannelInfo' record represents metadata
--   about a channel
data ChannelInfo = ChannelInfo
  { _cdChannelId        :: ChannelId
    -- ^ The channel's ID
  , _cdTeamId           :: Maybe TeamId
    -- ^ The channel's team ID
  , _cdViewed           :: Maybe ServerTime
    -- ^ The last time we looked at a channel
  , _cdNewMessageIndicator :: NewMessageIndicator
    -- ^ The state of the channel's new message indicator.
  , _cdEditedMessageThreshold :: Maybe ServerTime
    -- ^ The channel's edited message threshold.
  , _cdMentionCount     :: Int
    -- ^ The current number of unread mentions
  , _cdUpdated          :: ServerTime
    -- ^ The last time a message showed up in the channel
  , _cdName             :: Text
    -- ^ The name of the channel
  , _cdDisplayName      :: Text
    -- ^ The display name of the channel
  , _cdHeader           :: Text
    -- ^ The header text of a channel
  , _cdPurpose          :: Text
    -- ^ The stated purpose of the channel
  , _cdType             :: Type
    -- ^ The type of a channel: public, private, or DM
  , _cdNotifyProps      :: ChannelNotifyProps
    -- ^ The user's notification settings for this channel
  , _cdDMUserId         :: Maybe UserId
    -- ^ The user associated with this channel, if it is a DM channel
  , _cdSidebarShowOverride :: Maybe UTCTime
    -- ^ If set, show this channel in the sidebar regardless of other
    -- considerations as long as the specified timestamp meets a cutoff.
    -- Otherwise fall back to other application policy to determine
    -- whether to show the channel.
  , _cdFetchPending :: Bool
    -- ^ Whether a fetch in this channel is pending
  }

-- ** Channel-related Lenses

makeLenses ''ChannelInfo
makeLenses ''ClientChannel

isMuted :: ClientChannel -> Bool
isMuted cc = cc^.ccInfo.cdNotifyProps.channelNotifyPropsMarkUnreadL ==
             IsValue NotifyOptionMention

notifyPreference :: User -> ClientChannel -> NotifyOption
notifyPreference u cc =
    if isMuted cc then NotifyOptionNone
    else case cc^.ccInfo.cdNotifyProps.channelNotifyPropsDesktopL of
             IsValue v -> v
             Default   -> (userNotifyProps u)^.userNotifyPropsDesktopL

-- ** Miscellaneous channel operations

makeClientChannel :: (MonadIO m) => UserId -> Channel -> m ClientChannel
makeClientChannel myId nc = emptyChannelMessages >>= \msgs ->
  return ClientChannel
  { _ccMessages = msgs
  , _ccInfo = initialChannelInfo myId nc
  , _ccEphemeralEditState = defaultEphemeralEditState
  }

canLeaveChannel :: ChannelInfo -> Bool
canLeaveChannel cInfo = not $ cInfo^.cdType `elem` [Direct]

-- ** Manage the collection of all Channels

data ChannelCollection a =
    ChannelCollection { _chanMap :: HashMap ChannelId a
                      , _channelNameSet :: HashMap TeamId (S.Set Text)
                      , _userChannelMap :: HashMap UserId ChannelId
                      }
                      deriving (Functor, Foldable, Traversable)

type ClientChannels = ChannelCollection ClientChannel

makeLenses ''ChannelCollection

getChannelNameSet :: TeamId -> ClientChannels -> S.Set Text
getChannelNameSet tId cs = case cs^.channelNameSet.at tId of
    Nothing -> mempty
    Just s -> s

-- | Initial collection of Channels with no members
noChannels :: ClientChannels
noChannels =
    ChannelCollection { _chanMap = HM.empty
                      , _channelNameSet = HM.empty
                      , _userChannelMap = HM.empty
                      }

-- | Add a channel to the existing collection.
addChannel :: ChannelId -> ClientChannel -> ClientChannels -> ClientChannels
addChannel cId cinfo =
    (chanMap %~ HM.insert cId cinfo) .
    (if cinfo^.ccInfo.cdType `notElem` [Direct, Group]
     then case cinfo^.ccInfo.cdTeamId of
         Nothing -> id
         Just tId -> channelNameSet %~ HM.insertWith S.union tId (S.singleton $ cinfo^.ccInfo.cdName)
     else id) .
    (case cinfo^.ccInfo.cdDMUserId of
         Nothing -> id
         Just uId -> userChannelMap %~ HM.insert uId cId
    )

-- | Remove a channel from the collection.
removeChannel :: ChannelId -> ClientChannels -> ClientChannels
removeChannel cId cs =
    let mChan = findChannelById cId cs
        removeChannelName = case mChan of
            Nothing -> id
            Just ch -> case ch^.ccInfo.cdTeamId of
                Nothing -> id
                Just tId -> channelNameSet %~ HM.adjust (S.delete (ch^.ccInfo.cdName)) tId
    in cs & chanMap %~ HM.delete cId
          & removeChannelName
          & userChannelMap %~ HM.filter (/= cId)

instance Semigroup (ChannelCollection ClientChannel) where
    a <> b =
        let pairs = HM.toList $ _chanMap a
        in foldr (uncurry addChannel) b pairs

instance Monoid (ChannelCollection ClientChannel) where
    mempty = noChannels
#if !MIN_VERSION_base(4,11,0)
    mappend = (<>)
#endif

getDmChannelFor :: UserId -> ClientChannels -> Maybe ChannelId
getDmChannelFor uId cs = cs^.userChannelMap.at uId

allDmChannelMappings :: ClientChannels -> [(UserId, ChannelId)]
allDmChannelMappings = HM.toList . _userChannelMap

-- | Get the ChannelInfo information given the ChannelId
findChannelById :: ChannelId -> ClientChannels -> Maybe ClientChannel
findChannelById cId = HM.lookup cId . _chanMap

-- | Transform the specified channel in place with provided function.
modifyChannelById :: ChannelId -> (ClientChannel -> ClientChannel)
                  -> ClientChannels -> ClientChannels
modifyChannelById cId f = chanMap.ix(cId) %~ f

-- | A 'Traversal' that will give us the 'ClientChannel' in a
-- 'ClientChannels' structure if it exists
channelByIdL :: ChannelId -> Traversal' ClientChannels ClientChannel
channelByIdL cId = chanMap . ix cId

-- | A 'Lens' that will give us the 'ClientChannel' in a
-- 'ClientChannels' wrapped in a 'Maybe'
maybeChannelByIdL :: ChannelId -> Lens' ClientChannels (Maybe ClientChannel)
maybeChannelByIdL cId = chanMap . at cId

-- | Apply a filter to each ClientChannel and return a list of the
-- ChannelId values for which the filter matched.
filteredChannelIds :: (ClientChannel -> Bool) -> ClientChannels -> [ChannelId]
filteredChannelIds f cc = fst <$> filter (f . snd) (HM.toList (cc^.chanMap))

-- | Get all the team IDs in the channel collection.
allTeamIds :: ClientChannels -> [TeamId]
allTeamIds cc = HM.keys $ cc^.channelNameSet

-- | Filter the ClientChannel collection, keeping only those for which
-- the provided filter test function returns True.
filteredChannels :: ((ChannelId, ClientChannel) -> Bool)
                 -> ClientChannels -> [(ChannelId, ClientChannel)]
filteredChannels f cc = filter f $ cc^.chanMap.to HM.toList

------------------------------------------------------------------------

-- * Channel State management


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
isTownSquare c = (sanitizeUserText $ c^.channelNameL) == "town-square"

channelDeleted :: Channel -> Bool
channelDeleted c = c^.channelDeleteAtL > c^.channelCreateAtL
