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
  , NewMessageIndicator(..)
  -- * Lenses created for accessing ClientChannel fields
  , ccContents, ccInfo
  -- * Lenses created for accessing ChannelInfo fields
  , cdViewed, cdNewMessageIndicator, cdEditedMessageThreshold, cdUpdated
  , cdName, cdHeader, cdType, cdCurrentState
  , cdMentionCount
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
  -- * Channel State management
  , initialChannelState
  , loadingChannelContentState
  , isPendingState
  , pendingChannelState
  , quiescentChannelState
  , clearNewMessageIndicator
  , clearEditedThreshold
  , adjustUpdated
  , adjustEditedThreshold
  , updateNewMessageIndicator
  -- * Notification settings
  , notifyPreference
  -- * Miscellaneous channel-related operations
  , canLeaveChannel
  , preferredChannelName
  , isTownSquare
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
                                          , Post
                                          , User(userNotifyProps)
                                          , ChannelNotifyProps
                                          , NotifyOption(..)
                                          , WithDefault(..)
                                          , emptyChannelNotifyProps
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

data NewMessageIndicator =
    Hide
    | NewPostsAfterServerTime UTCTime
    | NewPostsStartingAt UTCTime
    deriving (Eq, Show)

initialChannelInfo :: Channel -> ChannelInfo
initialChannelInfo chan =
    let updated  = chan ^. channelLastPostAtL
    in ChannelInfo { _cdViewed           = Nothing
                   , _cdNewMessageIndicator = Hide
                   , _cdEditedMessageThreshold = Nothing
                   , _cdMentionCount     = 0
                   , _cdUpdated          = updated
                   , _cdName             = preferredChannelName chan
                   , _cdHeader           = chan^.channelHeaderL
                   , _cdType             = chan^.channelTypeL
                   , _cdCurrentState     = initialChannelState
                   , _cdNotifyProps      = emptyChannelNotifyProps
                   }

channelInfoFromChannelWithData :: ChannelWithData -> ChannelInfo -> ChannelInfo
channelInfoFromChannelWithData (ChannelWithData chan chanData) ci =
    let viewed   = chanData ^. channelDataLastViewedAtL
        updated  = chan ^. channelLastPostAtL
    in ci { _cdViewed           = Just viewed
          , _cdNewMessageIndicator = case _cdNewMessageIndicator ci of
              Hide -> if updated > viewed then NewPostsAfterServerTime viewed else Hide
              v -> v
          , _cdUpdated          = updated
          , _cdName             = preferredChannelName chan
          , _cdHeader           = (chan^.channelHeaderL)
          , _cdType             = (chan^.channelTypeL)
          , _cdMentionCount     = chanData^.channelDataMentionCountL
          , _cdNotifyProps      = chanData^.channelDataNotifyPropsL
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

------------------------------------------------------------------------

-- * Channel State management

-- | The 'ChannelState' represents our internal state
--   of the channel with respect to our knowledge (or
--   lack thereof) about the server's information
--   about the channel.
data ChannelState
  = ChanGettingInfo    -- ^ (Re-) fetching for an unloaded channel
  | ChanUnloaded       -- ^ Only have channel metadata
  | ChanGettingPosts   -- ^ (Re-) fetching for a loaded channel
  | ChanInitialSelect  -- ^ Initially selected channel, but not contents yet
  | ChanLoaded         -- ^ Have channel metadata and contents
    deriving (Eq, Show)

-- The ChannelState may be affected by background operations (see
-- asyncIO), so state transitions may be suggested by the completion
-- of those asynchronous operations but they should be reconciled with
-- any other asynchronous (or foreground) state updates.
--
-- To facilitate this, the ChannelState is a member of Ord and Enum,
-- with the intention that states generally transition to higher state
-- values (indicating more knowledge/completeness) and not downwards,
-- allowing the use of `max` and `pred` and `succ` below for managing
-- state changes.  The `Ord` and `Enum` membership is merely for local
-- convenience: all state management should be handled by the methods
-- below so more detailed methods can be used instead of the Ord and
-- Enum instances without impact to external code.

-- | Specifies the initial state for created ClientChannel objects.
initialChannelState :: ChannelState
initialChannelState = ChanUnloaded

-- | The state to use to indicate that channel content (i.e.,
-- messages) are being loaded (possibly asynchronously).  In contrast
-- to the `pendingChannelState` function, this function is used when
-- the channel is transitioning from only having the metadata
-- information to having full content information.
loadingChannelContentState :: ChannelState
loadingChannelContentState = ChanGettingPosts

-- | The pendingChannelState specifies the new ChannelState to
-- represent an active fetch of information for a channel, given the
-- channel's current state.  This is used when the existing
-- information is being refreshed.  The return is a tuple of the new
-- state and a function to call after the async operation has finished
-- with the ChannelState at that time and which will return the new
-- state that should be set on that completion.
pendingChannelState :: ChannelState -> (ChannelState,
                                        (ChannelState -> ChannelState))
pendingChannelState ChanGettingInfo = (ChanGettingInfo, id)
pendingChannelState ChanGettingPosts = (ChanGettingPosts, id)
pendingChannelState ChanUnloaded = (ChanGettingInfo, quiescentChannelState ChanUnloaded)
pendingChannelState ChanLoaded = (ChanGettingPosts, quiescentChannelState ChanLoaded)
pendingChannelState ChanInitialSelect = (ChanGettingPosts, quiescentChannelState ChanInitialSelect)

-- | The completionChannelState specifies the new ChannelState upon
-- completion of an activity.  The activity is represented by the
-- first argument, which is the pendingState.  The channel may have
-- been updated in the interim by other activities as well, so the
-- currentState is also provided.  This function determines the proper
-- new state of the channel based on the action and current state.  In
-- the event that multiple update operations are performed at the same
-- time, the state should always reach higher resting states.
quiescentChannelState :: ChannelState -> ChannelState -> ChannelState
quiescentChannelState targetState currentState =
  if isPendingState targetState
  then currentState
  else case (currentState, targetState) of
         (ChanLoaded,        ChanUnloaded) -> ChanLoaded
         (ChanGettingPosts,  ChanUnloaded) -> ChanGettingPosts
         (ChanInitialSelect, ChanUnloaded) -> ChanInitialSelect
         (_, t) -> t

-- | Returns true if the channel's state is one where there is a
-- pending asynchronous update already scheduled.
isPendingState :: ChannelState -> Bool
isPendingState cstate = cstate `elem` [ ChanGettingPosts
                                      , ChanGettingInfo
                                      ]

------------------------------------------------------------------------

-- | The 'ChannelInfo' record represents metadata
--   about a channel
data ChannelInfo = ChannelInfo
  { _cdViewed           :: Maybe UTCTime
    -- ^ The last time we looked at a channel
  , _cdNewMessageIndicator :: NewMessageIndicator
    -- ^ The state of the channel's new message indicator.
  , _cdEditedMessageThreshold :: Maybe UTCTime
    -- ^ The channel's edited message threshold.
  , _cdMentionCount     :: Int
    -- ^ The current number of unread mentions
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
  , _cdNotifyProps      :: ChannelNotifyProps
    -- ^ The user's notification settings for this channel
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

maxPostTimestamp :: Post -> UTCTime
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
