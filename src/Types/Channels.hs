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
  , EphemeralEditState(..)
  , EditMode(..)
  , eesMultiline, eesInputHistoryPosition, eesLastInput
  , defaultEphemeralEditState
  -- * Lenses created for accessing ClientChannel fields
  , ccContents, ccInfo, ccEditState
  -- * Lenses created for accessing ChannelInfo fields
  , cdViewed, cdNewMessageIndicator, cdEditedMessageThreshold, cdUpdated
  , cdName, cdDisplayName, cdHeader, cdPurpose, cdType
  , cdMentionCount, cdTypingUsers, cdDMUserId, cdChannelId
  , cdSidebarShowOverride, cdNotifyProps
  -- * Lenses created for accessing ChannelContents fields
  , cdMessages, cdFetchPending
  -- * Creating ClientChannel objects
  , makeClientChannel
  -- * Managing ClientChannel collections
  , noChannels, addChannel, removeChannel, findChannelById, modifyChannelById
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
  , getDmChannelFor
  , allDmChannelMappings
  , getChannelNameSet
  )
where

import           Prelude ()
import           Prelude.MH

import qualified Data.HashMap.Strict as HM
import qualified Data.Set as S
import qualified Data.Text as T
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
                                          , emptyChannelNotifyProps
                                          )

import           Types.Messages ( Messages, noMessages, addMessage
                                , clientMessageToMessage, Message, MessageType )
import           Types.Posts ( ClientMessageType(UnknownGapBefore)
                             , newClientMessage )
import           Types.Users ( TypingUsers, noTypingUsers, addTypingUser )
import           Types.Common


-- * Channel representations

-- | A 'ClientChannel' contains both the message
--   listing and the metadata about a channel
data ClientChannel = ClientChannel
  { _ccContents :: ChannelContents
    -- ^ A list of 'Message's in the channel
  , _ccInfo :: ChannelInfo
    -- ^ The 'ChannelInfo' for the channel
  , _ccEditState :: EphemeralEditState
    -- ^ Editor state that we swap in and out as the current channel is
    -- changed.
  }

-- | The input state associated with the message editor.
data EditMode =
    NewPost
    -- ^ The input is for a new post.
    | Editing Post MessageType
    -- ^ The input is ultimately to replace the body of an existing post
    -- of the specified type.
    | Replying Message Post
    -- ^ The input is to be used as a new post in reply to the specified
    -- post.
    deriving (Show)

data EphemeralEditState =
    EphemeralEditState { _eesMultiline :: Bool
                       -- ^ Whether the editor is in multiline mode
                       , _eesInputHistoryPosition :: Maybe Int
                       -- ^ The input history position, if any
                       , _eesLastInput :: (T.Text, EditMode)
                       -- ^ The input entered into the text editor last
                       -- time the user was focused on the channel
                       -- associated with this state.
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
                   , _cdTypingUsers            = noTypingUsers
                   , _cdDMUserId               = if chan^.channelTypeL == Direct
                                                 then userIdForDMChannel myId $
                                                      sanitizeUserText $ channelName chan
                                                 else Nothing
                   , _cdSidebarShowOverride    = Nothing
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

-- | The 'ChannelContents' is a wrapper for a list of
--   'Message' values
data ChannelContents = ChannelContents
  { _cdMessages :: Messages
  , _cdFetchPending :: Bool
  }

-- | An initial empty 'ChannelContents' value.  This also contains an
-- UnknownGapBefore, which is a signal that causes actual content fetching.
-- The initial Gap's timestamp is the local client time, but
-- subsequent fetches will synchronize with the server (and eventually
-- eliminate this Gap as well).
emptyChannelContents :: MonadIO m => m ChannelContents
emptyChannelContents = do
  gapMsg <- clientMessageToMessage <$> newClientMessage UnknownGapBefore "--Fetching messages--"
  return $ ChannelContents { _cdMessages = addMessage gapMsg noMessages
                           , _cdFetchPending = False
                           }


------------------------------------------------------------------------

-- | The 'ChannelInfo' record represents metadata
--   about a channel
data ChannelInfo = ChannelInfo
  { _cdChannelId        :: ChannelId
    -- ^ The channel's ID
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
  , _cdTypingUsers      :: TypingUsers
    -- ^ The users who are currently typing in this channel
  , _cdDMUserId         :: Maybe UserId
    -- ^ The user associated with this channel, if it is a DM channel
  , _cdSidebarShowOverride :: Maybe UTCTime
    -- ^ If set, show this channel in the sidebar regardless of other
    -- considerations as long as the specified timestamp meets a cutoff.
    -- Otherwise fall back to other application policy to determine
    -- whether to show the channel.
  }

-- ** Channel-related Lenses

makeLenses ''ChannelContents
makeLenses ''ChannelInfo
makeLenses ''ClientChannel
makeLenses ''EphemeralEditState

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
makeClientChannel myId nc = emptyChannelContents >>= \contents ->
  return ClientChannel
  { _ccContents = contents
  , _ccInfo = initialChannelInfo myId nc
  , _ccEditState = defaultEphemeralEditState
  }

defaultEphemeralEditState :: EphemeralEditState
defaultEphemeralEditState =
    EphemeralEditState { _eesMultiline = False
                       , _eesInputHistoryPosition = Nothing
                       , _eesLastInput = ("", NewPost)
                       }

canLeaveChannel :: ChannelInfo -> Bool
canLeaveChannel cInfo = not $ cInfo^.cdType `elem` [Direct]

-- ** Manage the collection of all Channels

-- | Define a binary kinded type to allow derivation of functor.
data AllMyChannels a =
    AllChannels { _chanMap :: HashMap ChannelId a
                , _userChannelMap :: HashMap UserId ChannelId
                , _channelNameSet :: S.Set Text
                }
                deriving (Functor, Foldable, Traversable)

-- | Define the exported typename which universally binds the
-- collection to the ChannelInfo type.
type ClientChannels = AllMyChannels ClientChannel

makeLenses ''AllMyChannels

getChannelNameSet :: ClientChannels -> S.Set Text
getChannelNameSet = _channelNameSet

-- | Initial collection of Channels with no members
noChannels :: ClientChannels
noChannels = AllChannels HM.empty HM.empty mempty

-- | Add a channel to the existing collection.
addChannel :: ChannelId -> ClientChannel -> ClientChannels -> ClientChannels
addChannel cId cinfo =
    (chanMap %~ HM.insert cId cinfo) .
    (if cinfo^.ccInfo.cdType `notElem` [Direct, Group]
     then channelNameSet %~ S.insert (cinfo^.ccInfo.cdName)
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
            Just ch -> channelNameSet %~ S.delete (ch^.ccInfo.cdName)
    in cs & chanMap %~ HM.delete cId
          & removeChannelName
          & userChannelMap %~ HM.filter (/= cId)

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

-- | Filter the ClientChannel collection, keeping only those for which
-- the provided filter test function returns True.
filteredChannels :: ((ChannelId, ClientChannel) -> Bool)
                 -> ClientChannels -> [(ChannelId, ClientChannel)]
filteredChannels f cc = filter f $ cc^.chanMap.to HM.toList

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
isTownSquare c = (sanitizeUserText $ c^.channelNameL) == "town-square"

channelDeleted :: Channel -> Bool
channelDeleted c = c^.channelDeleteAtL > c^.channelCreateAtL
