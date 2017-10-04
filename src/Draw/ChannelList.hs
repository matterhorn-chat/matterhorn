{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}

-- | This module provides the Drawing functionality for the
-- ChannelList sidebar.  The sidebar is divided vertically into groups
-- and each group is rendered separately.
--
-- There are actually two UI modes handled by this code:
--
--   * Normal display of the channels, with various markers to
--     indicate the current channel, channels with unread messages,
--     user state (for Direct Message channels), etc.
--
--   * ChannelSelect display where the user is typing match characters
--     into a prompt at the ChannelList sidebar is showing only those
--     channels matching the entered text (and highlighting the
--     matching portion).

module Draw.ChannelList (renderChannelList) where

import           Brick
import           Brick.Widgets.Border
import qualified Data.HashMap.Strict as HM
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Draw.Util
import           Lens.Micro.Platform
import           State
import           Themes
import           Types
import           Types.Users
import           Types.Channels

type GroupName = T.Text

-- | Specify the different groups of channels to be displayed
-- vertically in the ChannelList sidebar.  This list provides the
-- central control over what channels are displayed and how they are
-- grouped.
--
-- Each group is specified as a tuple of:
--
--   * the name of this group
--
--   * A lens to get the HashMap of matching selections when in
--     ChannelSelect mode (ignored for Normal mode).
--
--   * The function to retrieve the list of channels for this group
--     from the ChatState.
channelListGroups :: [ ( GroupName
                       , Getting ChannelSelectMap ChatState ChannelSelectMap
                       , ChatState -> [ChannelListEntry]
                       ) ]
channelListGroups =
    [ ("Channels", csChannelSelectState.channelMatches, getOrdinaryChannels)
    , ("Users",    csChannelSelectState.userMatches,    getDmChannels)
    ]

-- | True if there is an active channel selection operation (i.e. in
-- ChannelSelect mode).  This requires both the state change *and*
-- some channel selection text.
hasActiveChannelSelection :: ChatState -> Bool
hasActiveChannelSelection st =
    st^.csMode == ChannelSelect && not (T.null (st^.csChannelSelectState.channelSelectInput))

-- | This is the main function that is called from external code to
-- render the ChannelList sidebar.
renderChannelList :: ChatState -> Widget Name
renderChannelList st =
    let maybeViewport = if hasActiveChannelSelection st
                        then id -- no viewport scrolling when actively selecting a channel
                        else viewport ChannelList Vertical
        selMatch = st^.csChannelSelectState.selectedMatch
        renderedGroups = if hasActiveChannelSelection st
                         then renderChannelGroup (renderChannelSelectListEntry selMatch) <$> selectedGroupEntries
                         else renderChannelGroup renderChannelListEntry       <$> plainGroupEntries
        plainGroupEntries (n, _m, f) = (n, f st)
        selectedGroupEntries (n, m, f) = (n, foldr (addSelectedChannel m) [] $ f st)
        addSelectedChannel m e s = case HM.lookup (entryLabel e) (st^.m) of
                                     Just y -> SCLE e y : s
                                     Nothing -> s
    in maybeViewport $ vBox $ concat $ renderedGroups <$> channelListGroups

-- | Renders a specific group, given the name of the group and the
-- list of entries in that group (which are expected to be either
-- ChannelListEntry or SelectedChannelListEntry elements).
renderChannelGroup :: (a -> Widget Name) -> (GroupName, [a]) -> [Widget Name]
renderChannelGroup eRender (groupName, entries) =
    let header label = hBorderWithLabel $ withDefAttr channelListHeaderAttr $ txt label
    in header groupName : (eRender <$> entries)

-- | Internal record describing each channel entry and its associated
-- attributes.  This is the object passed to the rendering function so
-- that it can determine how to render each channel.
data ChannelListEntry =
    ChannelListEntry { entrySigil       :: T.Text
                     , entryLabel       :: T.Text
                     , entryHasUnread   :: Bool
                     , entryMentions    :: Int
                     , entryIsRecent    :: Bool
                     , entryIsCurrent   :: Bool
                     , entryUserStatus  :: Maybe UserStatus
                     }

-- | Similar to the ChannelListEntry, but also holds information about
-- the matching channel select specification.
data SelectedChannelListEntry = SCLE ChannelListEntry ChannelSelectMatch

-- | Render an individual Channel List entry (in Normal mode) with
-- appropriate visual decorations.
renderChannelListEntry :: ChannelListEntry -> Widget Name
renderChannelListEntry entry =
    decorate $ decorateRecent entry $ decorateMentions $ padRight Max $
    entryWidget $ entrySigil entry <> entryLabel entry
    where
    decorate = if | entryIsCurrent entry ->
                      visible . forceAttr currentChannelNameAttr
                  | entryMentions entry > 0 ->
                      forceAttr mentionsChannelAttr
                  | entryHasUnread entry ->
                      forceAttr unreadChannelAttr
                  | otherwise -> id
    entryWidget = case entryUserStatus entry of
                    Just Offline -> withDefAttr clientMessageAttr . txt
                    Just _       -> colorUsername
                    Nothing      -> txt
    decorateMentions
      | entryMentions entry > 9 =
        (<+> str "(9+)")
      | entryMentions entry > 0 =
        (<+> str ("(" <> show (entryMentions entry) <> ")"))
      | otherwise = id


-- | Render an individual entry when in Channel Select mode,
-- highlighting the matching portion, or completely suppressing the
-- entry if it doesn't match.
renderChannelSelectListEntry :: T.Text -> SelectedChannelListEntry -> Widget Name
renderChannelSelectListEntry selMatch (SCLE entry match) =
    let ChannelSelectMatch preMatch inMatch postMatch = match
        fullName = channelNameFromMatch match
        maybeSelect = if fullName == selMatch
                      then withDefAttr currentChannelNameAttr
                      else id
    in maybeSelect $
       decorateRecent entry $
       padRight Max $
         hBox [ txt $ entrySigil entry
              , txt preMatch
              , forceAttr channelSelectMatchAttr $ txt inMatch
              , txt postMatch
              ]

-- | If this channel is the most recently viewed channel (prior to the
-- currently viewed channel), add a decoration to denote that.
decorateRecent :: ChannelListEntry -> Widget n -> Widget n
decorateRecent entry = if entryIsRecent entry
                       then (<+> (withDefAttr recentMarkerAttr $ str "<"))
                       else id

-- | Extract the names and information about normal channels to be
-- displayed in the ChannelList sidebar.
getOrdinaryChannels :: ChatState -> [ChannelListEntry]
getOrdinaryChannels st =
    [ ChannelListEntry sigil n unread mentions recent current Nothing
    | n <- (st ^. csNames . cnChans)
    , let Just chan = st ^. csNames . cnToChanId . at n
          unread = hasUnread st chan
          recent = Just chan == st^.csRecentChannel
          current = isCurrentChannel st chan
          sigil = case st ^. csEditState.cedLastChannelInput.at chan of
            Nothing      -> T.singleton normalChannelSigil
            Just ("", _) -> T.singleton normalChannelSigil
            _            -> "»"
          mentions = maybe 0 id (st^?csChannel(chan).ccInfo.cdMentionCount)
    ]

-- | Extract the names and information about Direct Message channels
-- to be displayed in the ChannelList sidebar.
getDmChannels :: ChatState -> [ChannelListEntry]
getDmChannels st =
    [ ChannelListEntry sigil uname unread mentions recent current (Just $ u^.uiStatus)
    | u <- sortedUserList st
    , let sigil =
            case do { cId <- m_chanId; st^.csEditState.cedLastChannelInput.at cId } of
              Nothing      -> T.singleton $ userSigilFromInfo u
              Just ("", _) -> T.singleton $ userSigilFromInfo u
              _            -> "»"  -- shows that user has a message in-progress
          uname = u^.uiName
          recent = maybe False ((== st^.csRecentChannel) . Just) m_chanId
          m_chanId = st^.csNames.cnToChanId.at (u^.uiName)
          unread = maybe False (hasUnread st) m_chanId
          current = maybe False (isCurrentChannel st) m_chanId
          mentions = case m_chanId of
            Just cId -> maybe 0 id (st^?csChannel(cId).ccInfo.cdMentionCount)
            Nothing  -> 0
       ]
