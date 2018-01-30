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
import qualified Data.Sequence as Seq
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as HM
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Draw.Util
import           Lens.Micro.Platform
import           State
import           Themes
import           Types
import           Types.Users

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
--
-- The retrieval function is also given an optional integer that
-- indicates the height of the channel list (the amount of screen space
-- available to be used). This is provided as an optimization so that we
-- can avoid rendering all known entries when they wouldn't be visible
-- anyway. Please see 'getDmChannels' for more details.
--
-- The height value is optional because when we're in channel selection
-- mode, we don't want to optimize away hidden channel list entries. If
-- we were to do that, they wouldn't be checked for matching against the
-- channel selection input. So in that case 'Nothing' is given as the
-- channel list height. But otherwise when we're not in that mode we
-- want to skip rendering hidden entries. (Ideally we wouldn't need this
-- hack at all and could be smart about which entries to show regardless
-- of mode, but right now we use a Brick viewport to make scrolling the
-- channel list easy, but that doesn't give us much control over which
-- entries to skip.)
--
-- Lastly, this functionality uses Sequences instead of lists to
-- facilitate efficient take/drop operations when the optimization
-- mentioned above is in effect.
channelListGroups :: [ ( GroupName
                       , Getting ChannelSelectMap ChatState ChannelSelectMap
                       , ChatState -> Maybe Int -> Seq.Seq ChannelListEntry
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
    Widget Fixed Greedy $ do
        ctx <- getContext

        let maybeViewport =
                if hasActiveChannelSelection st
                then id -- no viewport scrolling when actively selecting a channel
                else viewport ChannelList Vertical
            selMatch = st^.csChannelSelectState.selectedMatch
            renderedGroups =
                if hasActiveChannelSelection st
                then renderChannelGroup (renderChannelSelectListEntry selMatch) <$>
                         selectedGroupEntries
                else renderChannelGroup renderChannelListEntry <$> plainGroupEntries
            plainGroupEntries (n, _m, f) =
                (n, f st (Just $ ctx^.availHeightL))
            selectedGroupEntries (n, m, f) =
                (n, F.foldr (addSelectedChannel m) mempty $ f st Nothing)
            addSelectedChannel m e s =
                case HM.lookup (entryLabel e) (st^.m) of
                    Just y -> SCLE e y Seq.<| s
                    Nothing -> s

        render $ maybeViewport $ vBox $ vBox <$>
                 F.toList <$> (F.toList $ renderedGroups <$> channelListGroups)

-- | Renders a specific group, given the name of the group and the
-- list of entries in that group (which are expected to be either
-- ChannelListEntry or SelectedChannelListEntry elements).
renderChannelGroup :: (a -> Widget Name) -> (GroupName, Seq.Seq a) -> Seq.Seq (Widget Name)
renderChannelGroup eRender (groupName, entries) =
    let header label = hBorderWithLabel $
                       withDefAttr channelListHeaderAttr $ txt label
    in header groupName Seq.<| (eRender <$> entries)

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
                    Just _       -> colorUsername (entryLabel entry)
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
getOrdinaryChannels :: ChatState -> Maybe Int -> Seq.Seq ChannelListEntry
getOrdinaryChannels st _ =
    Seq.fromList [ ChannelListEntry sigil n unread mentions recent current Nothing
    | n <- allChannelNames st
    , let Just chan = channelIdByName n st
          unread = hasUnread st chan
          recent = isRecentChannel st chan
          current = isCurrentChannel st chan
          sigil = case st ^. csEditState.cedLastChannelInput.at chan of
            Nothing      -> normalChannelSigil
            Just ("", _) -> normalChannelSigil
            _            -> "»"
          mentions = channelMentionCount chan st
    ]

-- | Extract the names and information about Direct Message channels
-- to be displayed in the ChannelList sidebar.
--
-- This function takes advantage of the channel height, when given, by
-- only returning enough entries to guarantee that we fill the channel
-- list. For example, if the list is N rows high, this function will
-- return at most 2N channel list entries. It does this, rather than
-- return them all, to avoid rendering (potentially thousands of)
-- entries that won't be visible on the screen anyway, and that turns
-- out to be a big performance win on servers with thousands of users.
-- We return *twice* the number of required entries to ensure that no
-- matter where the selected channel is within the set of returned
-- entries, there are enough entries before and after the selected
-- channel to get the Brick viewport to position the final result in a
-- way that is natural.
getDmChannels :: ChatState -> Maybe Int -> Seq.Seq ChannelListEntry
getDmChannels st height =
    let es = Seq.fromList
             [ ChannelListEntry (T.cons sigil " ") uname unread
                                mentions recent current (Just $ u^.uiStatus)
             | u <- sortedUserList st
             , let sigil =
                     case do { cId <- m_chanId; st^.csEditState.cedLastChannelInput.at cId } of
                       Nothing      -> userSigilFromInfo u
                       Just ("", _) -> userSigilFromInfo u
                       _            -> '»'  -- shows that user has a message in-progress
                   uname = u^.uiName
                   recent = maybe False (isRecentChannel st) m_chanId
                   m_chanId = channelIdByName (u^.uiName) st
                   unread = maybe False (hasUnread st) m_chanId
                   current = maybe False (isCurrentChannel st) m_chanId
                   mentions = fromMaybe 0 $ channelMentionCount <$> m_chanId <*> pure st
                ]
        (h, t) = Seq.breakl entryIsCurrent es
    in case height of
        Nothing -> es
        Just height' -> if null t
                        then Seq.take height' h
                        else Seq.drop (length h - height') h <>
                             Seq.take height' t
