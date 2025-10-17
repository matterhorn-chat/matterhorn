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

module Matterhorn.Draw.ChannelList
  ( renderChannelList
  , renderChannelListHeader
  , channelListWidth
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick
import           Brick.Widgets.Border
import           Brick.Widgets.Center (hCenter)
import           Brick.Widgets.Edit ( editContentsL )
import           Data.List (intersperse)
import qualified Data.Text as T
import qualified Data.Text.Zipper as TZ
import           Data.Maybe ( fromJust )
import           Lens.Micro.Platform (non)

import qualified Network.Mattermost.Types as MM

import           Matterhorn.Draw.Util
import           Matterhorn.State.Channels
import           Matterhorn.Constants ( channelListMinAutoWidth, channelListMaxAutoWidth )
import           Matterhorn.Themes
import           Matterhorn.Types
import           Matterhorn.Types.Common ( sanitizeUserText )
import qualified Matterhorn.Zipper as Z

data ChannelListEntryLabel =
    Single !Text
    | UserList ![Text]

-- | Internal record describing each channel entry and its associated
-- attributes.  This is the object passed to the rendering function so
-- that it can determine how to render each channel.
data ChannelListEntryData =
    ChannelListEntryData { entrySigil       :: !Text
                         , entryLabel       :: !ChannelListEntryLabel
                         , entryHasUnread   :: !Bool
                         , entryMentions    :: !Int
                         , entryIsRecent    :: !Bool
                         , entryIsReturn    :: !Bool
                         , entryIsCurrent   :: !Bool
                         , entryIsMuted     :: !Bool
                         , entryUserStatus  :: !(Maybe UserStatus)
                         }

sbRenderer :: VScrollbarRenderer n
sbRenderer =
    verticalScrollbarRenderer { renderVScrollbarHandleBefore = str "▲"
                              , renderVScrollbarHandleAfter = str "▼"
                              , scrollbarWidthAllocation = 2
                              , renderVScrollbar =
                                  hLimit 1 $
                                  renderVScrollbar verticalScrollbarRenderer
                              , renderVScrollbarTrough =
                                  hLimit 1 $
                                  renderVScrollbarTrough verticalScrollbarRenderer
                              }

renderChannelListHeader :: ChatState -> MM.TeamId -> Widget Name
renderChannelListHeader st tId =
    vBox [ teamHeader
         , selfHeader
         , unreadCountHeader
         ]
    where
        myUsername_ = myUsername st
        teamHeader = hCenter $
                     withDefAttr clientEmphAttr $
                     txt $ "Team: " <> teamNameStr
        selfHeader = hCenter $
                     colorUsername myUsername_ myUsername_
                         (T.singleton statusSigil <> " " <> addUserSigil myUsername_)
        teamNameStr = T.strip $ sanitizeUserText $ MM.teamDisplayName $ st^.csTeam(tId).tsTeam
        statusSigil = maybe ' ' userSigilFromInfo me
        me = knownUserById (myUserId st) st
        unreadCountHeader = hCenter $ txt $ "Unread: " <> (T.pack $ show unreadCount)
        unreadCount = sum $ (channelListGroupUnread . fst) <$> Z.toList (st^.csTeam(tId).tsFocus)

renderChannelList :: ChatState -> MM.TeamId -> Widget Name
renderChannelList st tId =
    header <=> vpBody
    where
        sbOrientation = case st^.csResources.crConfiguration.configChannelListOrientationL of
            ChannelListLeft -> OnLeft
            ChannelListRight -> OnRight
        myUsername_ = myUsername st
        channelName e = ClickableChannelListEntry $ channelListEntryChannelId e
        renderEntry s e = clickable (channelName e) $
                          renderChannelListEntry tId myUsername_ $ mkChannelEntryData s tId e
        header = renderChannelListHeader st tId
        vpBody = withVScrollBarRenderer sbRenderer $
                 withVScrollBars sbOrientation $
                 withVScrollBarHandles $
                 withClickableVScrollBars VScrollBar $
                 viewport (ChannelListViewport tId) Vertical body
        body = case teamMode $ st^.csTeam(tId) of
            ChannelSelect ->
                let zipper = st^.csTeam(tId).tsChannelSelectState.channelSelectMatches
                    matches = if Z.isEmpty zipper
                              then [hCenter $ txt "No matches"]
                              else (renderChannelListGroup False st
                                       (renderChannelSelectListEntry tId (Z.focus zipper)) <$>
                                   Z.toList zipper)
                in vBox matches
            _ ->
                cached (ChannelSidebar tId) $
                vBox $
                (renderChannelListGroup True st renderEntry <$> Z.toList (st^.csTeam(tId).tsFocus))

renderChannelListGroupHeading :: ChannelListGroup -> Widget Name
renderChannelListGroupHeading g =
    let label = channelListGroupLabel g
        labelStr = case label of
            ChannelGroupPublicChannels   -> "Public Channels"
            ChannelGroupPrivateChannels  -> "Private Channels"
            ChannelGroupFavoriteChannels -> "Favorite Channels"
            ChannelGroupDirectMessages   -> "Direct Messages"
        unread = channelListGroupUnread g
        collapsed = channelListGroupCollapsed g
        addUnread = if unread > 0
                    then (<+> (withDefAttr unreadGroupMarkerAttr $ txt "*"))
                    else id
        addExpand = if collapsed
                    then (<+> (withDefAttr unreadGroupMarkerAttr $ txt "[+]"))
                    else id
        labelWidget = addExpand $ addUnread $ withDefAttr channelListHeaderAttr $ txt labelStr
    in hBorderWithLabel $ clickable (ClickableChannelListGroupHeading label) labelWidget

renderChannelListGroup :: Bool
                       -- ^ Whether to draw empty groups (just show
                       -- their headings)
                       -> ChatState
                       -> (ChatState -> e -> Widget Name)
                       -> (ChannelListGroup, [e])
                       -> Widget Name
renderChannelListGroup False _ _ (_, []) = emptyWidget
renderChannelListGroup _ st renderEntry (group, es) =
    let heading = renderChannelListGroupHeading group
        entryWidgets = renderEntry st <$> es
    in if channelListGroupEntries group > 0 || (channelListGroupCollapsed group)
       then vBox (heading : entryWidgets)
       else emptyWidget

mkChannelEntryData :: ChatState
                   -> MM.TeamId
                   -> ChannelListEntry
                   -> ChannelListEntryData
mkChannelEntryData st tId e =
    ChannelListEntryData { entrySigil       = sigilWithSpace
                         , entryLabel       = label
                         , entryHasUnread   = unread
                         , entryMentions    = mentions
                         , entryIsRecent    = recent
                         , entryIsReturn    = ret
                         , entryIsCurrent   = current
                         , entryIsMuted     = muted
                         , entryUserStatus  = status
                         }
    where
        cId = channelListEntryChannelId e
        unread = channelListEntryUnread e
        chan = fromJust $ findChannelById cId (st^.csChannels)
        recent = isRecentChannel st tId cId
        ret = isReturnChannel st tId cId
        current = isCurrentChannel st tId cId
        muted = channelListEntryMuted e
        dropComma t = if "," `T.isSuffixOf` t
                      then T.init t
                      else t
        (label, normalSigil, addSpace, status) = case channelListEntryType e of
            CLChannel ->
                (Single $ chan^.ccInfo.cdDisplayName, Nothing, False, Nothing)
            CLGroupDM ->
                (UserList $ dropComma <$> (T.words $ chan^.ccInfo.cdDisplayName),
                 Just " ", True, Nothing)
            CLUserDM uId ->
                let u = fromJust $ knownUserById uId st
                    uname = if useNickname st
                            then u^.uiNickName.non (u^.uiName)
                            else u^.uiName
                in (Single $ uname, Just $ T.singleton $ userSigilFromInfo u,
                    True, Just $ u^.uiStatus)
        sigilWithSpace = sigil <> if addSpace then " " else ""
        prevEditSigil = "»"
        sigil = if current
                then fromMaybe "" normalSigil
                else case chan^.ccMessageInterface.miEditor.esEphemeral.eesInputHistoryPosition of
                    Just _ -> prevEditSigil
                    Nothing ->
                        let emptyEditor = T.null $ T.concat $ TZ.getText $
                                          chan^.ccMessageInterface.miEditor.esEditor.editContentsL
                        in if emptyEditor
                           then fromMaybe "" normalSigil
                           else prevEditSigil
        mentions = chan^.ccInfo.cdMentionCount

-- | Render an individual Channel List entry (in Normal mode) with
-- appropriate visual decorations.
renderChannelListEntry :: MM.TeamId -> Text -> ChannelListEntryData -> Widget Name
renderChannelListEntry tId myUName entry = body
    where
    body = decorate $ decorateEntry entry $ decorateMentions entry $ padRight Max $
           txt (entrySigil entry) <+> entryWidget
    decorate = if | entryIsCurrent entry ->
                      reportExtent (SelectedChannelListEntry tId) . forceAttr currentChannelNameAttr
                  | entryMentions entry > 0 && not (entryIsMuted entry) ->
                      forceAttr mentionsChannelAttr
                  | entryHasUnread entry ->
                      forceAttr unreadChannelAttr
                  | otherwise -> id
    entryWidget =
        case entryLabel entry of
            Single label ->
                case entryUserStatus entry of
                    Just Offline -> withDefAttr clientMessageAttr $ txt label
                    Just _       -> colorUsername myUName label label
                    Nothing      -> txt label
            UserList usernames ->
                let color n = colorUsername myUName n n
                in hBox $ intersperse (txt ", ") $ color <$> usernames

-- | Render an individual entry when in Channel Select mode,
-- highlighting the matching portion, or completely suppressing the
-- entry if it doesn't match.
renderChannelSelectListEntry :: MM.TeamId
                             -> Maybe ChannelSelectMatch
                             -> ChatState
                             -> ChannelSelectMatch
                             -> Widget Name
renderChannelSelectListEntry tId curMatch st match =
    let ChannelSelectMatch preMatch inMatch postMatch _ entry = match
        maybeSelect = if (Just entry) == (matchEntry <$> curMatch)
                      then visible . withDefAttr currentChannelNameAttr
                      else id
        entryData = mkChannelEntryData st tId entry
        decorate = if | entryMentions entryData > 0 && not (entryIsMuted entryData) ->
                          withDefAttr mentionsChannelAttr
                      | entryHasUnread entryData ->
                          withDefAttr unreadChannelAttr
                      | otherwise -> id
    in clickable (ClickableChannelSelectEntry match) $
       decorate $ maybeSelect $
       decorateEntry entryData $ decorateMentions entryData $
       padRight Max $
         hBox [ txt $ entrySigil entryData <> preMatch
              , forceAttr channelSelectMatchAttr $ txt inMatch
              , txt postMatch
              ]

-- If this channel is the return channel, add a decoration to denote
-- that.
--
-- Otherwise, if this channel is the most recently viewed channel (prior
-- to the currently viewed channel), add a decoration to denote that.
decorateEntry :: ChannelListEntryData -> Widget n -> Widget n
decorateEntry entry =
    if entryIsReturn entry
    then (<+> (withDefAttr recentMarkerAttr $ str returnChannelSigil))
    else if entryIsRecent entry
         then (<+> (withDefAttr recentMarkerAttr $ str recentChannelSigil))
         else id

decorateMentions :: ChannelListEntryData -> Widget n -> Widget n
decorateMentions entry
  | entryMentions entry > 9 =
      (<+> str "(9+)")
  | entryMentions entry > 0 =
      (<+> str ("(" <> show (entryMentions entry) <> ")"))
  | entryIsMuted entry =
      (<+> str "(m)")
  | otherwise = id

recentChannelSigil :: String
recentChannelSigil = "<"

returnChannelSigil :: String
returnChannelSigil = "~"

channelListWidthAutoPercent :: Double
channelListWidthAutoPercent = 0.2

channelListWidth :: ChatState -> Int
channelListWidth st =
    case configChannelListWidth $ st^.csResources.crConfiguration of
        ChannelListWidthFixed w -> w
        ChannelListWidthAuto ->
            let calcWidth = round $ channelListWidthAutoPercent * (fromIntegral width :: Double)
                width = fst $ st^.csResources.crWindowSize
            in min channelListMaxAutoWidth $
               max channelListMinAutoWidth calcWidth
