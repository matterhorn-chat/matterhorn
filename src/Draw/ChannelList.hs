{-# LANGUAGE MultiWayIf #-}

module Draw.ChannelList (renderChannelList) where

import           Brick
import           Brick.Widgets.Border
import qualified Data.HashMap.Strict as HM
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Draw.Util
import           Lens.Micro.Platform
import           Network.Mattermost.Lenses
import           State
import           State.Common
import           Themes
import           Types

renderChannelList :: ChatState -> Widget Name
renderChannelList st = maybeViewport $
                       vBox $ concat $ renderChannelGroup st <$> channelGroups
    where
        -- Only render the channel list in a viewport if we're not in
        -- channel select mode, since we don't want or need the viewport
        -- state to be affected by channel select input.
        maybeViewport = if st^.csMode == ChannelSelect
                        then id
                        else viewport ChannelList Vertical
        channelGroups = [ ( "Channels"
                          , getOrdinaryChannels st
                          , st^.csChannelSelectChannelMatches
                          )
                        , ( "Users"
                          , getDmChannels st
                          , st^.csChannelSelectUserMatches
                          )
                        ]

renderChannelGroup :: ChatState
                   -> (T.Text, [ChannelListEntry], HM.HashMap T.Text ChannelSelectMatch)
                   -> [Widget Name]
renderChannelGroup st (groupName, entries, csMatches) =
    let header label = hBorderWithLabel $ withDefAttr channelListHeaderAttr $ txt label
    in header groupName : (renderChannelListEntry st csMatches <$> entries)

data ChannelListEntry =
    ChannelListEntry { entryChannelName :: T.Text
                     , entrySigil       :: T.Text
                     , entryLabel       :: T.Text
                     , entryMakeWidget  :: T.Text -> Widget Name
                     , entryHasUnread   :: Bool
                     , entryIsRecent    :: Bool
                     }

renderChannelListEntry :: ChatState
                       -> HM.HashMap T.Text ChannelSelectMatch
                       -> ChannelListEntry
                       -> Widget Name
renderChannelListEntry st csMatches entry =
    decorate $ decorateRecent $ padRight Max $
    entryMakeWidget entry $ entrySigil entry <> entryLabel entry
    where
    decorate = if | matches -> const $
                      let Just (ChannelSelectMatch preMatch inMatch postMatch) =
                                   HM.lookup (entryLabel entry) csMatches
                      in (txt $ entrySigil entry)
                          <+> txt preMatch
                          <+> (forceAttr channelSelectMatchAttr $ txt inMatch)
                          <+> txt postMatch
                  | isChanSelect &&
                    (not $ T.null $ st^.csChannelSelectString) -> const emptyWidget
                  | current ->
                      if isChanSelect
                      then forceAttr currentChannelNameAttr
                      else visible . forceAttr currentChannelNameAttr
                  | entryHasUnread entry ->
                      forceAttr unreadChannelAttr
                  | otherwise -> id

    decorateRecent = if entryIsRecent entry
                     then (<+> (withDefAttr recentMarkerAttr $ str "<"))
                     else id

    matches = isChanSelect && (HM.member (entryLabel entry) csMatches) &&
              (not $ T.null $ st^.csChannelSelectString)

    isChanSelect = st^.csMode == ChannelSelect
    current = entryChannelName entry == currentChannelName
    currentChannelName = st^.csCurrentChannel.ccInfo.cdName

getOrdinaryChannels :: ChatState -> [ChannelListEntry]
getOrdinaryChannels st =
    [ ChannelListEntry n sigil n txt unread recent
    | n <- (st ^. csNames . cnChans)
    , let Just chan = st ^. csNames . cnToChanId . at n
          unread = hasUnread st chan
          recent = Just chan == st^.csRecentChannel
          sigil = case st ^. csLastChannelInput . at chan of
            Nothing      -> T.singleton normalChannelSigil
            Just ("", _) -> T.singleton normalChannelSigil
            _            -> "»"
    ]

getDmChannels :: ChatState -> [ChannelListEntry]
getDmChannels st =
    [ ChannelListEntry cname sigil uname colorUsername' unread recent
    | u <- sortedUserList st
    , let colorUsername' =
            if | u^.uiStatus == Offline ->
                 withDefAttr clientMessageAttr . txt
               | otherwise ->
                 colorUsername
          sigil =
            case do { cId <- m_chanId; st^.csLastChannelInput.at cId } of
              Nothing      -> T.singleton $ userSigilFromInfo u
              Just ("", _) -> T.singleton $ userSigilFromInfo u
              _            -> "»"
          uname = u^.uiName
          cname = getDMChannelName (st^.csMe^.userIdL) (u^.uiId)
          recent = maybe False ((== st^.csRecentChannel) . Just) m_chanId
          m_chanId = st^.csNames.cnToChanId.at (u^.uiName)
          unread = maybe False (hasUnread st) m_chanId
       ]
