{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
module Matterhorn.Draw.Main
  ( drawMain
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick
import           Brick.Widgets.Border
import           Data.List ( intersperse )
import           Data.Maybe ( fromJust )
import qualified Data.Text as T
import           Lens.Micro.Platform ( Lens' )

import           Network.Mattermost.Types ( Type(Direct, Private, Group)
                                          , TeamId, teamDisplayName, teamId
                                          )


import           Matterhorn.Draw.ChannelList ( renderChannelList )
import           Matterhorn.Draw.Messages
import           Matterhorn.Draw.MessageInterface
import           Matterhorn.Draw.Autocomplete
import           Matterhorn.Draw.Util
import           Matterhorn.Draw.RichText
import           Matterhorn.Themes
import           Matterhorn.Types
import           Matterhorn.Types.Common ( sanitizeUserText )
import qualified Matterhorn.Zipper as Z


drawMain :: ChatState -> Mode -> [Widget Name]
drawMain st mode =
    (connectionLayer st : drawAutocompleteLayers st) <>
    [joinBorders $ mainInterface st mode (st^.csCurrentTeamId)]

connectionLayer :: ChatState -> Widget Name
connectionLayer st =
    case st^.csConnectionStatus of
        Connected -> emptyWidget
        Disconnected ->
            Widget Fixed Fixed $ do
                ctx <- getContext
                let aw = ctx^.availWidthL
                    w = length msg + 2
                    msg = "NOT CONNECTED"
                render $ translateBy (Location (max 0 (aw - w), 0)) $
                         withDefAttr errorMessageAttr $
                         border $ str msg

mainInterface :: ChatState -> Mode -> Maybe TeamId -> Widget Name
mainInterface st mode mtId =
    vBox [ teamList st
         , body
         ]
    where
    config = st^.csResources.crConfiguration

    showChannelList =
        config^.configShowChannelListL ||
        case mtId of
            Nothing -> True
            Just {} -> mode == ChannelSelect

    body = if showChannelList
           then case st^.csChannelListOrientation of
               ChannelListLeft ->
                   hBox [channelList, vBorder, mainDisplay]
               ChannelListRight ->
                   hBox [mainDisplay, vBorder, channelList]
           else mainDisplay

    mainDisplay = maybeSubdue messageInterface

    channelList = channelListMaybeVlimit mode $
                  hLimit channelListWidth $ case mtId of
                      Nothing -> fill ' '
                      Just tId -> renderChannelList st tId
    channelListWidth = configChannelListWidth $ st^.csResources.crConfiguration
    channelListMaybeVlimit ChannelSelect w =
        Widget (hSize w) (vSize w) $ do
            ctx <- getContext
            render $ vLimit (ctx^.availHeightL - 1) w
    channelListMaybeVlimit _ w = w

    noMessageInterface = fill ' '

    messageInterface = fromMaybe noMessageInterface $ do
        tId <- mtId
        let hs = getHighlightSet st tId

            channelHeader chan =
                withDefAttr channelHeaderAttr $
                padRight Max $
                renderChannelHeader st tId hs chan

            focused = st^.csTeam(tId).tsMessageInterfaceFocus == FocusCurrentChannel &&
                      threadShowing
            threadShowing = isJust $ st^.csTeam(tId).tsThreadInterface
            channelMessageIface cId =
                drawMessageInterface st hs tId True
                                     (csChannelMessageInterface(cId))
                                     True
                                     focused

            maybeThreadIface = do
                _ <- st^.csTeam(tId).tsThreadInterface
                return $ drawThreadWindow st tId

        cId <- st^.csCurrentChannelId(tId)
        ch <- st^?csChannel(cId)

        let channelUI = channelHeader ch <=> hBorder <=> channelMessageIface cId

        return $ fromMaybe channelUI $ do
            tui <- maybeThreadIface
            return $ case config^.configThreadOrientationL of
                ThreadAbove -> tui <=> hBorder <=> channelUI
                ThreadBelow -> channelUI <=> hBorder <=> tui
                ThreadLeft  -> tui <+> vBorder <+> channelUI
                ThreadRight -> channelUI <+> vBorder <+> tui

    maybeSubdue = if mode == ChannelSelect
                  then forceAttr $ attrName ""
                  else id

teamList :: ChatState -> Widget Name
teamList st =
    let curTid = st^.csCurrentTeamId
        z = st^.csTeamZipper
        pos = fromJust $ Z.position z
        teams = (\tId -> st^.csTeam(tId)) <$> (concat $ snd <$> Z.toList z)
        numTeams = length teams
        entries = mkEntry <$> teams
        mkEntry ts =
            let tId = teamId $ _tsTeam ts
                unread = uCount > 0
                uCount = teamUnreadCount tId st
                tName  = ClickableTeamListEntry tId
            in (if Just tId == curTid
                   then visible . withDefAttr currentTeamAttr
                   else if unread
                        then withDefAttr unreadChannelAttr
                        else id) $
               clickable tName $ txt $
               (T.strip $ sanitizeUserText $ teamDisplayName $ _tsTeam ts)
    in if numTeams == 1
       then emptyWidget
       else vBox [ hBox [ padRight (Pad 1) $ txt $ T.pack $ "Teams (" <> show (pos + 1) <> "/" <> show numTeams <> "):"
                        , vLimit 1 $ viewport TeamList Horizontal $
                          hBox $
                          intersperse (txt " ") entries
                        ]
                 , hBorder
                 ]

renderChannelHeader :: ChatState -> TeamId -> HighlightSet -> ClientChannel -> Widget Name
renderChannelHeader st tId hs chan =
    let chnType = chan^.ccInfo.cdType
        topicStr = chan^.ccInfo.cdHeader
        userHeader u = let s = T.intercalate " " $ filter (not . T.null) parts
                           parts = [ chanName
                                   , if (all T.null names)
                                     then mempty
                                     else "is"
                                   ] <> names <> [
                                     if T.null (u^.uiEmail)
                                     then mempty
                                     else "(" <> u^.uiEmail <> ")"
                                   ]
                           names = [ u^.uiFirstName
                                   , nick
                                   , u^.uiLastName
                                   ]
                           quote n = "\"" <> n <> "\""
                           nick = maybe "" quote $ u^.uiNickName
                       in s
        firstTopicLine = case T.lines topicStr of
            [h] -> h
            (h:_:_) -> h
            _ -> ""
        maybeTopic = if T.null topicStr
                     then ""
                     else " - " <> if st^.csResources.crConfiguration.configShowExpandedChannelTopicsL
                                   then topicStr
                                   else firstTopicLine
        channelNameString = case chnType of
            Direct ->
                case chan^.ccInfo.cdDMUserId >>= flip userById st of
                    Nothing -> chanName
                    Just u -> userHeader u
            Private ->
                channelNamePair <> " (Private)"
            Group ->
                channelNamePair <> " (Private group)"
            _ ->
                channelNamePair
        channelNamePair = chanName <> " - " <> (chan^.ccInfo.cdDisplayName)
        chanName = mkChannelName st (chan^.ccInfo)
        baseUrl = serverBaseUrl st tId

    in renderText' (Just baseUrl) (myUsername st)
         hs (Just (mkClickableInline Nothing (ChannelTopic $ chan^.ccInfo.cdChannelId)))
         (channelNameString <> maybeTopic)

drawThreadWindow :: ChatState -> TeamId -> Widget Name
drawThreadWindow st tId = withDefAttr threadAttr body
    where
        ti :: Lens' ChatState ThreadInterface
        ti = unsafeThreadInterface(tId)

        hs = getHighlightSet st tId
        cId = st^.ti.miChannelId

        titleText = case st^?csChannel(cId) of
            Nothing -> "Thread"
            Just chan ->
                let prefix = case chan^.ccInfo.cdType of
                        Group -> "Thread with "
                        Direct -> "Thread with "
                        _ -> "Thread in "
                in prefix <> mkChannelName st (chan^.ccInfo)

        title = renderText' Nothing "" hs Nothing titleText
        focused = st^.csTeam(tId).tsMessageInterfaceFocus == FocusThread
        body = title <=> hBorder <=> messageUI
        messageUI = drawMessageInterface st hs tId False ti False focused
