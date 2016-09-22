{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PackageImports #-}
module Draw.Main (drawMain) where

import           Brick
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import           Brick.Widgets.Center (hCenter, center)
import           Brick.Widgets.Edit (renderEditor)
import           Data.Time.Clock (UTCTime)
import           Data.Time.Format ( formatTime
                                  , defaultTimeLocale )
import           Data.Time.LocalTime ( TimeZone, utcToLocalTime, localDay )
import           Data.Default (def)
import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as Seq
import qualified Data.Foldable as F
import           Data.HashMap.Strict ( HashMap )
import           Data.List (sort, intersperse)
import           Data.Maybe ( listToMaybe, maybeToList )
import           Data.Monoid ((<>))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import           Lens.Micro.Platform

import           Network.Mattermost
import           Network.Mattermost.Lenses

import qualified Graphics.Vty as Vty

import           Markdown
import           State
import           Themes
import           Types
import           Draw.Util

-- If the config's date format is not set.
defaultDateFormat :: Text
defaultDateFormat = "%R"

renderTime :: Text -> TimeZone -> UTCTime -> Widget Name
renderTime fmt tz t =
    let timeStr = T.pack $ formatTime defaultTimeLocale (T.unpack fmt) (utcToLocalTime tz t)
    in txt "[" <+> withDefAttr timeAttr (txt timeStr) <+> txt "]"

renderChatMessage :: Set Text -> Maybe Text -> TimeZone -> Message -> Widget Name
renderChatMessage uSet mFormat tz msg =
    let m = renderMarkdown (msg^.mText) msgUsr (msg^.mType) uSet
        omitUsernameTypes = [ CP Join
                            , CP Leave
                            , CP TopicChange
                            ]
        msgUsr = case msg^.mUserName of
          Just u
            | msg^.mType `elem` omitUsernameTypes -> Nothing
            | otherwise -> Just u
          Nothing -> Nothing
        msgAtch = if Seq.null (msg^.mAttachments)
          then emptyWidget
          else withDefAttr clientMessageAttr
                  (txt "  [this message has an attachment]")
        msgTxt =
          case msg^.mUserName of
            Just _
              | msg^.mType == CP Join || msg^.mType == CP Leave || msg^.mDeleted ->
                  withDefAttr clientMessageAttr m
              | otherwise -> m
            Nothing ->
                case msg^.mType of
                    C DateTransition -> withDefAttr dateTransitionAttr (hBorderWithLabel m)
                    C Error -> withDefAttr errorMessageAttr m
                    _ -> withDefAttr clientMessageAttr m
        fullMsg = msgTxt <=> msgAtch
        maybeRenderTime = case mFormat of
            Just ""     -> id
            Just format -> \w -> renderTime format tz (msg^.mDate)            <+> txt " " <+> w
            Nothing     -> \w -> renderTime defaultDateFormat tz (msg^.mDate) <+> txt " " <+> w
        maybeRenderTimeWith f = case msg^.mType of
            C DateTransition -> id
            _ -> f
    in maybeRenderTimeWith maybeRenderTime fullMsg

mkChannelName :: ChannelInfo -> Text
mkChannelName c = T.cons sigil (c^.cdName)
  where sigil =  case c^.cdType of
          Private   -> '?'
          Ordinary  -> '#'
          Direct    -> '@'
          Unknown _ -> '!'

mkDMChannelName :: UserInfo -> Text
mkDMChannelName u = T.cons (userSigil u) (u^.uiName)

userSigil :: UserInfo -> Char
userSigil u = case u^.uiStatus of
    Offline -> ' '
    Online  -> '+'
    Away    -> '-'
    Other _ -> '?'

channelListWidth :: Int
channelListWidth = 20

normalChannelListHeight :: Int
normalChannelListHeight = 10

renderChannelList :: ChatState -> Widget Name
renderChannelList st = hLimit channelListWidth $ vBox $ renderChannelGroup st <$> channelGroups
    where
        channelGroups = [ ( "Channels"
                          , NormalChannelList
                          , Just normalChannelListHeight
                          , getOrdinaryChannels st
                          )
                        , ( "Users"
                          , DMChannelList
                          , Nothing
                          , getDmChannels st
                          )
                        ]

renderChannelGroup :: ChatState -> (T.Text, Name, Maybe Int, [ChannelListEntry]) -> Widget Name
renderChannelGroup st (groupName, vpName, heightLimit, entries) =
    let limit = maybe id vLimit heightLimit
        header label = hBorderWithLabel $ withDefAttr channelListHeaderAttr $ txt label
    in header groupName <=>
       (limit $ viewport vpName Vertical $ vBox $ renderChannelListEntry st <$> entries)

data ChannelListEntry =
    ChannelListEntry { entryChannelName :: T.Text
                     , entrySigil       :: T.Text
                     , entryLabel       :: T.Text
                     , entryMakeWidget  :: T.Text -> Widget Name
                     , entryHasUnread   :: Bool
                     , entryIsRecent    :: Bool
                     }

renderChannelListEntry :: ChatState -> ChannelListEntry -> Widget Name
renderChannelListEntry st entry =
    decorate $ decorateRecent $ padRight Max $
    entryMakeWidget entry $ entrySigil entry <> entryLabel entry
    where
    decorate = if | matches -> const $ (txt $ entrySigil entry)
                        <+> txt preMatch
                        <+> (forceAttr channelSelectMatchAttr $ txt $ st^.csChannelSelect)
                        <+> txt postMatch
                  | isChanSelect &&
                    (not $ T.null $ st^.csChannelSelect) -> const emptyWidget
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

    matches = isChanSelect &&
              (st^.csChannelSelect) `T.isInfixOf` entryLabel entry &&
              (not $ T.null $ st^.csChannelSelect)

    isChanSelect = st^.csMode == ChannelSelect
    (preMatch,postMatch) = case T.breakOn (st^.csChannelSelect) (entryLabel entry) of
      (pre, post) -> (pre, T.drop (T.length $ st^.csChannelSelect) post)
    current = entryChannelName entry == currentChannelName
    currentChannelName = getChannelName cId st
    cId = currentChannelId st

getOrdinaryChannels :: ChatState -> [ChannelListEntry]
getOrdinaryChannels st =
    [ ChannelListEntry n "#" n txt unread recent
    | n <- (st ^. csNames . cnChans)
    , let Just chan = st ^. csNames . cnToChanId . at n
          unread = hasUnread st chan
          recent = Just chan == st^.csRecentChannel
    ]

getDmChannels :: ChatState -> [ChannelListEntry]
getDmChannels st =
    let isSelf :: UserInfo -> Bool
        isSelf u = (st^.csMe.userIdL) == (u^.uiId)
        usersToList = filter (not . isSelf) $ st ^. usrMap & HM.elems

    in [ ChannelListEntry cname sigil uname colorUsername' unread recent
       | u <- sort usersToList
       , let colorUsername' =
               if | u^.uiStatus == Offline ->
                    withDefAttr clientMessageAttr . txt
                  | otherwise ->
                    colorUsername
             sigil = T.singleton $ userSigil u
             uname = u^.uiName
             cname = getDMChannelName (st^.csMe^.userIdL) (u^.uiId)
             recent = maybe False ((== st^.csRecentChannel) . Just) m_chanId
             m_chanId = st^.csNames.cnToChanId.at (u^.uiName)
             unread = maybe False (hasUnread st) m_chanId
       ]

renderUserCommandBox :: ChatState -> Widget Name
renderUserCommandBox st = prompt <+> inputBox
    where
    prompt = txt "> "
    inputBox = renderEditor True (st^.cmdLine)

renderCurrentChannelDisplay :: ChatState -> Widget Name
renderCurrentChannelDisplay st = (header <+> conn) <=> messages
    where
    conn = case st^.csConnectionStatus of
      Connected -> emptyWidget
      Disconnected -> withDefAttr errorMessageAttr (str "[NOT CONNECTED]")
    header = withDefAttr channelHeaderAttr $
             padRight Max $
             case T.null topicStr of
                 True -> case chnType of
                   Direct ->
                     case findUserByDMChannelName (st^.usrMap)
                                                  chnName
                                                  (st^.csMe^.userIdL) of
                       Nothing -> txt $ mkChannelName (chan^.ccInfo)
                       Just u  -> colorUsername $ mkDMChannelName u
                   _        -> txt $ mkChannelName (chan^.ccInfo)
                 False -> wrappedText txt $ mkChannelName (chan^.ccInfo) <> " - " <> topicStr
    messages = if chan^.ccInfo.cdLoaded
               then chatText <+> txt " "
               else center $ txt "[loading channel scrollback]"
    uSet = Set.fromList (map _uiName (HM.elems (st^.usrMap)))
    chatText = case st^.csMode of
        ChannelScroll ->
            viewport (ChannelMessages cId) Vertical $
            cached (ChannelMessages cId) $
            vBox $ F.toList $ renderSingleMessage <$> channelMessages
        _ -> renderLastMessages channelMessages
    channelMessages = insertDateBoundaries (st ^. timeZone) $ getMessageListing cId st
    renderSingleMessage = renderChatMessage uSet (st ^. timeFormat) (st ^. timeZone)

    renderLastMessages :: Seq.Seq Message -> Widget Name
    renderLastMessages msgs =
        Widget Greedy Greedy $ do
            ctx <- getContext

            let targetHeight = ctx^.availHeightL
                go :: Seq.Seq Message -> Vty.Image -> RenderM Name Vty.Image
                go ms img
                    | Seq.null ms =
                        return img
                    | Vty.imageHeight img >= targetHeight =
                        return img
                    | otherwise =
                        case Seq.viewr ms of
                            Seq.EmptyR -> return img
                            ms' Seq.:> msg -> do
                                result <- render $ padRight Max $ renderSingleMessage msg
                                go ms' $ Vty.vertJoin (result^.imageL) img

            img <- Vty.cropTop targetHeight <$> go msgs Vty.emptyImage
            return $ def & imageL .~ img

    cId = currentChannelId st
    Just chan = getChannel cId st
    chnName = chan^.ccInfo.cdName
    chnType = chan^.ccInfo.cdType
    topicStr = chan^.ccInfo.cdHeader

getMessageListing :: ChannelId -> ChatState -> Seq.Seq Message
getMessageListing cId st =
    st ^. msgMap . ix cId . ccContents . cdMessages

dateTransitionFormat :: String
dateTransitionFormat = "%Y-%m-%d"

insertDateBoundaries :: TimeZone -> Seq.Seq Message -> Seq.Seq Message
insertDateBoundaries tz ms = fst $ F.foldl' nextMsg initState ms
    where
        initState :: (Seq.Seq Message, Maybe Message)
        initState = (mempty, Nothing)

        dateMsg d = Message (getBlocks (T.pack $ formatTime defaultTimeLocale dateTransitionFormat d))
                            Nothing d (C DateTransition) False False Seq.empty Nothing

        nextMsg :: (Seq.Seq Message, Maybe Message) -> Message -> (Seq.Seq Message, Maybe Message)
        nextMsg (rest, Nothing) msg = (rest Seq.|> msg, Just msg)
        nextMsg (rest, Just prevMsg) msg =
            if localDay (utcToLocalTime tz (msg^.mDate)) /= localDay (utcToLocalTime tz (prevMsg^.mDate))
            then (rest Seq.|> (dateMsg (msg^.mDate)) Seq.|> msg, Just msg)
            else (rest Seq.|> msg, Just msg)

findUserByDMChannelName :: HashMap UserId UserInfo
                        -> T.Text -- ^ the dm channel name
                        -> UserId -- ^ me
                        -> Maybe UserInfo -- ^ you
findUserByDMChannelName userMap dmchan me = listToMaybe
  [ user
  | u <- HM.keys userMap
  , getDMChannelName me u == dmchan
  , user <- maybeToList (HM.lookup u userMap)
  ]

subdue :: ChatState -> Widget a -> Widget a
subdue st = if st^.csMode == ChannelSelect
            then forceAttr ""
            else id

renderChannelSelect :: ChatState -> Widget Name
renderChannelSelect st =
    withDefAttr channelSelectPromptAttr $
    (txt "Switch to channel: ") <+>
     (showCursor ChannelSelectString (Location (T.length $ st^.csChannelSelect, 0)) $
      txt $
      (if T.null $ st^.csChannelSelect
       then " "
       else st^.csChannelSelect))

drawMain :: ChatState -> [Widget Name]
drawMain st = [mainInterface st]

completionAlternatives :: ChatState -> Widget Name
completionAlternatives st =
    let alternatives = intersperse (txt " ") $ mkAlternative <$> st^.csEditState.cedCompletionAlternatives
        mkAlternative val = let format = if val == st^.csEditState.cedCurrentAlternative
                                         then visible . withDefAttr completionAlternativeCurrentAttr
                                         else id
                            in format $ txt val
    in hBox [ borderElem bsHorizontal
            , txt "["
            , withDefAttr completionAlternativeListAttr $
              vLimit 1 $ viewport CompletionAlternatives Horizontal $ hBox alternatives
            , txt "]"
            , borderElem bsHorizontal
            ]

mainInterface :: ChatState -> Widget Name
mainInterface st =
    (renderChannelList st <+> (subdue st (borderElem bsIntersectR <=>
                                            vLimit normalChannelListHeight vBorder <=>
                                            borderElem bsIntersectR <=> vBorder))
                            <+> (subdue st $ renderCurrentChannelDisplay st))
      <=> bottomBorder
      <=> case st^.csMode of
              ChannelSelect -> renderChannelSelect st
              ChannelScroll -> hCenter $ hBox [ txt "Press "
                                              , withDefAttr clientEmphAttr $ txt "Escape"
                                              , txt " to stop scrolling and resume chatting."
                                              ]
              _             -> renderUserCommandBox st
    where
    bottomBorder = case st^.csCurrentCompletion of
        Just _ | length (st^.csEditState.cedCompletionAlternatives) > 1 -> completionAlternatives st
        _ -> subdue st $ hLimit channelListWidth hBorder <+> borderElem bsIntersectB <+> hBorder
