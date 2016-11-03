{-# LANGUAGE MultiWayIf #-}
module Draw.Main (drawMain) where

import           Brick
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import           Brick.Widgets.Center (hCenter)
import           Brick.Widgets.Edit (editContentsL, renderEditor, getEditContents)
import           Brick.Widgets.List (renderList)
import           Control.Applicative
import           Data.Time.Clock (UTCTime(..))
import           Data.Time.Calendar (fromGregorian)
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
import           Data.Text.Zipper (cursorPosition)
import           Lens.Micro.Platform

import           Prelude

import           Network.Mattermost
import           Network.Mattermost.Lenses

import qualified Graphics.Vty as Vty

import           Markdown
import           State
import           Themes
import           Types
import           Config

-- If the config's time format is not set.
defaultTimeFormat :: Text
defaultTimeFormat = "%R"

renderTime :: Text -> TimeZone -> UTCTime -> Widget Name
renderTime fmt tz t =
    let timeStr = T.pack $ formatTime defaultTimeLocale (T.unpack fmt) (utcToLocalTime tz t)
    in txt "[" <+> withDefAttr timeAttr (txt timeStr) <+> txt "]"

renderChatMessage :: Set Text -> Maybe Text -> TimeZone -> Message -> Widget Name
renderChatMessage uSet mFormat tz msg =
    let m = renderMessage msg True uSet
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
                    C NewMessagesTransition -> withDefAttr newMessageTransitionAttr (hBorderWithLabel m)
                    C Error -> withDefAttr errorMessageAttr m
                    _ -> withDefAttr clientMessageAttr m
        fullMsg = msgTxt <=> msgAtch
        maybeRenderTime = case mFormat of
            Just "" -> id
            _ -> \w ->
                let fmt = maybe defaultTimeFormat id mFormat
                in renderTime fmt tz (msg^.mDate) <+> txt " " <+> w
        maybeRenderTimeWith f = case msg^.mType of
            C DateTransition -> id
            C NewMessagesTransition -> id
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

renderChannelList :: ChatState -> Widget Name
renderChannelList st = hLimit channelListWidth $ viewport ChannelList Vertical $
                       vBox $ concat $ renderChannelGroup st <$> channelGroups
    where
        channelGroups = [ ( "Channels"
                          , getOrdinaryChannels st
                          , st^.csChannelSelectChannelMatches
                          )
                        , ( "Users"
                          , getDmChannels st
                          , st^.csChannelSelectUserMatches
                          )
                        ]

renderChannelGroup :: ChatState -> (T.Text, [ChannelListEntry], HM.HashMap T.Text ChannelSelectMatch) -> [Widget Name]
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

renderChannelListEntry :: ChatState -> HM.HashMap T.Text ChannelSelectMatch -> ChannelListEntry -> Widget Name
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

previewFromInput :: T.Text -> T.Text -> Maybe Message
previewFromInput uname s =
    -- If it starts with a slash but not /me, this has no preview
    -- representation
    let isCommand = "/" `T.isPrefixOf` s
        isEmote = "/me " `T.isPrefixOf` s
        content = if isEmote
                  then T.stripStart $ T.drop 3 s
                  else s
        msgTy = if isEmote then CP Emote else CP NormalPost
    in if isCommand && not isEmote
       then Nothing
       else Just $ Message { _mText          = getBlocks content
                           , _mUserName      = Just uname
                           , _mDate          = UTCTime (fromGregorian 1970 1 1) 0
                           -- ^ The date is not used for preview
                           -- rendering, but we need to provide one.
                           -- Ideally we'd just today's date, but the
                           -- rendering function is pure so we can't.
                           , _mType          = msgTy
                           , _mPending       = False
                           , _mDeleted       = False
                           , _mAttachments   = mempty
                           , _mInReplyToMsg  = NotAReply
                           , _mPostId        = Nothing
                           }

renderUserCommandBox :: ChatState -> Widget Name
renderUserCommandBox st =
    let prompt = txt "> "
        inputBox = renderEditor True (st^.cmdLine)
        curContents = getEditContents $ st^.cmdLine
        multilineContent = length curContents > 1
        multilineHints =
            (borderElem bsHorizontal) <+>
            (str $ "[" <> (show $ (+1) $ fst $ cursorPosition $
                                  st^.cmdLine.editContentsL) <>
                   "/" <> (show $ length curContents) <> "]") <+>
            (hBorderWithLabel $ withDefAttr clientEmphAttr $
             (str "In multi-line mode. Press Esc to finish."))

        commandBox = case st^.csEditState.cedMultiline of
            False ->
                let linesStr = if numLines == 1
                               then "line"
                               else "lines"
                    numLines = length curContents
                in vLimit 1 $
                   prompt <+> if multilineContent
                              then ((withDefAttr clientEmphAttr $
                                     str $ "[" <> show numLines <> " " <> linesStr <>
                                           "; Enter: send, M-e: edit, Backspace: cancel] ")) <+>
                                   (txt $ head curContents) <+>
                                   (showCursor MessageInput (Location (0,0)) $ str " ")
                              else inputBox
            True -> vLimit 5 inputBox <=> multilineHints
    in commandBox

renderCurrentChannelDisplay :: Set Text -> ChatState -> Widget Name
renderCurrentChannelDisplay uSet st = (header <+> conn) <=> messages
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
                 False -> renderText $
                          mkChannelName (chan^.ccInfo) <> " - " <> topicStr
    messages = body <+> txt " "
    body = chatText <=> case chan^.ccInfo.cdCurrentState of
      ChanUnloaded   -> withDefAttr clientMessageAttr $
                          txt "[Loading channel scrollback...]"
      ChanRefreshing -> withDefAttr clientMessageAttr $
                          txt "[Refreshing scrollback...]"
      _              -> emptyWidget
    chatText = case st^.csMode of
        ChannelScroll ->
            viewport (ChannelMessages cId) Vertical $
            reportExtent (ChannelMessages cId) $
            cached (ChannelMessages cId) $
            vBox $ (withDefAttr loadMoreAttr $ hCenter $
                    str "<< Press C-l to load more messages >>") :
                   (F.toList $ renderSingleMessage <$> channelMessages)
        _ -> renderLastMessages channelMessages

    channelMessages =
        insertTransitions (getDateFormat st)
                          (st ^. timeZone)
                          (getNewMessageCutoff cId st)
                          (getMessageListing cId st)

    renderSingleMessage =
        renderChatMessage uSet (getTimeFormat st) (st ^. timeZone)

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
                                result <- case msg^.mDeleted of
                                    True -> return Vty.emptyImage
                                    False -> do
                                        r <- render $ padRight Max $ renderSingleMessage msg
                                        return $ r^.imageL
                                go ms' $ Vty.vertJoin result img

            img <- Vty.cropTop targetHeight <$> go msgs Vty.emptyImage
            return $ def & imageL .~ img

    cId = st^.csCurrentChannelId
    chan = st^.csCurrentChannel
    chnName = chan^.ccInfo.cdName
    chnType = chan^.ccInfo.cdType
    topicStr = chan^.ccInfo.cdHeader

getTimeFormat :: ChatState -> T.Text
getTimeFormat st = maybe defaultTimeFormat id (st^.timeFormat)

getDateFormat :: ChatState -> T.Text
getDateFormat st = maybe defaultDateFormat id
    (configDateFormat $ st^.csResources.crConfiguration)

getMessageListing :: ChannelId -> ChatState -> Seq.Seq Message
getMessageListing cId st =
    st ^. msgMap . ix cId . ccContents . cdMessages

defaultDateFormat :: T.Text
defaultDateFormat = "%Y-%m-%d"

insertTransitions :: Text -> TimeZone -> Maybe UTCTime -> Seq.Seq Message -> Seq.Seq Message
insertTransitions fmt tz cutoff ms = fst $ F.foldl' nextMsg initState ms
    where
        initState :: (Seq.Seq Message, Maybe Message)
        initState = (mempty, Nothing)

        dateMsg d = Message (getBlocks (T.pack $ formatTime defaultTimeLocale (T.unpack fmt) d))
                            Nothing d (C DateTransition) False False Seq.empty NotAReply Nothing
        newMessagesMsg d = Message (getBlocks (T.pack "New Messages"))
                                   Nothing d (C NewMessagesTransition) False False Seq.empty NotAReply Nothing

        nextMsg :: (Seq.Seq Message, Maybe Message) -> Message -> (Seq.Seq Message, Maybe Message)
        nextMsg (rest, Nothing) msg = (rest Seq.|> msg, Just msg)
        nextMsg (rest, Just prevMsg) msg =
            let toInsert = newMessageTransition cutoff <> dateTransition
                dateTransition =
                    if localDay (utcToLocalTime tz (msg^.mDate)) /= localDay (utcToLocalTime tz (prevMsg^.mDate))
                    then Seq.singleton $ dateMsg (msg^.mDate)
                    else mempty
                newMessageTransition Nothing = mempty
                newMessageTransition (Just cutoffTime) =
                    if prevMsg^.mDate < cutoffTime && msg^.mDate >= cutoffTime
                    then Seq.singleton $ newMessagesMsg cutoffTime
                    else mempty
            in ((rest Seq.>< toInsert) Seq.|> msg, Just msg)

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

renderChannelSelect :: ChatState -> Widget Name
renderChannelSelect st =
    withDefAttr channelSelectPromptAttr $
    (txt "Switch to channel: ") <+>
     (showCursor ChannelSelectString (Location (T.length $ st^.csChannelSelectString, 0)) $
      txt $
      (if T.null $ st^.csChannelSelectString
       then " "
       else st^.csChannelSelectString))

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

inputPreview :: Set T.Text -> ChatState -> Widget Name
inputPreview uSet st = maybePreview
    where
    uname = st^.csMe.userUsernameL
    curContents = getEditContents $ st^.cmdLine
    curStr = T.intercalate "\n" curContents
    previewMsg = previewFromInput uname curStr
    maybePreview = if not $ st^.csShowMessagePreview
                   then emptyWidget
                   else let noPreview = str "(No preview)"
                            msgPreview = case previewMsg of
                              Nothing -> noPreview
                              Just pm -> if T.null curStr
                                         then noPreview
                                         else renderMessage pm True uSet
                        in vLimit 5 msgPreview <=>
                           hBorderWithLabel (withDefAttr clientEmphAttr $ str "[Preview ↑]")

userInputArea :: ChatState -> Widget Name
userInputArea st =
    case st^.csMode of
        ChannelSelect -> renderChannelSelect st
        UrlSelect     -> hCenter $ hBox [ txt "Press "
                                        , withDefAttr clientEmphAttr $ txt "Enter"
                                        , txt " to open the selected URL or "
                                        , withDefAttr clientEmphAttr $ txt "Escape"
                                        , txt " to cancel."
                                        ]
        ChannelScroll -> hCenter $ hBox [ txt "Press "
                                        , withDefAttr clientEmphAttr $ txt "Escape"
                                        , txt " to stop scrolling and resume chatting."
                                        ]
        _             -> renderUserCommandBox st

mainInterface :: ChatState -> Widget Name
mainInterface st =
    (renderChannelList st <+> vBorder <+> mainDisplay)
      <=> bottomBorder
      <=> inputPreview uSet st
      <=> userInputArea st
    where
    mainDisplay = case st^.csMode of
        UrlSelect -> renderUrlList st
        _         -> maybeSubdue $ renderCurrentChannelDisplay uSet st
    uSet = Set.fromList (map _uiName (HM.elems (st^.usrMap)))

    bottomBorder = case st^.csCurrentCompletion of
        Just _ | length (st^.csEditState.cedCompletionAlternatives) > 1 -> completionAlternatives st
        _ -> maybeSubdue $ hLimit channelListWidth hBorder <+> borderElem bsIntersectB <+> hBorder

    maybeSubdue = if st^.csMode == ChannelSelect
                  then forceAttr ""
                  else id

renderUrlList :: ChatState -> Widget Name
renderUrlList st =
    header <=> urlDisplay
    where
        header = withDefAttr channelHeaderAttr $ vLimit 1 $
                 (txt $ "URLs: " <> (st^.csCurrentChannel.ccInfo.cdName)) <+>
                 fill ' '

        urlDisplay = if F.length urls == 0
                     then str "No URLs found in this channel."
                     else renderList renderItem True urls

        urls = st^.csUrlList

        renderItem sel (time, uname, url) = attr sel $ vLimit 2 $
            (vLimit 1 $
             (colorUsername uname <+> fill ' ' <+>
             (renderTime (getDateFormat st) (st^.timeZone) time) <+> str " " <+>
             (renderTime (getTimeFormat st) (st^.timeZone) time))) <=>
            (vLimit 1 (withDefAttr urlAttr $ txt url))

        attr True = forceAttr "urlListSelectedAttr"
        attr False = id
