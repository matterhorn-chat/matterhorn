{-# LANGUAGE MultiWayIf #-}
module Draw.Main (drawMain) where

import           Brick
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import           Brick.Widgets.Center (hCenter)
import           Brick.Widgets.Edit (editContentsL, renderEditor, getEditContents)
import           Brick.Widgets.List (renderList)
import           Control.Applicative
import           Control.Arrow ((>>>))
import           Control.Monad.Trans.Reader (withReaderT)
import           Data.Time.Clock (UTCTime(..))
import           Data.Time.Calendar (fromGregorian)
import           Data.Time.Format ( formatTime
                                  , defaultTimeLocale )
import           Data.Time.LocalTime ( TimeZone, utcToLocalTime, localDay )
import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as Seq
import qualified Data.Foldable as F
import           Data.HashMap.Strict ( HashMap )
import           Data.List (intersperse)
import qualified Data.Map.Strict as Map
import           Data.Maybe (listToMaybe, maybeToList, catMaybes)
import           Data.Monoid ((<>))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Zipper (cursorPosition, insertChar, getText, gotoEOL)
import           Lens.Micro.Platform

import           Prelude

import           Network.Mattermost
import           Network.Mattermost.Lenses

import qualified Graphics.Vty as Vty

import           Markdown
import           State
import           State.Common
import           Themes
import           Types
import           Draw.Util

renderChatMessage :: Set Text -> (UTCTime -> Widget Name) -> Message -> Widget Name
renderChatMessage uSet renderTimeFunc msg =
    let m = renderMessage msg True uSet
        msgAtch = if Seq.null (msg^.mAttachments)
          then emptyWidget
          else withDefAttr clientMessageAttr $ vBox
                 [ txt ("  [attached: `" <> a^.attachmentName <> "`]")
                 | a <- F.toList (msg^.mAttachments)
                 ]
        msgReac = if Map.null (msg^.mReactions)
          then emptyWidget
          else let renderR e 1 = " [" <> e <> "]"
                   renderR e n
                     | n > 1     = " [" <> e <> " " <> T.pack (show n) <> "]"
                     | otherwise = ""
                   reacMsg = Map.foldMapWithKey renderR (msg^.mReactions)
               in withDefAttr emojiAttr $ txt ("   " <> reacMsg)
        msgTxt =
          case msg^.mUserName of
            Just _
              | msg^.mType == CP Join || msg^.mType == CP Leave ->
                  withDefAttr clientMessageAttr m
              | otherwise -> m
            Nothing ->
                case msg^.mType of
                    C DateTransition -> withDefAttr dateTransitionAttr (hBorderWithLabel m)
                    C NewMessagesTransition -> withDefAttr newMessageTransitionAttr (hBorderWithLabel m)
                    C Error -> withDefAttr errorMessageAttr m
                    _ -> withDefAttr clientMessageAttr m
        fullMsg = msgTxt <=> msgAtch <=> msgReac
        maybeRenderTime w = renderTimeFunc (msg^.mDate) <+> txt " " <+> w
        maybeRenderTimeWith f = case msg^.mType of
            C DateTransition -> id
            C NewMessagesTransition -> id
            _ -> f
    in maybeRenderTimeWith maybeRenderTime fullMsg

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
    [ ChannelListEntry cname sigil uname colorUsername' unread recent
    | u <- sortedUserList st
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
previewFromInput _ s | s == T.singleton cursorSentinel = Nothing
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
                           -- The date is not used for preview
                           -- rendering, but we need to provide one.
                           -- Ideally we'd just today's date, but the
                           -- rendering function is pure so we can't.
                           , _mType          = msgTy
                           , _mPending       = False
                           , _mDeleted       = False
                           , _mAttachments   = mempty
                           , _mInReplyToMsg  = NotAReply
                           , _mPostId        = Nothing
                           , _mReactions     = mempty
                           , _mOriginalPost  = Nothing
                           }

renderUserCommandBox :: Set T.Text -> ChatState -> Widget Name
renderUserCommandBox uSet st =
    let prompt = txt $ case st^.csEditState.cedEditMode of
            Replying _ _ -> "reply> "
            Editing _    -> "edit> "
            NewPost      -> "> "
        inputBox = renderEditor True (st^.cmdLine)
        curContents = getEditContents $ st^.cmdLine
        multilineContent = length curContents > 1
        multilineHints =
            (borderElem bsHorizontal) <+>
            (str $ "[" <> (show $ (+1) $ fst $ cursorPosition $
                                  st^.cmdLine.editContentsL) <>
                   "/" <> (show $ length curContents) <> "]") <+>
            (hBorderWithLabel $ withDefAttr clientEmphAttr $
             (str "In multi-line mode. Press M-e to finish."))

        replyDisplay = case st^.csEditState.cedEditMode of
            Replying msg _ ->
                let msgWithoutParent = msg & mInReplyToMsg .~ NotAReply
                in hBox [ str " "
                        , borderElem bsCornerTL
                        , str "▸"
                        , addEllipsis $ renderMessage msgWithoutParent True uSet
                        ]
            _ -> emptyWidget

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
    in replyDisplay <=> commandBox

maxMessageHeight :: Int
maxMessageHeight = 200

renderSingleMessage :: ChatState -> Set T.Text -> Message -> Widget Name
renderSingleMessage st uSet msg = renderChatMessage uSet (withBrackets . renderTime st) msg

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
                    str "<< Press C-b to load more messages >>") :
                   (F.toList $ renderSingleMessage st uSet <$> channelMessages)
        MessageSelect ->
            renderMessagesWithSelect (st^.csMessageSelect) channelMessages
        MessageSelectDeleteConfirm ->
            renderMessagesWithSelect (st^.csMessageSelect) channelMessages
        _ -> renderLastMessages channelMessages

    -- The first case here should never happen because if we're in
    -- MessageSelect mode, we should only get into that mode when the
    -- selected message in the selection state is present (Just).
    renderMessagesWithSelect (MessageSelectState Nothing) msgs =
        renderLastMessages msgs
    renderMessagesWithSelect (MessageSelectState (Just selPostId)) msgs =
        -- In this case, we want to fill the message list with messages
        -- but use the post ID as a cursor. To do this efficiently we
        -- only want to render enough messages to fill the screen.
        --
        -- If the message area is H rows high, this actually renders at
        -- most 2H rows' worth of messages and then does the appropriate
        -- cropping. This way we can simplify the math needed to figure
        -- out how to crop while bounding the number of messages we
        -- render around the cursor.
        --
        -- First, we sanity-check the application state because under
        -- some conditions, the selected message might be gone (e.g.
        -- deleted).
        case Seq.findIndexR (\m -> m^.mPostId == Just selPostId) msgs of
            Nothing -> renderLastMessages msgs
            Just idx ->
                case Seq.lookup idx msgs of
                    Nothing -> renderLastMessages msgs
                    Just curMsg -> unsafeMessageSelectList msgs idx curMsg

    unsafeMessageSelectList msgs idx curMsg = Widget Greedy Greedy $ do
        ctx <- getContext

        let relaxHeight c = c & availHeightL .~ (max maxMessageHeight (c^.availHeightL))

        -- Render the message associated with the current post ID.
        curMsgResult <- withReaderT relaxHeight $ render $
            forceAttr messageSelectAttr $
            padRight Max $ renderSingleMessage st uSet curMsg

        let targetHeight = ctx^.availHeightL
            upperHeight = targetHeight `div` 2
            lowerHeight = targetHeight - upperHeight

            goDown :: Seq.Seq Message -> Int -> Int -> Vty.Image -> RenderM Name (Int, Vty.Image)
            goDown ms maxHeight num img
                | Seq.null ms =
                    return (num, img)
                | Vty.imageHeight img >= maxHeight =
                    return (num, img)
                | otherwise =
                    case Seq.viewl ms of
                        Seq.EmptyL -> return (num, img)
                        msg Seq.:< ms' -> do
                            result <- case msg^.mDeleted of
                                True -> return Vty.emptyImage
                                False -> do
                                    r <- withReaderT relaxHeight $
                                           render $ padRight Max $ renderSingleMessage st uSet msg
                                    return $ r^.imageL
                            goDown ms' maxHeight (num + 1) (Vty.vertJoin img result)

            goUp :: Seq.Seq Message -> Int -> Vty.Image -> RenderM Name Vty.Image
            goUp ms maxHeight img
                | Vty.imageHeight img >= maxHeight =
                    return $ Vty.cropTop maxHeight img
                | Seq.null ms =
                    return img
                | otherwise =
                    case Seq.viewr ms of
                        Seq.EmptyR -> return img
                        ms' Seq.:> msg -> do
                            result <- case msg^.mDeleted of
                                True -> return Vty.emptyImage
                                False -> do
                                    r <- withReaderT relaxHeight $
                                           render $ padRight Max $ renderSingleMessage st uSet msg
                                    return $ r^.imageL
                            goUp ms' maxHeight $ Vty.vertJoin result img

        let (before, after) = Seq.splitAt idx msgs

        (_, lowerHalf) <- goDown (Seq.drop 1 after) targetHeight 0 Vty.emptyImage
        upperHalf <- goUp before targetHeight Vty.emptyImage

        let curHeight = Vty.imageHeight $ curMsgResult^.imageL
            uncropped = upperHalf Vty.<-> curMsgResult^.imageL Vty.<-> lowerHalf
            img = if Vty.imageHeight lowerHalf < (lowerHeight - curHeight)
                  then Vty.cropTop targetHeight uncropped
                  else if Vty.imageHeight upperHalf < upperHeight
                       then Vty.cropBottom targetHeight uncropped
                       else Vty.cropTop upperHeight upperHalf Vty.<->
                            curMsgResult^.imageL Vty.<->
                            Vty.cropBottom (lowerHeight - curHeight) lowerHalf

        return $ emptyResult & imageL .~ img

    channelMessages =
        insertTransitions (getDateFormat st)
                          (st ^. timeZone)
                          (getNewMessageCutoff cId st)
                          (getMessageListing cId st)

    renderLastMessages :: Seq.Seq Message -> Widget Name
    renderLastMessages msgs =
        Widget Greedy Greedy $ do
            ctx <- getContext

            let targetHeight = ctx^.availHeightL
                relaxHeight c = c & availHeightL .~ (max maxMessageHeight (c^.availHeightL))
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
                                        r <- withReaderT relaxHeight $
                                               render $ padRight Max $ renderSingleMessage st uSet msg
                                        return $ r^.imageL
                                go ms' $ Vty.vertJoin result img

            img <- Vty.cropTop targetHeight <$> go msgs Vty.emptyImage
            return $ emptyResult & imageL .~ img

    cId = st^.csCurrentChannelId
    chan = st^.csCurrentChannel
    chnName = chan^.ccInfo.cdName
    chnType = chan^.ccInfo.cdType
    topicStr = chan^.ccInfo.cdHeader

getMessageListing :: ChannelId -> ChatState -> Seq.Seq Message
getMessageListing cId st =
    st ^. msgMap . ix cId . ccContents . cdMessages

insertTransitions :: Text -> TimeZone -> Maybe UTCTime -> Seq.Seq Message -> Seq.Seq Message
insertTransitions fmt tz cutoff ms = fst $ F.foldl' nextMsg initState ms
    where
        initState :: (Seq.Seq Message, Maybe Message)
        initState = (mempty, Nothing)

        dateMsg d = Message (getBlocks (T.pack $ formatTime defaultTimeLocale (T.unpack fmt)
                                                 (utcToLocalTime tz d)))
                            Nothing d (C DateTransition) False False
                            Seq.empty NotAReply Nothing mempty Nothing
        newMessagesMsg d = Message (getBlocks (T.pack "New Messages"))
                                   Nothing d (C NewMessagesTransition)
                                   False False Seq.empty NotAReply
                                   Nothing mempty Nothing

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
            in if msg^.mDeleted
               then (rest, Just prevMsg)
               else ((rest Seq.>< toInsert) Seq.|> msg, Just msg)

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

messageSelectBottomBar :: ChatState -> Widget Name
messageSelectBottomBar st =
    let optionStr = T.intercalate " " $ catMaybes $ mkOption <$> options
        mkOption (f, k, desc) = if f postMsg
                                then Just $ k <> ":" <> desc
                                else Nothing
        numURLs = Seq.length $ msgURLs postMsg
        s = if numURLs == 1 then "" else "s"
        hasURLs = numURLs > 0
        openUrlsMsg = "open " <> (T.pack $ show numURLs) <> " URL" <> s
        options = [ (const True,    "r", "reply")
                  , (isMine st,     "e", "edit")
                  , (isMine st,     "d", "delete")
                  , (const hasURLs, "o", openUrlsMsg)
                  ]
        Just postMsg = getSelectedMessage st

    in hBox [ borderElem bsHorizontal
            , txt "["
            , withDefAttr messageSelectStatusAttr $
              txt $ "Message select: " <> optionStr
            , txt "]"
            , hBorder
            ]

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

previewMaxHeight :: Int
previewMaxHeight = 5

maybePreviewViewport :: Widget Name -> Widget Name
maybePreviewViewport w =
    Widget Greedy Fixed $ do
        result <- render w
        case (Vty.imageHeight $ result^.imageL) > previewMaxHeight of
            False -> return result
            True ->
                render $ vLimit previewMaxHeight $ viewport MessagePreviewViewport Vertical $
                         (Widget Fixed Fixed $ return result)

inputPreview :: Set T.Text -> ChatState -> Widget Name
inputPreview uSet st | not $ st^.csShowMessagePreview = emptyWidget
                     | otherwise = thePreview
    where
    uname = st^.csMe.userUsernameL
    -- Insert a cursor sentinel into the input text just before
    -- rendering the preview. We use the inserted sentinel (which is
    -- not rendered) to get brick to ensure that the line the cursor is
    -- on is visible in the preview viewport. We put the sentinel at
    -- the *end* of the line because it will still influence markdown
    -- parsing and can create undesirable/confusing churn in the
    -- rendering while the cursor moves around. If the cursor is at the
    -- end of whatever line the user is editing, that is very unlikely
    -- to be a problem.
    curContents = getText $ (gotoEOL >>> insertChar cursorSentinel) $
                  st^.cmdLine.editContentsL
    curStr = T.intercalate "\n" curContents
    previewMsg = previewFromInput uname curStr
    thePreview = let noPreview = str "(No preview)"
                     msgPreview = case previewMsg of
                       Nothing -> noPreview
                       Just pm -> if T.null curStr
                                  then noPreview
                                  else renderMessage pm True uSet
                 in (maybePreviewViewport msgPreview) <=>
                    hBorderWithLabel (withDefAttr clientEmphAttr $ str "[Preview ↑]")

userInputArea :: Set T.Text -> ChatState -> Widget Name
userInputArea uSet st =
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
        MessageSelectDeleteConfirm -> renderDeleteConfirm
        _             -> renderUserCommandBox uSet st

renderDeleteConfirm :: Widget Name
renderDeleteConfirm =
    hCenter $ txt "Are you sure you want to delete the selected message? (y/n)"

mainInterface :: ChatState -> Widget Name
mainInterface st =
    (renderChannelList st <+> vBorder <+> mainDisplay)
      <=> bottomBorder
      <=> inputPreview uSet st
      <=> userInputArea uSet st
    where
    mainDisplay = case st^.csMode of
        UrlSelect -> renderUrlList st
        _         -> maybeSubdue $ renderCurrentChannelDisplay uSet st
    uSet = Set.fromList (map _uiName (HM.elems (st^.usrMap)))

    bottomBorder = case st^.csMode of
        MessageSelect -> messageSelectBottomBar st
        _ -> case st^.csCurrentCompletion of
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

        renderItem sel link =
          let time = link^.linkTime
          in attr sel $ vLimit 2 $
            (vLimit 1 $
             hBox [ colorUsername (link^.linkUser)
                  , if link^.linkName == link^.linkURL
                      then emptyWidget
                      else (txt ": " <+> (renderText $ link^.linkName))
                  , fill ' '
                  , renderDate st time
                  , str " "
                  , renderTime st time
                  ] ) <=>
            (vLimit 1 (renderText $ link^.linkURL))

        attr True = forceAttr "urlListSelectedAttr"
        attr False = id
