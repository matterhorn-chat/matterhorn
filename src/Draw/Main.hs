{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PackageImports #-}
module Draw.Main (drawMain) where

import           Brick
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import           Brick.Widgets.Center (center)
import           Brick.Widgets.Edit (renderEditor)
import           Data.Time.Clock (UTCTime, utctDay)
import           Data.Time.Format ( formatTime
                                  , defaultTimeLocale )
import           Data.Time.LocalTime ( TimeZone, utcToLocalTime )
import qualified Data.HashMap.Strict as HM
import           Data.HashMap.Strict ( HashMap )
import           Data.List (sortBy, foldl')
import           Data.Ord (comparing)
import           Data.Maybe ( listToMaybe, maybeToList )
import           Data.Monoid ((<>))
import           Lens.Micro.Platform

import           Text.Regex.TDFA.String (Regex)

import           Network.Mattermost
import           Network.Mattermost.Lenses

import           Highlighting
import           Markdown
import           State
import           Themes
import           Types
import           Draw.Util

-- If the config's date format is not set.
defaultDateFormat :: String
defaultDateFormat = "%R"

renderTime :: String -> TimeZone -> UTCTime -> Widget Name
renderTime fmt tz t =
    let timeStr = formatTime defaultTimeLocale fmt (utcToLocalTime tz t)
    in str "[" <+> withDefAttr timeAttr (str timeStr) <+> str "]"

renderChatMessage :: Regex -> Maybe String -> TimeZone -> Message -> Widget Name
renderChatMessage uPattern mFormat tz msg =
    let t = msg^.mDate
        m = renderMessage (msg^.mText) (msg^.mUserName) (msg^.mType) uPattern
        msgAtch = case msg^.mAttachments of
          [] -> emptyWidget
          _  -> withDefAttr clientMessageAttr
                  (str "  [this message has an attachment]")
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
            Just format -> \w -> renderTime format tz t            <+> str " " <+> w
            Nothing     -> \w -> renderTime defaultDateFormat tz t <+> str " " <+> w
        maybeRenderTimeWith f = case msg^.mType of
            C DateTransition -> id
            _ -> f
    in maybeRenderTimeWith maybeRenderTime fullMsg

mkChannelName :: String -> String
mkChannelName = ('#':)

mkDMChannelName :: String -> String
mkDMChannelName = ('@':)

channelListWidth :: Int
channelListWidth = 20

normalChannelListHeight :: Int
normalChannelListHeight = 10

renderChannelList :: ChatState -> Widget Name
renderChannelList st = hLimit channelListWidth $ vBox
                       [ header "Channels"
                       , vLimit normalChannelListHeight $ viewport NormalChannelList Vertical $ vBox channelNames
                       , header "Users"
                       , viewport DMChannelList Vertical $ vBox dmChannelNames
                       ]
    where
    cId = currentChannelId st
    currentChannelName = getChannelName cId st
    header label = hBorderWithLabel $
                   withDefAttr channelListHeaderAttr $
                   str label
    channelNames = [ decorate $ padRight Max $ str (mkChannelName n)
                   | n <- (st ^. csNames . cnChans)
                   , let decorate = if | current   -> visible . withDefAttr currentChannelNameAttr
                                       | unread    -> withDefAttr unreadChannelAttr
                                       | otherwise -> id
                         current = n == currentChannelName
                         Just chan = st ^. csNames . cnToChanId . at n
                         unread = hasUnread st chan
                   ]

    isSelf :: UserProfile -> Bool
    isSelf u = (st^.csMe.userIdL) == (u^.userProfileIdL)
    usersToList = filter (not . isSelf) $ st ^. usrMap & HM.elems

    dmChannelNames = [ decorate $ padRight Max $ colorUsername' (mkDMChannelName (u^.userProfileUsernameL))
                     | u <- sortBy (comparing userProfileUsername) usersToList
                     , let decorate = if | current   -> visible . forceAttr currentChannelNameAttr
                                         | unread    -> withDefAttr unreadChannelAttr
                                         | otherwise -> id
                           colorUsername' = case unread of
                             True -> str
                             _    -> colorUsername
                           cname = getDMChannelName (st^.csMe^.userIdL)
                                                    (u^.userProfileIdL)
                           current = cname == currentChannelName
                           m_chanId = st^.csNames.cnToChanId.at (userProfileUsername u)
                           unread = maybe False (hasUnread st) m_chanId
                     ]

renderUserCommandBox :: ChatState -> Widget Name
renderUserCommandBox st = prompt <+> inputBox
    where
    prompt = str "> "
    inputBox = renderEditor True (st^.cmdLine)

renderCurrentChannelDisplay :: ChatState -> Widget Name
renderCurrentChannelDisplay st = header <=> messages
    where
    header = withDefAttr channelHeaderAttr $
             padRight Max $
             case null purposeStr of
                 True -> case chnType of
                   Type "D" ->
                     case findUserByDMChannelName (st^.usrMap)
                                                  chnName
                                                  (st^.csMe^.userIdL) of
                       Nothing -> str $ mkChannelName chnName
                       Just u  -> colorUsername $ mkDMChannelName (u^.userProfileUsernameL)
                   _        -> str $ mkChannelName chnName
                 False -> wrappedText str $ mkChannelName chnName <> " - " <> purposeStr
    messages = if chan^.ccInfo.cdLoaded
               then viewport (ChannelMessages cId) Vertical chatText <+> str " "
               else center $ str "[loading channel scrollback]"
    uPattern = mkUsernamePattern (HM.elems (st^.usrMap))
    chatText = vBox $ renderChatMessage uPattern (st ^. timeFormat) (st ^. timeZone) <$>
                      channelMessages
    channelMessages = insertDateBoundaries $ getMessageListing cId st
    cId = currentChannelId st
    Just chan = getChannel cId st
    chnName = chan^.ccInfo.cdName
    chnType = chan^.ccInfo.cdType
    purposeStr = chan^.ccInfo.cdPurpose

dateTransitionFormat :: String
dateTransitionFormat = "%Y-%m-%d"

insertDateBoundaries :: [Message] -> [Message]
insertDateBoundaries ms = fst $ foldl' nextMsg initState ms
    where
        initState :: ([Message], Maybe Message)
        initState = ([], Nothing)

        dateMsg d = Message (getBlocks (formatTime defaultTimeLocale dateTransitionFormat d))
                            Nothing d (C DateTransition) False False []

        nextMsg :: ([Message], Maybe Message) -> Message -> ([Message], Maybe Message)
        nextMsg (rest, Nothing) msg = (rest <> [msg], Just msg)
        nextMsg (rest, Just prevMsg) msg =
            if utctDay (msg^.mDate) /= utctDay (prevMsg^.mDate)
            then (rest <> [dateMsg (msg^.mDate), msg], Just msg)
            else (rest <> [msg], Just msg)

findUserByDMChannelName :: HashMap UserId UserProfile
                        -> String -- ^ the dm channel name
                        -> UserId -- ^ me
                        -> Maybe UserProfile -- ^ you
findUserByDMChannelName userMap dmchan me = listToMaybe
  [ user
  | u <- HM.keys userMap
  , getDMChannelName me u == dmchan
  , user <- maybeToList (HM.lookup u userMap)
  ]

drawMain :: ChatState -> [Widget Name]
drawMain st =
    [ (renderChannelList st <+> (borderElem bsIntersectR <=>
                                 vLimit normalChannelListHeight vBorder <=>
                                 borderElem bsIntersectR <=> vBorder)
                            <+> renderCurrentChannelDisplay st)
      <=> (hLimit channelListWidth hBorder <+> borderElem bsIntersectB <+> hBorder)
      <=> renderUserCommandBox st
    ]
