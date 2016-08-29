{-# LANGUAGE MultiWayIf #-}

module Draw where

import           Brick
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import           Brick.Widgets.Edit (renderEditor)
import           Data.Time.Clock (UTCTime)
import           Data.Time.Format ( formatTime
                                  , defaultTimeLocale )
import           Data.Time.LocalTime ( TimeZone, utcToLocalTime )
import qualified Data.HashMap.Strict as HM
import           Data.HashMap.Strict ( HashMap )
import           Data.List (sortBy, isSuffixOf, isPrefixOf)
import           Data.Ord (comparing)
import           Data.Maybe ( listToMaybe, maybeToList )
import           Data.Monoid ((<>))
import           Lens.Micro.Platform

import           Network.Mattermost
import           Network.Mattermost.Lenses

import           State
import           Themes
import           DrawUtil

-- If the config's date format is not set.
defaultDateFormat :: String
defaultDateFormat = "%R"

renderTime :: String -> TimeZone -> UTCTime -> Widget Name
renderTime fmt tz t =
    let timeStr = formatTime defaultTimeLocale fmt (utcToLocalTime tz t)
    in str "[" <+> withDefAttr timeAttr (str timeStr) <+> str "]"

postIsEmote :: Post -> Bool
postIsEmote p =
    and [ HM.lookup "override_icon_url" (postProps p) == Just (""::String)
        , HM.lookup "override_username" (postProps p) == Just ("webhook"::String)
        , ("*" `isPrefixOf` postMessage p)
        , ("*" `isSuffixOf` postMessage p)
        ]

renderChatMessage :: Maybe String -> TimeZone -> Int -> (Int, ((UTCTime, String, String), (Maybe Post))) -> Widget Name
renderChatMessage mFormat tz lastIdx (i, ((t, u, m), mp)) =
    let f = if i == lastIdx
            then visible
            else id
        doFormat prefix wrapped =
            let suffix = drop (length u + length prefix) wrapped
                (first, rest) = case lines suffix of
                    [] -> ("", [])
                    (fl:r) -> (fl, r)
                firstLine = str prefix <+> colorUsername u <+> str first
            in case rest of
                 [] -> firstLine
                 _ -> vBox $ firstLine : (str <$> rest)
        isEmotePost = case mp of
            Nothing -> False
            Just p -> postIsEmote p
        msg = case isEmotePost of
               True -> wrappedText (doFormat "*") ("*" ++ u ++ " " ++ (init $ tail m))
               False -> wrappedText (doFormat "")  (u ++ ": " ++ m)
    in f $ case mFormat of
        Just ""     -> msg
        Just format -> renderTime format tz t            <+> str " " <+> msg
        Nothing     -> renderTime defaultDateFormat tz t <+> str " " <+> msg

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
    channelNames = [ attr $ padRight Max $ str (indicator ++ mkChannelName n)
                   | n <- (st ^. csNames . cnChans)
                   , let indicator = if | unread    -> "!"
                                        | otherwise -> " "
                         attr = if current
                                then visible . withDefAttr currentChannelNameAttr
                                else id
                         current = n == currentChannelName
                         Just chan = st ^. csNames . cnToChanId . at n
                         unread = hasUnread st chan
                   ]
    dmChannelNames = [ attr $ padRight Max $ str indicator <+> colorUsername (mkDMChannelName (u^.userProfileUsernameL))
                     | u <- sortBy (comparing userProfileUsername) (st ^. usrMap & HM.elems)
                     , let indicator = if | unread    -> "!"
                                          | otherwise -> " "
                           attr = if current
                                  then visible . forceAttr currentChannelNameAttr
                                  else id
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
    messages = viewport (ChannelMessages cId) Vertical chatText <+> str " "
    chatText = vBox $ renderChatMessage (st ^. timeFormat) (st ^. timeZone) (length channelMessages - 1) <$>
                      zip [0..] channelMessages
    channelMessages = getMessageListing cId st
    cId = currentChannelId st
    Just chan = getChannel cId st
    chnName = chan^.channelNameL
    chnType = chan^.channelTypeL
    purposeStr = chan^.channelPurposeL

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

chatDraw :: ChatState -> [Widget Name]
chatDraw st =
    [ (renderChannelList st <+> (borderElem bsIntersectR <=>
                                 vLimit normalChannelListHeight vBorder <=>
                                 borderElem bsIntersectR <=> vBorder)
                            <+> renderCurrentChannelDisplay st)
      <=> (hLimit channelListWidth hBorder <+> borderElem bsIntersectB <+> hBorder)
      <=> renderUserCommandBox st
    ]
