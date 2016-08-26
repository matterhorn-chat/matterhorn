{-# LANGUAGE MultiWayIf #-}

module Draw where

import           Brick
import           Brick.Widgets.Border
import           Brick.Widgets.Edit (renderEditor)
import           Data.Time.Clock (UTCTime)
import           Data.Time.Format ( formatTime
                                  , defaultTimeLocale )
import           Data.Time.LocalTime ( TimeZone, utcToLocalTime )
import qualified Data.HashMap.Strict as HM
import           Data.HashMap.Strict ( HashMap )
import           Data.List (sortBy)
import           Data.Ord (comparing)
import           Data.Maybe ( listToMaybe, maybeToList )
import           Data.Monoid ((<>))
import           Lens.Micro.Platform
import           Text.LineBreak (breakString, BreakFormat(..))

import           Network.Mattermost
import           Network.Mattermost.Lenses

import           State
import           Themes

wrappedText :: String -> Widget Name
wrappedText msg = Widget Fixed Fixed $ do
  ctx <- getContext
  let w = ctx ^. availWidthL
  render (str (breakString (BreakFormat w 8 '-' Nothing) msg))

renderTime :: TimeZone -> UTCTime -> Widget Name
renderTime tz t =
    -- %R gives HH:MM in 24 hour time
    let timeStr = formatTime defaultTimeLocale "%R" (utcToLocalTime tz t)
    in str "[" <+> withDefAttr timeAttr (str timeStr) <+> str "]"

renderChatMessage :: TimeZone -> Int -> (Int, (UTCTime, String, String)) -> Widget Name
renderChatMessage tz lastIdx (i, (t, u, m)) =
    let f = if i == lastIdx
            then visible
            else id
    in f $ renderTime tz t <+> str " " <+> wrappedText (u ++ ": " ++ m)

mkChannelName :: String -> String
mkChannelName = ('#':)

mkDMChannelName :: String -> String
mkDMChannelName = ('@':)

renderChannelList :: ChatState -> Widget Name
renderChannelList st = hLimit channelListWidth $ vBox
                       [ header "Channels"
                       , vLimit 10 $ viewport NormalChannelList Vertical $ vBox channelNames
                       , header "Users"
                       , viewport DMChannelList Vertical $ vBox dmChannelNames
                       ]
    where
    channelListWidth = 20
    cId = currentChannelId st
    currentChannelName = getChannelName cId st
    header label = hBorderWithLabel $
                   withDefAttr channelListHeaderAttr $
                   str label
    channelNames = [ attr $ str (indicator ++ mkChannelName n)
                   | n <- (st ^. csNames . cnChans)
                   , let indicator = if | current   -> "+"
                                        | unread    -> "!"
                                        | otherwise -> " "
                         attr = if current
                                then visible . withDefAttr currentChannelNameAttr
                                else id
                         current = n == currentChannelName
                         Just chan = st ^. csNames . cnToChanId . at n
                         unread = hasUnread st chan
                   ]
    dmChannelNames = [ attr $ str (indicator ++ mkDMChannelName (u^.userProfileUsernameL))
                     | u <- sortBy (comparing userProfileUsername) (st ^. usrMap & HM.elems)
                     , let indicator = if | current   -> "+"
                                          | unread    -> "!"
                                          | otherwise -> " "
                           attr = if current
                                  then visible . withDefAttr currentChannelNameAttr
                                  else id
                           cname = getDMChannelName (st^.csMe^.userIdL)
                                                    (u^.userProfileIdL)
                           current = cname == currentChannelName
                           unread = hasUnread st cId
                     ]

renderUserCommandBox :: ChatState -> Widget Name
renderUserCommandBox st = prompt <+> inputBox
    where
    prompt = str "> "
    inputBox = renderEditor True (st^.cmdLine)

renderCurrentChannelDisplay :: ChatState -> Widget Name
renderCurrentChannelDisplay st = header <=> hBorder <=> messages
    where
    header = padRight Max $
             withDefAttr channelHeaderAttr $
             case null purposeStr of
                 True -> str $ case chnType of
                   Type "D" ->
                     case findUserByDMChannelName (st^.usrMap)
                                                  chnName
                                                  (st^.csMe^.userIdL) of
                       Nothing -> mkChannelName chnName
                       Just u  -> mkDMChannelName (u^.userProfileUsernameL)
                   _        -> mkChannelName   chnName
                 False -> wrappedText $ mkChannelName chnName <> " - " <> purposeStr
    messages = viewport ChannelMessages Vertical chatText <+> str " "
    chatText = vBox $ renderChatMessage (st ^. timeZone) (length channelMessages - 1) <$>
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
    [ (renderChannelList st <+> vBorder <+> renderCurrentChannelDisplay st)
      <=> hBorder
      <=> renderUserCommandBox st
    ]
