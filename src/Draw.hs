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

renderChatMessage :: TimeZone -> (UTCTime, String, String) -> Widget Name
renderChatMessage tz (t, u, m) =
    renderTime tz t <+> str " " <+> wrappedText (u ++ ": " ++ m)

mkChannelName :: String -> String
mkChannelName = ('#':)

mkDMChannelName :: String -> String
mkDMChannelName = ('@':)

renderChannelList :: ChatState -> Widget Name
renderChannelList st = hLimit channelListWidth $
                       vBox $ header "Channels" : channelNames <>
                              (header "Users" : dmChannelNames)
    where
    channelListWidth = 20
    cId = currentChannelId st
    currentChannelName = getChannelName cId st
    header label = hBorderWithLabel $
                   withDefAttr channelListHeaderAttr $
                   str label
    channelNames = [ attr $ str (indicator ++ mkChannelName n)
                   | n <- (st ^. csNames . cnChans)
                   , let indicator = if current then "+" else " "
                         attr = if current
                                then withDefAttr currentChannelNameAttr
                                else id
                         current = n == currentChannelName
                   ]
    dmChannelNames = [ attr $ str (indicator ++ mkDMChannelName (u^.userProfileUsernameL))
                     | u <- sortBy (comparing userProfileUsername) (st ^. usrMap & HM.elems)
                     , let indicator = if current then "+" else " "
                           attr = if current
                                  then withDefAttr currentChannelNameAttr
                                  else id
                           cname = getDMChannelName (st^.csMe^.userIdL)
                                                    (u^.userProfileIdL)
                           current = cname == currentChannelName
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
    chatText = vBox $ renderChatMessage (st ^. timeZone) <$> channelMessages
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
