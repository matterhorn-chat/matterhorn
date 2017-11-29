module Draw.Util where

import Prelude ()
import Prelude.Compat

import Brick
import qualified Data.Text as T
import qualified Data.Set as Set
import Data.Time.Clock (UTCTime(..))
import Data.Time.LocalTime.TimeZone.Series (TimeZoneSeries)
import Lens.Micro.Platform
import Network.Mattermost

import Types
import Types.Channels
import Types.Messages
import Types.Posts
import Types.Users
import TimeUtils
import Themes

defaultTimeFormat :: T.Text
defaultTimeFormat = "%R"

defaultDateFormat :: T.Text
defaultDateFormat = "%Y-%m-%d"

getTimeFormat :: ChatState -> T.Text
getTimeFormat st =
    maybe defaultTimeFormat id (st^.csResources.crConfiguration.to configTimeFormat)

getDateFormat :: ChatState -> T.Text
getDateFormat st =
    maybe defaultDateFormat id (st^.csResources.crConfiguration.to configDateFormat)

renderTime :: ChatState -> UTCTime -> Widget Name
renderTime st = renderUTCTime (getTimeFormat st) (st^.timeZone)

renderDate :: ChatState -> UTCTime -> Widget Name
renderDate st = renderUTCTime (getDateFormat st) (st^.timeZone)

renderUTCTime :: T.Text -> TimeZoneSeries -> UTCTime -> Widget a
renderUTCTime fmt tz t =
    if T.null fmt
    then emptyWidget
    else withDefAttr timeAttr (txt $ localTimeText fmt $ asLocalTime tz t)

insertDateMarkers :: Messages -> T.Text -> TimeZoneSeries -> Messages
insertDateMarkers ms datefmt tz = foldr (addMessage . dateMsg) ms dateRange
    where dateRange = foldr checkDateChange Set.empty ms
          checkDateChange m = let msgDay = startOfDay (Just tz) (m^.mDate)
                              in if m^.mDeleted then id else Set.insert msgDay
          dateMsg d = let t = localTimeText datefmt $ asLocalTime tz d
                      in newMessageOfType t (C DateTransition) d


withBrackets :: Widget a -> Widget a
withBrackets w = hBox [str "[", w, str "]"]

userSigilFromInfo :: UserInfo -> Char
userSigilFromInfo u = case u^.uiStatus of
    Offline -> ' '
    Online  -> '+'
    Away    -> '-'
    Other _ -> '?'

mkChannelName :: ChannelInfo -> T.Text
mkChannelName c = T.cons sigil (c^.cdName)
  where sigil =  case c^.cdType of
          Private   -> '?'
          Ordinary  -> normalChannelSigil
          Group     -> normalChannelSigil
          Direct    -> userSigil
          _         -> '!'

mkDMChannelName :: UserInfo -> T.Text
mkDMChannelName u = T.cons (userSigilFromInfo u) (u^.uiName)
