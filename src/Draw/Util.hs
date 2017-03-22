module Draw.Util where

import Prelude ()
import Prelude.Compat

import Brick
import qualified Data.Text as T
import Data.Time.Clock (UTCTime(..))
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.LocalTime (TimeZone, utcToLocalTime)
import Lens.Micro.Platform
import Network.Mattermost

import Types
import Themes

defaultTimeFormat :: T.Text
defaultTimeFormat = "%R"

defaultDateFormat :: T.Text
defaultDateFormat = "%Y-%m-%d"

getTimeFormat :: ChatState -> T.Text
getTimeFormat st = maybe defaultTimeFormat id (st^.timeFormat)

getDateFormat :: ChatState -> T.Text
getDateFormat st = maybe defaultDateFormat id (st^.dateFormat)

renderTime :: ChatState -> UTCTime -> Widget Name
renderTime st = renderUTCTime (getTimeFormat st) (st^.timeZone)

renderDate :: ChatState -> UTCTime -> Widget Name
renderDate st = renderUTCTime (getDateFormat st) (st^.timeZone)

renderUTCTime :: T.Text -> TimeZone -> UTCTime -> Widget a
renderUTCTime fmt tz t =
    let timeStr = T.pack $ formatTime defaultTimeLocale (T.unpack fmt) (utcToLocalTime tz t)
    in if T.null fmt
       then emptyWidget
       else withDefAttr timeAttr (txt timeStr)

withBrackets :: Widget a -> Widget a
withBrackets w = str "[" <+> w <+> str "]"

userSigil :: UserInfo -> Char
userSigil u = case u^.uiStatus of
    Offline -> ' '
    Online  -> '+'
    Away    -> '-'
    Other _ -> '?'

mkChannelName :: ChannelInfo -> T.Text
mkChannelName c = T.cons sigil (c^.cdName)
  where sigil =  case c^.cdType of
          Private   -> '?'
          Ordinary  -> '#'
          Direct    -> '@'
          _         -> '!'

mkDMChannelName :: UserInfo -> T.Text
mkDMChannelName u = T.cons (userSigil u) (u^.uiName)
