module Matterhorn.Draw.Util
  ( withBrackets
  , renderTime
  , renderDate
  , renderKeybindingHelp
  , insertDateMarkers
  , getDateFormat
  , mkChannelName
  , userSigilFromInfo
  , multilineHeightLimit
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick
import           Data.List ( intersperse )
import qualified Data.Set as Set
import qualified Data.Text as T
import           Network.Mattermost.Types

import           Matterhorn.Constants ( userSigil, normalChannelSigil )
import           Matterhorn.Themes
import           Matterhorn.TimeUtils
import           Matterhorn.Types
import           Matterhorn.Types.KeyEvents
import           Matterhorn.Events.Keybindings ( firstActiveBinding )


defaultTimeFormat :: Text
defaultTimeFormat = "%R"

defaultDateFormat :: Text
defaultDateFormat = "%Y-%m-%d"

multilineHeightLimit :: Int
multilineHeightLimit = 5

getTimeFormat :: ChatState -> Text
getTimeFormat st =
    maybe defaultTimeFormat id (st^.csResources.crConfiguration.configTimeFormatL)

getDateFormat :: ChatState -> Text
getDateFormat st =
    maybe defaultDateFormat id (st^.csResources.crConfiguration.configDateFormatL)

renderTime :: ChatState -> UTCTime -> Widget Name
renderTime st = renderUTCTime (getTimeFormat st) (st^.timeZone)

renderDate :: ChatState -> UTCTime -> Widget Name
renderDate st = renderUTCTime (getDateFormat st) (st^.timeZone)

renderUTCTime :: Text -> TimeZoneSeries -> UTCTime -> Widget a
renderUTCTime fmt tz t =
    if T.null fmt
    then emptyWidget
    else withDefAttr timeAttr (txt $ localTimeText fmt $ asLocalTime tz t)

renderKeybindingHelp :: ChatState -> Text -> [KeyEvent] -> Widget Name
renderKeybindingHelp st label evs =
  let ppEv ev = withDefAttr clientEmphAttr $ txt (ppBinding (firstActiveBinding kc ev))
      kc = st^.csResources.crConfiguration.configUserKeysL
  in hBox $ (intersperse (txt "/") $ ppEv <$> evs) <> [txt (":" <> label)]

-- | Modifies a message sequence by inserting date transition markers
-- in between messages with different creation dates. Server dates from
-- messages are converted to local time (via the current timezone)
-- and midnight of that timezone used to generate date markers.
insertDateMarkers :: Messages -> Text -> TimeZoneSeries -> Messages
insertDateMarkers ms datefmt tz = foldr (addMessage . dateMsg) ms dateRange
    where dateRange = foldr checkDateChange Set.empty ms
          checkDateChange m = let msgDay = startOfDay (Just tz) (withServerTime (m^.mDate))
                              in if m^.mDeleted then id else Set.insert msgDay
          dateMsg d = let t = localTimeText datefmt $ asLocalTime tz d
                      in newMessageOfType t (C DateTransition) (ServerTime d)

withBrackets :: Widget a -> Widget a
withBrackets w = hBox [str "[", w, str "]"]

userSigilFromInfo :: UserInfo -> Char
userSigilFromInfo u = case u^.uiStatus of
    Offline      -> ' '
    Online       -> '+'
    Away         -> '-'
    DoNotDisturb -> '×'
    Other _      -> '?'

mkChannelName :: ChatState -> ChannelInfo -> Text
mkChannelName st c = T.append sigil t
    where
        t = case c^.cdDMUserId >>= flip userById st of
            Nothing -> c^.cdName
            Just u -> u^.uiName
        sigil = case c^.cdType of
            Private   -> mempty
            Ordinary  -> normalChannelSigil
            Group     -> mempty
            Direct    -> userSigil
            Unknown _ -> mempty
