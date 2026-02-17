module Matterhorn.Draw.Util
  ( withBrackets
  , renderTime
  , renderDate
  , renderKeybindingHelp
  , insertDateLines
  , getDateFormat
  , userSigilFromInfo
  , multilineHeightLimit
  , keyEventBindings
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick
import           Brick.Keybindings

import           Data.List ( intersperse )
import           Data.Maybe ( fromJust )
import qualified Data.Set as Set
import qualified Data.Text as T
import           Network.Mattermost.Types

import           Matterhorn.Constants ( userSigil, normalChannelSigil )
import           Matterhorn.Themes
import           Matterhorn.TimeUtils
import           Matterhorn.Types

defaultTimeFormat :: DateTimeFormat
defaultTimeFormat = fromJust $ dateTimeFormat "%R"

defaultDateFormat :: DateTimeFormat
defaultDateFormat = fromJust $ dateTimeFormat "%Y-%m-%d"

multilineHeightLimit :: Int
multilineHeightLimit = 5

getTimeFormat :: ChatState -> DateTimeFormat
getTimeFormat st =
    fromMaybe defaultTimeFormat (dateTimeFormat =<< fmt)
    where
        fmt = st^.csResources.crConfiguration.configTimeFormatL

getDateFormat :: ChatState -> DateTimeFormat
getDateFormat st =
    fromMaybe defaultDateFormat (dateTimeFormat =<< fmt)
    where
        fmt = st^.csResources.crConfiguration.configDateFormatL

renderTime :: ChatState -> UTCTime -> Widget Name
renderTime st = renderUTCTime (getTimeFormat st) (st^.timeZone)

renderDate :: ChatState -> UTCTime -> Widget Name
renderDate st = renderUTCTime (getDateFormat st) (st^.timeZone)

renderUTCTime :: DateTimeFormat -> TimeZoneSeries -> UTCTime -> Widget a
renderUTCTime fmt tz t =
    withDefAttr timeAttr (txt $ localTimeText fmt $ asLocalTime tz t)

renderKeybindingHelp :: ChatState -> Text -> [KeyEvent] -> Widget Name
renderKeybindingHelp st label evs =
  let ppEv ev = withDefAttr clientEmphAttr $ txt (ppMaybeBinding (firstActiveBinding kc ev))
      kc = st^.csResources.crConfiguration.configUserKeysL
  in hBox $ (intersperse (txt "/") $ ppEv <$> evs) <> [txt (":" <> label)]

-- | Modifies a message sequence by inserting date transition markers
-- in between messages with different creation dates. Server dates from
-- messages are converted to local time (via the current timezone)
-- and midnight of that timezone used to generate date markers.
insertDateLines :: DateTimeFormat -> TimeZoneSeries -> Messages -> Messages
insertDateLines datefmt tz ms = foldr (addMessage . dateMsg) ms dateRange
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

-- | Resolve the specified key event into a pretty-printed
-- representation of the active bindings for that event, using the
-- specified key handler map builder. If the event has more than one
-- active binding, the bindings are comma-delimited in the resulting
-- string.
keyEventBindings :: ChatState
                 -- ^ The current application state
                 -> (KeyConfig KeyEvent -> KeyDispatcher KeyEvent MH)
                 -- ^ The function to obtain the relevant key handler
                 -- map
                 -> KeyEvent
                 -- ^ The key event to look up
                 -> T.Text
keyEventBindings st mkBindingsMap e =
    let keyconf = st^.csResources.crConfiguration.configUserKeysL
        keymap = mkBindingsMap keyconf
    in T.intercalate ","
         [ ppBinding b
         | KeyHandler { khBinding = b
                      , khHandler = h
                      } <- snd <$> keyDispatcherToList keymap
         , kehEventTrigger h == ByEvent e
         ]
