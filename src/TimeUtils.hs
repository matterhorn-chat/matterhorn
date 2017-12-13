module TimeUtils
    ( lookupLocalTimeZone
    , startOfDay
    , justAfter, justBefore
    , asLocalTime
    , localTimeText
    )
    where

import qualified Data.Text as T
import           Data.Time.Clock (UTCTime(..))
import           Data.Time.Format (formatTime, defaultTimeLocale)
import           Data.Time.LocalTime (LocalTime(..), TimeOfDay(..))
import           Data.Time.LocalTime.TimeZone.Olson (getTimeZoneSeriesFromOlsonFile)
import           Data.Time.LocalTime.TimeZone.Series (TimeZoneSeries
                                                     , localTimeToUTC'
                                                     , utcToLocalTime')
import           Network.Mattermost.Types (ServerTime(..))


-- | Get the timezone series that should be used for converting UTC
-- times into local times with appropriate DST adjustments.
lookupLocalTimeZone :: IO TimeZoneSeries
lookupLocalTimeZone = getTimeZoneSeriesFromOlsonFile "/etc/localtime"


-- | Sometimes it is convenient to render a divider between messages;
-- the 'justAfter' function can be used to get a time that is after
-- the input time but by such a small increment that there is unlikely
-- to be anything between (or at) the result.  Adding the divider
-- using this timestamp value allows the general sorting based on
-- timestamps to operate normally (whereas a type-match for a
-- non-timestamp-entry in the sort operation would be considerably
-- more complex).
justAfter :: ServerTime -> ServerTime
justAfter = ServerTime . justAfterUTC . withServerTime
    where justAfterUTC time = let UTCTime d t = time in UTCTime d (succ t)


-- | Obtain a time value that is just moments before the input time;
-- see the comment for the 'justAfter' function for more details.
justBefore :: ServerTime -> ServerTime
justBefore = ServerTime . justBeforeUTC . withServerTime
    where justBeforeUTC time = let UTCTime d t = time in UTCTime d (pred t)


-- | The timestamp for the start of the day associated with the input
-- timestamp.  If timezone information is supplied, then the returned
-- value will correspond to when the day started in that timezone;
-- otherwise it is the start of the day in a timezone aligned with
-- UTC.
startOfDay :: Maybe TimeZoneSeries -> UTCTime -> UTCTime
startOfDay Nothing time = let UTCTime d _ = time in UTCTime d 0
startOfDay (Just tz) time = let lt = utcToLocalTime' tz time
                                ls = LocalTime (localDay lt) (TimeOfDay 0 0 0)
                            in localTimeToUTC' tz ls


-- | Convert a UTC time value to a local time.
asLocalTime :: TimeZoneSeries -> UTCTime -> LocalTime
asLocalTime = utcToLocalTime'


-- | Local time in displayable format
localTimeText :: T.Text -> LocalTime -> T.Text
localTimeText fmt time = T.pack $ formatTime defaultTimeLocale (T.unpack fmt) time
