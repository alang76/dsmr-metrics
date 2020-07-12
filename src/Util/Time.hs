module Util.Time 
    (
        localTimeStampToUTC,
        utcToLocalTimeStamp,
        mkUTCTime
    ) where

import Data.Time.Calendar(fromGregorian)
import Data.Fixed (Pico)
import Data.Time.Clock(UTCTime(..))
import Data.Time.LocalTime(TimeZone(..), ZonedTime(..), TimeOfDay(..), localTimeToUTC, utcToZonedTime, zonedTimeToLocalTime, timeOfDayToTime)
import Data.Time.Format(parseTimeOrError, defaultTimeLocale, formatTime)
import Effects.Env
import Polysemy as P

localTimeStampToUTC :: String -> TimeZone -> UTCTime
localTimeStampToUTC timeStr timeZone = 
    let localTime = parseTimeOrError True defaultTimeLocale "%y%m%d%H%M%S" . init $ timeStr
    in localTimeToUTC timeZone localTime

utcToLocalTimeStamp :: UTCTime -> TimeZone -> String
utcToLocalTimeStamp utcTime timeZone =
    let 
        zonedTime =  utcToZonedTime timeZone utcTime
        localTime = zonedTimeToLocalTime zonedTime
        timeZoneZoned = zonedTimeZone zonedTime
        dstPostfix = if timeZoneSummerOnly timeZoneZoned then "S" else "W"
    in formatTime defaultTimeLocale "%y%m%d%H%M%S" localTime ++ dstPostfix


mkUTCTime :: (Integer, Int, Int)
          -> (Int, Int, Pico)
          -> UTCTime
mkUTCTime (year, mon, day) (hour, min, sec) =
  UTCTime (fromGregorian year mon day)
          (timeOfDayToTime (TimeOfDay hour min sec))