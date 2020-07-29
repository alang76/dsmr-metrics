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
import Data.Time.Format(parseTimeM, defaultTimeLocale, formatTime)

localTimeStampToUTC :: TimeZone ->  Integer -> String -> Maybe UTCTime
localTimeStampToUTC timeZone century timeStr = do
    localTime <- parseTimeM True defaultTimeLocale "%Y%m%d%H%M%S" . (show century ++) . init $ timeStr
    return $ localTimeToUTC timeZone localTime

utcToLocalTimeStamp :: TimeZone -> UTCTime -> String
utcToLocalTimeStamp timeZone utcTime =
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