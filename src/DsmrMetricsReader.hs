module DsmrMetricsReader
    ( readMetrics,
      DsmrMetric(PowerConsumedTariff1, PowerConsumedTariff2, PowerReturnedTariff1, PowerReturnedTariff2)
    ) where

import Control.Concurrent (threadDelay)
import Data.Time (UTCTime, fromGregorian, timeOfDayToTime)
import Data.Fixed (Pico)
import Data.Void (Void)
import Text.Megaparsec (Parsec, some, count)
import Text.Megaparsec.Char (char, digitChar, string, upperChar, eol)
import Control.Applicative ((<|>))

--import Text.Megaparsec (Parsec, optional, , parseMaybe)

-- telegram parser
type Parser = Parsec Void String
data DsmrTelegram = DsmrTelegram MeterID [DsmrField] Checksum
type MeterID = Integer
type Checksum = Integer

data DsmrField = 
      VersionNumber Int
    | Timestamp UTCTime
    | EquipmentID String
    | PowerConsumedTariff1 Int --Watt
    | PowerConsumedTariff2 Int --Watt
    | PowerReturnedTariff1 Int --Watt
    | PowerReturnedTariff2 Int --Watt
    | ActualTariffIndicator Int
    | ActualPowerConsumption Int --Watt
    | ActualPowerReturned Int -- Watt
    | NumberOfPowerFailures Int
    | NumberOfLongPowerFailures Int
    | PowerFailureLog [(UTCTime, Int)] --Timestamp of failure end, duration in seconds
    | NumberOfVoltageSagsPhaseL1
    | NumberOfVoltageSagsPhaseL2
    | ActualCurrentConsumption Int -- Amperes
    | ActualPowerConsumptionPhaseL1 Int --Watt
    | ActualPowerReturnedPhaseL1 Integer -- Watt
    | SlaveGasMeterDeviceType Integer 
    | GasMeterSerialNumber String
    | GasConsumption (UTCTime,Int) -- (timestamp, amount in m3)

-- telegram = "
-- /XMX5LGBBFFB231215493
-- 1-3:0.2.8(42)
-- 0-0:1.0.0(200529163319S)
-- 0-0:96.1.1(4530303034303031353934373534343134)
-- 1-0:1.8.1(014765.683*kWh)
-- 1-0:2.8.1(000000.000*kWh)
-- 1-0:1.8.2(014628.043*kWh)
-- 1-0:2.8.2(000000.000*kWh)
-- 0-0:96.14.0(0002)
-- 1-0:1.7.0(03.877*kW)
-- 1-0:2.7.0(00.000*kW)
-- 0-0:96.7.21(00003)
-- 0-0:96.7.9(00002)
-- 1-0:99.97.0(2)(0-0:96.7.19)(170326062519S)(0029642045*s)(160417043131S)(0032998738*s)
-- 1-0:32.32.0(00000)
-- 1-0:32.36.0(00000)
-- 0-0:96.13.1()
-- 0-0:96.13.0()
-- 1-0:31.7.0(017*A)
-- 1-0:21.7.0(03.877*kW)
-- 1-0:22.7.0(00.000*kW)
-- 0-1:24.1.0(003)
-- 0-1:96.1.0(4730303137353931323139313130333134)
-- 0-1:24.2.1(200529160000S)(05277.053*m3)
-- !7667"

dsmrTelegramParserP :: Parser DsmrTelegram
dsmrTelegramParserP = do
    char '/'
    meterID <- some (upperChar <|> digitChar)
    eol
    fields <- some dsmrFieldParserP
    char '!'
    checkSum <- some digitChar
    return $ DsmrTelegram meterID fields checkSum


dsmrFieldParserP :: Parser DsmrField
dsmrFieldParserP = versionFieldParserP <|> timestampFieldParserP

versionFieldParserP :: Parser DsmrField
versionFieldParserP = do
  string "1-3:0.2.8"
  version <- valueParser valueIntParserP
  return $ VersionNumber version

valueParser :: Parser a -> Parser a
valueParser valueTypeParserP = do
  char '('
  val <- valueTypeParserP
  char ')'
  return val

valueIntParserP :: Parser Integer
valueIntParserP = do
  val <- some digitChar
  return (read val :: Integer)

fieldIdParserP :: Parser String
fieldIdParserP = do
  some digitChar
  char '-'
  some digitChar
  char ':'
  some digitChar
  char '.'
  some digitChar
  char '.'
  some digitChar
  char '.'
  some digitChar


-- FIXME, use getTimeZone to know how to convert to UTC based on DST
-- FIXME, use getCurrentTime to know what decade it is..
timestampFieldParserP :: Parser DsmrField
timestampFieldParserP = do
  let
    fixDst 'S' = (+) 2
    fixDst 'W' = (+) 1
  yearOfDecade <- count 2 digitChar
  month <- count 2 digitChar
  day <- count 2 digitChar
  hour <- count 2 digitChar
  minute <- count 2 digitChar
  second <- count 2 digitChar
  dst <- char 'S' <|> char 'W'
  return . Timestamp $ mkUTCTime 
    (((read yearOfDecade :: Int) + 2000) , (read month :: Int), (read day :: Int)) 
    (fixDst (read hour :: Int) , (read minute :: Int), (read second :: Pico))
      
-- helper functions
mkUTCTime :: (Int, Int, Int) -> (Int, Int, Pico) -> UTCTime
mkUTCTime (year, mon, day) (hour, min, sec) =
  UTCTime (fromGregorian year mon day) 
          (timeOfDayToTime (TimeOfDay hour min sec))

readMetrics :: (DsmrTelegram -> IO ()) ->  IO ()
readMetrics callback =
    do
        let newMetric = PowerConsumedTariff1 0
        _ <- callback newMetric
        threadDelay 5000000
        readMetrics callback
