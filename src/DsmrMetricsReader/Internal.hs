{-# LANGUAGE TemplateHaskell #-}

module DsmrMetricsReader.Internal where

import Control.Concurrent (threadDelay)
import Data.Time (UTCTime(UTCTime), TimeOfDay(TimeOfDay), fromGregorian, timeOfDayToTime)
import Data.Fixed (Pico)
import Data.Void (Void)
import Data.Maybe (isJust, fromJust)
import Text.Megaparsec (Parsec, ParseErrorBundle, parse, errorBundlePretty, some, count, optional)
import Text.Megaparsec.Char (char, digitChar, string, upperChar, eol, printChar, alphaNumChar)
import Text.Megaparsec.Char.Lexer (float, decimal)
import Control.Applicative ((<|>))
import Control.Monad (replicateM)
import Control.Lens ((^.))
import Control.Lens.TH (makeLenses, makePrisms)

-- telegram parser
type Parser = Parsec Void String
data DsmrTelegram = DsmrTelegram {
  _meterID :: MeterID,
  _dsmrFields :: [DsmrField],
  _checkSum :: Checksum} deriving (Eq, Show)
type MeterID = String
type Checksum = Integer

data DsmrField =
      VersionNumber                 {_versionNumber                  :: Integer}
    | Timestamp                     {_timeStamp                      :: UTCTime}
    | EquipmentID                   {_equipmentID                    :: String}
    | EnergyConsumedTariff1         {_energyConsumedTariff1          :: Double}               --kilo Watt hour
    | EnergyConsumedTariff2         {_energyConsumedTariff2          :: Double}               --kilo Watt hour
    | EnergyReturnedTariff1         {_energyReturnedTariff1          :: Double}               --kilo Watt hour
    | EnergyReturnedTariff2         {_energyReturnedTariff2          :: Double}               --kilo Watt hour
    | ActualTariffIndicator         {_actualTariffIndicator          :: Int}
    | ActualPowerConsumption        {_actualPowerConsumption         :: Double}               --kilo Watt
    | ActualPowerReturned           {_actualPowerReturned            :: Double}               --kilo Watt
    | NumberOfPowerFailures         {_numberOfPowerFailures          :: Integer}
    | NumberOfLongPowerFailures     {_numberOfLongPowerFailures      :: Integer}
    | PowerFailureLog               {_powerFailureLog                :: [(UTCTime, Integer)]} -- [(timestamp of failure end, duration in seconds)]
    | NumberOfVoltageSagsPhaseL1    {_numberOfVoltageSagsPhaseL1     :: Integer }
    | NumberOfVoltageSagsPhaseL2    {_numberOfVoltageSagsPhaseL2     :: Integer }
    | ActualCurrentConsumption      {_actualCurrentConsumption       :: Integer}              -- Amperes
    | ActualPowerConsumptionPhaseL1 {_actualPowerConsumptionPhaseL1  :: Double}               -- kilo Watt
    | ActualPowerReturnedPhaseL1    {_actualPowerReturnedPhaseL1     :: Double}               -- kilo Watt
    | SlaveGasMeterDeviceType       {_slaveGasMeterDeviceType        :: Int}
    | GasMeterSerialNumber          {_gasMeterSerialNumber           :: String}
    | GasConsumption                {_gasConsumption                 :: (UTCTime, Double)}    -- (timestamp, amount in m3)
    deriving (Eq, Show)

$(makeLenses ''DsmrField)
$(makePrisms ''DsmrField)
$(makeLenses ''DsmrTelegram)


dsmrTelegramParserP :: Parser DsmrTelegram
dsmrTelegramParserP = do
    char '/'
    meterID <- some (upperChar <|> digitChar)
    eol
    fields <- dsmrfieldsParserP
    char '!'
    checkSum <- some digitChar
    return $ DsmrTelegram meterID fields (read checkSum)

dsmrfieldsParserP :: Parser [DsmrField]
dsmrfieldsParserP = do
  fields <- some (dsmrfieldParserP <|> unknownFieldsParserP)
  return (map fromJust . filter isJust $ fields)
  

dsmrfieldParserP :: Parser (Maybe DsmrField)
dsmrfieldParserP = 
      versionfieldParserP
  <|> timestampfieldParserP
  <|> equipmentIDfieldParserP
  <|> powerConsumedTariff1fieldParserP
  <|> powerConsumedTariff2fieldParserP
  <|> powerReturnedTariff1fieldParserP
  <|> powerReturnedTariff2fieldParserP
  <|> actualTariffIndicatorfieldParserP
  <|> actualPowerConsumptionfieldParserP
  <|> actualPowerReturnedfieldParserP
  <|> numberOfPowerFailuresfieldParserP
  <|> numberOfLongPowerFailuresfieldParserP
  <|> powerFailureLogfieldParserP
  <|> numberOfVoltageSagsPhaseL1fieldParserP
  <|> numberOfVoltageSagsPhaseL2fieldParserP
  <|> actualCurrentConsumptionfieldParserP
  <|> actualPowerConsumptionPhaseL1fieldParserP
  <|> actualPowerReturnedPhaseL1fieldParserP
  <|> slaveGasMeterDeviceTypefieldParserP
  <|> gasMeterSerialNumberfieldParserP
  <|> gasConsumptionfieldParserP
  <|> unknownFieldsParserP


fieldParserP :: String -> Parser a -> (a -> DsmrField) -> Parser (Maybe DsmrField)
fieldParserP dsmrFieldID valueTypeParser fieldConstructor = do
  string dsmrFieldID
  val <- valueParensParserP valueTypeParser
  eol
  return . Just $ fieldConstructor val

valueParensParserP :: Parser a -> Parser a
valueParensParserP valParser = do
  char '('
  val <- valParser
  char ')'
  return val

versionfieldParserP                       = fieldParserP "1-3:0.2.8"      valueIntegerParserP     VersionNumber
timestampfieldParserP                     = fieldParserP "0-0:1.0.0"      valueTimestampParserP   Timestamp
equipmentIDfieldParserP                   = fieldParserP "0-0:96.1.1"     valueStringParserP      EquipmentID
powerConsumedTariff1fieldParserP          = fieldParserP "1-0:1.8.1"      valueEnergyParserP      EnergyConsumedTariff1
powerConsumedTariff2fieldParserP          = fieldParserP "1-0:2.8.1"      valueEnergyParserP      EnergyConsumedTariff2
powerReturnedTariff1fieldParserP          = fieldParserP "1-0:1.8.2"      valueEnergyParserP      EnergyReturnedTariff1
powerReturnedTariff2fieldParserP          = fieldParserP "1-0:2.8.2"      valueEnergyParserP      EnergyReturnedTariff2
actualTariffIndicatorfieldParserP         = fieldParserP "0-0:96.14.0"    valueIntParserP         ActualTariffIndicator
actualPowerConsumptionfieldParserP        = fieldParserP "1-0:1.7.0"      valuePowerParserP       ActualPowerConsumption
actualPowerReturnedfieldParserP           = fieldParserP "1-0:2.7.0"      valuePowerParserP       ActualPowerReturned
numberOfPowerFailuresfieldParserP         = fieldParserP "0-0:96.7.21"    valueIntegerParserP     NumberOfPowerFailures
numberOfLongPowerFailuresfieldParserP     = fieldParserP "0-0:96.7.9"     valueIntegerParserP     NumberOfLongPowerFailures
powerFailureLogfieldParserP               = string       "1-0:99.97.0" >> valueFailureLogParserP  PowerFailureLog
numberOfVoltageSagsPhaseL1fieldParserP    = fieldParserP "1-0:32.32.0"    valueIntegerParserP     NumberOfVoltageSagsPhaseL1
numberOfVoltageSagsPhaseL2fieldParserP    = fieldParserP "1-0:32.36.0"    valueIntegerParserP     NumberOfVoltageSagsPhaseL2
actualCurrentConsumptionfieldParserP      = fieldParserP "1-0:31.7.0"     valueCurrentParserP     ActualCurrentConsumption
actualPowerConsumptionPhaseL1fieldParserP = fieldParserP "1-0:21.7.0"     valuePowerParserP       ActualPowerConsumptionPhaseL1
actualPowerReturnedPhaseL1fieldParserP    = fieldParserP "1-0:22.7.0"     valuePowerParserP       ActualPowerReturnedPhaseL1
slaveGasMeterDeviceTypefieldParserP       = fieldParserP "0-1:24.1.0"     valueIntParserP         SlaveGasMeterDeviceType
gasMeterSerialNumberfieldParserP          = fieldParserP "0-1:96.1.0"     valueStringParserP      GasMeterSerialNumber
gasConsumptionfieldParserP                = string       "0-1:24.2.1"  >> valueGasParserP         GasConsumption
unknownFieldsParserP                      = (string      "0-0:96.13.1" >> valueParensParserP      (pure ()) >> eol >> pure Nothing)
                                        <|> (string      "0-0:96.13.0" >> valueParensParserP      (pure ()) >> eol >> pure Nothing)

-- "/XMX5LGBBFFB231215493\n\
-- \1-3:0.2.8(42)\n\
-- \0-0:1.0.0(200529163319S)\n\
-- \0-0:96.1.1(4530303034303031353934373534343134)\n\
-- \1-0:1.8.1(014765.683*kWh)\n\
-- \1-0:2.8.1(000000.000*kWh)\n\
-- \1-0:1.8.2(014628.043*kWh)\n\
-- \1-0:2.8.2(000000.000*kWh)\n\
-- \0-0:96.14.0(0002)\n\
-- \1-0:1.7.0(03.877*kW)\n\
-- \1-0:2.7.0(00.000*kW)\n\
-- \0-0:96.7.21(00003)\n\
-- \0-0:96.7.9(00002)\n\
-- \1-0:99.97.0(2)(0-0:96.7.19)(170326062519S)(0029642045*s)(160417043131S)(0032998738*s)\n\
-- \1-0:32.32.0(00000)\n\
-- \1-0:32.36.0(00000)\n\
-- \0-0:96.13.1()\n\
-- \0-0:96.13.0()\n\
-- \1-0:31.7.0(017*A)\n\
-- \1-0:21.7.0(03.877*kW)\n\
-- \1-0:22.7.0(00.000*kW)\n\
-- \0-1:24.1.0(003)\n\
-- \0-1:96.1.0(4730303137353931323139313130333134)\n\
-- \0-1:24.2.1(200529160000S)(05277.053*m3)\n\
-- \!7667\n"
-- !7667"

valueFailureLogParserP :: ([(UTCTime, Integer)] -> DsmrField) -> Parser (Maybe DsmrField)
valueFailureLogParserP failureLogContructor = do
  numLogEntries <- valueParensParserP valueIntParserP
  valueParensParserP (string "0-0:96.7.19")
  logEntries <- replicateM numLogEntries logEntryParser
  eol
  return . Just $ failureLogContructor logEntries

logEntryParser :: Parser (UTCTime, Integer)
logEntryParser = do
  timeStampStr <- valueParensParserP valueTimestampParserP
  durationSeconds <- valueParensParserP valueSecondsParserP
  return (timeStampStr, durationSeconds)

valueGasParserP :: ((UTCTime, Double) -> DsmrField) -> Parser (Maybe DsmrField)
valueGasParserP gasContructor= do
  timeStamp <- valueParensParserP valueTimestampParserP
  volume <- valueParensParserP valueVolumeParserP
  eol
  return . Just $ gasContructor (timeStamp, volume)

valueVolumeParserP :: Parser Double
valueVolumeParserP = do
  value <- float
  string "*m3"
  return value

valueEnergyParserP :: Parser Double
valueEnergyParserP = do
  value <- float
  string "*kWh"
  return value

valueCurrentParserP :: Parser Integer
valueCurrentParserP = do
  value <- decimal
  string "*A"
  return value

valuePowerParserP :: Parser Double
valuePowerParserP = do
  value <- float
  string "*kW"
  return value

valueSecondsParserP :: Parser Integer
valueSecondsParserP = do
  value <- valueIntegerParserP
  string "*s"
  return value

valueIntegerParserP :: Parser Integer
valueIntegerParserP = do
  val <- some digitChar
  return (read val :: Integer)

valueIntParserP :: Parser Int
valueIntParserP = do
  val <- decimal
  return $ fromInteger val

valueStringParserP :: Parser String
valueStringParserP = some alphaNumChar

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
valueTimestampParserP :: Parser UTCTime
valueTimestampParserP = do
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
  return $ mkUTCTime
    (((read yearOfDecade :: Integer) + 2000) , (read month :: Int), (read day :: Int))
    (fixDst dst (read hour :: Int) , (read minute :: Int), (read second :: Pico))

-- helper functions
mkUTCTime :: (Integer, Int, Int) -> (Int, Int, Pico) -> UTCTime
mkUTCTime (year, mon, day) (hour, min, sec) =
  UTCTime (fromGregorian year mon day)
          (timeOfDayToTime (TimeOfDay hour min sec))

runDsmrParser :: String -> Either String DsmrTelegram
runDsmrParser input =
  let
    parseResult = parse dsmrTelegramParserP "" input
  in
    case parseResult of
      Left parseErrorBundle -> Left (errorBundlePretty parseErrorBundle)
      Right dsmrTelegram -> Right dsmrTelegram

telegram :: String
telegram = "/XMX5LGBBFFB231215493\n\
\1-3:0.2.8(42)\n\
\0-0:1.0.0(200529163319S)\n\
\0-0:96.1.1(4530303034303031353934373534343134)\n\
\1-0:1.8.1(014765.683*kWh)\n\
\1-0:2.8.1(000000.000*kWh)\n\
\1-0:1.8.2(014628.043*kWh)\n\
\1-0:2.8.2(000000.000*kWh)\n\
\0-0:96.14.0(0002)\n\
\1-0:1.7.0(03.877*kW)\n\
\1-0:2.7.0(00.000*kW)\n\
\0-0:96.7.21(00003)\n\
\0-0:96.7.9(00002)\n\
\1-0:99.97.0(2)(0-0:96.7.19)(170326062519S)(0029642045*s)(160417043131S)(0032998738*s)\n\
\1-0:32.32.0(00000)\n\
\1-0:32.36.0(00000)\n\
\0-0:96.13.1()\n\
\0-0:96.13.0()\n\
\1-0:31.7.0(017*A)\n\
\1-0:21.7.0(03.877*kW)\n\
\1-0:22.7.0(00.000*kW)\n\
\0-1:24.1.0(003)\n\
\0-1:96.1.0(4730303137353931323139313130333134)\n\
\0-1:24.2.1(200529160000S)(05277.053*m3)\n\
\!7667\n"

runDsmrParserPrintError :: String -> IO (Maybe DsmrTelegram)
runDsmrParserPrintError input = do
  let parseResult = runDsmrParser telegram
  case parseResult of
          Left parseError -> do
            putStr (parseError)
            return Nothing
          Right dsmrTelegram -> return $ Just dsmrTelegram

readMetrics :: (DsmrTelegram -> IO ()) ->  IO ()
readMetrics callback =
    do
        --let newTelegram = DsmrTelegram "FakeMeterID" [Energy 0] 666
        parseResult <- runDsmrParserPrintError telegram
        case parseResult of
          Just dsmrTelegram -> callback dsmrTelegram
        threadDelay 5000000
        readMetrics callback

