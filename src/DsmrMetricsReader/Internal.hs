module DsmrMetricsReader.Internal where

import Prelude hiding (min)

import Data.Void (Void)
import Data.Maybe (catMaybes)
import Data.Fixed (Pico)
import Data.Time (UTCTime(UTCTime), TimeOfDay(TimeOfDay), fromGregorian, timeOfDayToTime)
import Text.Megaparsec (Parsec, parse, errorBundlePretty, some, count, optional)
import Text.Megaparsec.Char (char, digitChar, string, upperChar, eol, alphaNumChar)
import Text.Megaparsec.Char.Lexer (float, decimal)
import Control.Applicative ((<|>))
import Control.Monad (replicateM)
import Control.Lens.TH (makeLenses, makePrisms)
import Effects.DsmrTelegramReader (DsmrTelegramReader(..), readTelegram)
import qualified Polysemy as P
import qualified Polysemy.Output as P

import DsmrMetricsReader.Model

type Parser = Parsec Void String

dsmrTelegramParserP :: Parser DsmrTelegram
dsmrTelegramParserP = do
    _ <- char '/'
    meterID_ <- some (upperChar <|> digitChar)
    _ <- eol
    _ <- optional eol
    fields <- dsmrfieldsParserP
    _ <- char '!'
    checkSum_ <- some digitChar
    return $ DsmrTelegram meterID_ fields (read checkSum_)

dsmrfieldsParserP :: Parser [DsmrField]
dsmrfieldsParserP = do
  fields <- some (dsmrfieldParserP <|> unknownFieldsParserP)
  return (catMaybes fields)

unknownFieldsParserP :: Parser (Maybe DsmrField)
unknownFieldsParserP = 
      (string       "0-0:96.13.1" >> valueParensParserP     (pure ()) >> eol >> pure Nothing)
  <|> (string       "0-0:96.13.0" >> valueParensParserP     (pure ()) >> eol >> pure Nothing)

dsmrfieldParserP :: Parser (Maybe DsmrField)
dsmrfieldParserP = 
      fieldParserP "1-3:0.2.8"      valueIntegerParserP     VersionNumber
  <|> fieldParserP "0-0:1.0.0"      valueTimestampParserP   TimeStamp
  <|> fieldParserP "0-0:96.1.1"     valueStringParserP      EquipmentID
  <|> fieldParserP "1-0:1.8.1"      valueEnergyParserP      EnergyConsumedTariff1
  <|> fieldParserP "1-0:2.8.1"      valueEnergyParserP      EnergyConsumedTariff2
  <|> fieldParserP "1-0:1.8.2"      valueEnergyParserP      EnergyReturnedTariff1
  <|> fieldParserP "1-0:2.8.2"      valueEnergyParserP      EnergyReturnedTariff2
  <|> fieldParserP "0-0:96.14.0"    valueIntParserP         ActualTariffIndicator
  <|> fieldParserP "1-0:1.7.0"      valuePowerParserP       ActualPowerConsumption
  <|> fieldParserP "1-0:2.7.0"      valuePowerParserP       ActualPowerReturned
  <|> fieldParserP "0-0:96.7.21"    valueIntegerParserP     NumberOfPowerFailures
  <|> fieldParserP "0-0:96.7.9"     valueIntegerParserP     NumberOfLongPowerFailures
  <|> (string       "1-0:99.97.0" >> valueFailureLogParserP  PowerFailureLog)
  <|> fieldParserP "1-0:32.32.0"    valueIntegerParserP     NumberOfVoltageSagsPhaseL1
  <|> fieldParserP "1-0:32.36.0"    valueIntegerParserP     NumberOfVoltageSagsPhaseL2
  <|> fieldParserP "1-0:31.7.0"     valueCurrentParserP     ActualCurrentConsumption
  <|> fieldParserP "1-0:21.7.0"     valuePowerParserP       ActualPowerConsumptionPhaseL1
  <|> fieldParserP "1-0:22.7.0"     valuePowerParserP       ActualPowerReturnedPhaseL1
  <|> fieldParserP "0-1:24.1.0"     valueIntParserP         SlaveGasMeterDeviceType
  <|> fieldParserP "0-1:96.1.0"     valueStringParserP      GasMeterSerialNumber
  <|> (string       "0-1:24.2.1"  >> valueGasParserP        GasConsumption)
  <|> unknownFieldsParserP


fieldParserP :: String -> Parser a -> (a -> DsmrField) -> Parser (Maybe DsmrField)
fieldParserP dsmrFieldID valueTypeParser fieldConstructor = do
  _ <- string dsmrFieldID
  val <- valueParensParserP valueTypeParser
  _ <- eol
  return . Just $ fieldConstructor val

valueParensParserP :: Parser a -> Parser a
valueParensParserP valParser = do
  _ <- char '('
  val <- valParser
  _ <- char ')'
  return val

valueFailureLogParserP :: ([(UTCTime, Integer)] -> DsmrField) -> Parser (Maybe DsmrField)
valueFailureLogParserP failureLogContructor = do
  numLogEntries <- valueParensParserP valueIntParserP
  _ <- valueParensParserP (string "0-0:96.7.19")
  logEntries <- replicateM numLogEntries logEntryParser
  _ <- eol
  return . Just $ failureLogContructor logEntries

logEntryParser :: Parser (UTCTime, Integer)
logEntryParser = do
  timeStampStr <- valueParensParserP valueTimestampParserP
  durationSeconds <- valueParensParserP valueSecondsParserP
  return (timeStampStr, durationSeconds)

valueGasParserP :: ((UTCTime, Double) -> DsmrField) -> Parser (Maybe DsmrField)
valueGasParserP gasContructor= do
  _timeStamp <- valueParensParserP valueTimestampParserP
  volume <- valueParensParserP valueVolumeParserP
  _ <- eol
  return . Just $ gasContructor (_timeStamp, volume)

valueVolumeParserP :: Parser Double
valueVolumeParserP = do
  value <- float
  _ <- string "*m3"
  return value

valueEnergyParserP :: Parser Double
valueEnergyParserP = do
  value <- float
  _ <- string "*kWh"
  return value

valueCurrentParserP :: Parser Integer
valueCurrentParserP = do
  value <- decimal
  _ <- string "*A"
  return value

valuePowerParserP :: Parser Double
valuePowerParserP = do
  value <- float
  _ <- string "*kW"
  return value

valueSecondsParserP :: Parser Integer
valueSecondsParserP = do
  value <- valueIntegerParserP
  _ <- string "*s"
  return value

valueIntegerParserP :: Parser Integer
valueIntegerParserP = do
  val <- some digitChar
  return (read val :: Integer)

valueIntParserP :: Parser Int
valueIntParserP = fromInteger <$> decimal

valueStringParserP :: Parser String
valueStringParserP = some alphaNumChar

-- FIXME, use getTimeZone to know how to convert to UTC based on DST
-- FIXME, use getCurrentTime to know what decade it is..
valueTimestampParserP :: Parser UTCTime
valueTimestampParserP = do
  let
    fixDst 'S' = (+) 2
    fixDst 'W' = (+) 1
    fixDst dstChar = error $ "invalid dst char format:'" ++ [dstChar] ++ "'"
  yearOfDecade <- count 2 digitChar
  month <- count 2 digitChar
  day <- count 2 digitChar
  hour <- count 2 digitChar
  minute <- count 2 digitChar
  second <- count 2 digitChar
  dst <- char 'S' <|> char 'W'
  return $ mkUTCTime
    ((read yearOfDecade :: Integer) + 2000 , read month :: Int, read day :: Int)
    (fixDst dst (read hour :: Int) , read minute :: Int, read second :: Pico)

-- helper functions
mkUTCTime :: (Integer, Int, Int) -> (Int, Int, Pico) -> UTCTime
mkUTCTime (year, mon, day) (hour, min, sec) =
  UTCTime (fromGregorian year mon day)
          (timeOfDayToTime (TimeOfDay hour min sec))

runDsmrParser :: P.Member (P.Output String) r => String -> P.Sem r (Maybe DsmrTelegram)
runDsmrParser input = do
  let parseResult = parse dsmrTelegramParserP "" input
  case parseResult of
          Left parseErrorBundle -> do
            P.output (errorBundlePretty parseErrorBundle)
            return Nothing
          Right dsmrTelegram -> return $ Just dsmrTelegram

readMetrics :: (P.Members '[DsmrTelegramReader, P.Output String] r) => (Maybe DsmrTelegram -> P.Sem r a) -> P.Sem r a
readMetrics callback =
    do
        telegram <- readTelegram
        parseResult <- runDsmrParser telegram
        callback parseResult
