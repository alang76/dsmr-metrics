{-# LANGUAGE PartialTypeSignatures #-}

module DsmrMetricsReader.Internal where

import Prelude hiding (min)

import Data.Void (Void)
import Data.Maybe (catMaybes, fromJust)
import Data.Time (UTCTime)
import Text.Megaparsec (Parsec, parse, errorBundlePretty, some, many, count, optional)
import Text.Megaparsec.Char (char, digitChar, string, eol, alphaNumChar)
import Text.Megaparsec.Char.Lexer (float, decimal)
import Control.Applicative ((<|>))
import Control.Monad (replicateM)
import qualified Polysemy as P
import qualified Polysemy.Output as P

import Effects.DsmrTelegramReader (DsmrTelegramReader(..), readTelegram)
import Effects.Env (Env(..), getEnvironmentTimeZone, getEnvironmentTimeCentury)
import DsmrMetricsReader.Model
import Util.Time (localTimeStampToUTC)

type Parser = Parsec Void String

dsmrTelegramParserP :: P.Member Env r => P.Sem r (Parser DsmrTelegram)
dsmrTelegramParserP = do
  fieldsParser <- dsmrFieldsParserP
  return $ do
      _ <- char '/'
      meterID_ <- many $ alphaNumChar --upperChar <|> digitChar
      _ <- eol
      _ <- optional eol
      fields <- fieldsParser
      _ <- char '!'
      checkSum_ <- some digitChar
      return $ DsmrTelegram meterID_ fields (read checkSum_)

dsmrFieldsParserP :: P.Member Env r => P.Sem r (Parser [DsmrField])
dsmrFieldsParserP = do
  leftParser <- dsmrFieldParserP
  rightParser <- unknownFieldsParserP
  let
    fieldParser :: Parser [DsmrField]
    fieldParser = do
      fields <- some (leftParser <|> rightParser)
      return (catMaybes fields)
  return fieldParser

unknownFieldsParserP :: P.Member Env r => P.Sem r (Parser (Maybe DsmrField))
unknownFieldsParserP = do
  valueParensParser <- valueParensParserP (pure $ pure ())
  return $ (string       "0-0:96.13.1" >> valueParensParser >> eol >> pure Nothing)
       <|> (string       "0-0:96.13.0" >> valueParensParser >> eol >> pure Nothing)

{-
TODO: Use something like this?

pScheme :: Parser Scheme
pScheme = choice
  [ SchemeData   <$ string "data"
  , SchemeFile   <$ string "file"
  , SchemeFtp    <$ string "ftp"
  , SchemeHttp   <$ string "http"
  , SchemeHttps  <$ string "https"
  , SchemeIrc    <$ string "irc"
  , SchemeMailto <$ string "mailto" ]
-}

dsmrFieldParserP :: P.Member Env r => P.Sem r (Parser (Maybe DsmrField))
dsmrFieldParserP = do
  fieldParserVersionNumber                 <- fieldParserP "1-3:0.2.8"       valueIntegerParserP     VersionNumber
  fieldParserTimeStamp                     <- fieldParserP "0-0:1.0.0"       valueTimestampParserP   TimeStamp
  fieldParserEquipmentID                   <- fieldParserP "0-0:96.1.1"      valueStringParserP      EquipmentID
  fieldParserEnergyConsumedTariff1         <- fieldParserP "1-0:1.8.1"       valueEnergyParserP      EnergyConsumedTariff1
  fieldParserEnergyConsumedTariff2         <- fieldParserP "1-0:2.8.1"       valueEnergyParserP      EnergyConsumedTariff2
  fieldParserEnergyReturnedTariff1         <- fieldParserP "1-0:1.8.2"       valueEnergyParserP      EnergyReturnedTariff1
  fieldParserEnergyReturnedTariff2         <- fieldParserP "1-0:2.8.2"       valueEnergyParserP      EnergyReturnedTariff2
  fieldParserActualTariffIndicator         <- fieldParserP "0-0:96.14.0"     valueIntParserP         ActualTariffIndicator
  fieldParserActualPowerConsumption        <- fieldParserP "1-0:1.7.0"       valuePowerParserP       ActualPowerConsumption
  fieldParserActualPowerReturned           <- fieldParserP "1-0:2.7.0"       valuePowerParserP       ActualPowerReturned
  fieldParserNumberOfPowerFailures         <- fieldParserP "0-0:96.7.21"     valueIntegerParserP     NumberOfPowerFailures
  fieldParserNumberOfLongPowerFailures     <- fieldParserP "0-0:96.7.9"      valueIntegerParserP     NumberOfLongPowerFailures
  fieldParserPowerFailureLog               <-                                valueFailureLogParserP  PowerFailureLog
  fieldParserNumberOfVoltageSagsPhaseL1    <- fieldParserP "1-0:32.32.0"     valueIntegerParserP     NumberOfVoltageSagsPhaseL1
  fieldParserNumberOfVoltageSagsPhaseL2    <- fieldParserP "1-0:32.36.0"     valueIntegerParserP     NumberOfVoltageSagsPhaseL2
  fieldParserActualCurrentConsumption      <- fieldParserP "1-0:31.7.0"      valueCurrentParserP     ActualCurrentConsumption
  fieldParserActualPowerConsumptionPhaseL1 <- fieldParserP "1-0:21.7.0"      valuePowerParserP       ActualPowerConsumptionPhaseL1
  fieldParserActualPowerReturnedPhaseL1    <- fieldParserP "1-0:22.7.0"      valuePowerParserP       ActualPowerReturnedPhaseL1
  fieldParserSlaveGasMeterDeviceType       <- fieldParserP "0-1:24.1.0"      valueIntParserP         SlaveGasMeterDeviceType
  fieldParserGasMeterSerialNumber          <- fieldParserP "0-1:96.1.0"      valueStringParserP      GasMeterSerialNumber
  fieldParserGasConsumption                <-                                valueGasParserP         GasConsumption
  unknownFieldParser                       <- unknownFieldsParserP
  return $
        fieldParserVersionNumber
    <|> fieldParserTimeStamp
    <|> fieldParserEquipmentID
    <|> fieldParserEnergyConsumedTariff1
    <|> fieldParserEnergyConsumedTariff2
    <|> fieldParserEnergyReturnedTariff1
    <|> fieldParserEnergyReturnedTariff2
    <|> fieldParserActualTariffIndicator
    <|> fieldParserActualPowerConsumption
    <|> fieldParserActualPowerReturned
    <|> fieldParserNumberOfPowerFailures
    <|> fieldParserNumberOfLongPowerFailures
    <|> fieldParserPowerFailureLog
    <|> fieldParserNumberOfVoltageSagsPhaseL1
    <|> fieldParserNumberOfVoltageSagsPhaseL2
    <|> fieldParserActualCurrentConsumption
    <|> fieldParserActualPowerConsumptionPhaseL1
    <|> fieldParserActualPowerReturnedPhaseL1
    <|> fieldParserSlaveGasMeterDeviceType
    <|> fieldParserGasMeterSerialNumber
    <|> fieldParserGasConsumption
    <|> unknownFieldParser

fieldParserP :: P.Member Env r =>  String -> P.Sem r (Parser a) -> (a -> DsmrField) -> P.Sem r (Parser (Maybe DsmrField))
fieldParserP dsmrFieldID valueTypeParser fieldConstructor = do
  valueParensParser <- valueParensParserP valueTypeParser
  return $ do
    _ <- string dsmrFieldID
    val <- valueParensParser 
    _ <- eol
    return . Just $ fieldConstructor val

valueParensParserP :: P.Member Env r => P.Sem r (Parser a) -> P.Sem r (Parser a)
valueParensParserP valParserP = do
    valParser <- valParserP
    return $ do
      _ <- char '('
      val <- valParser
      _ <- char ')'
      return val

valueFailureLogParserP :: P.Member Env r => ([(UTCTime, Integer)] -> DsmrField) -> P.Sem r (Parser (Maybe DsmrField))
valueFailureLogParserP failureLogContructor = do
  logEntryParser <- logEntryParserP
  valueParensInt <- valueParensParserP valueIntParserP
  valueParentId <- valueParensParserP . pure $ (string "0-0:96.7.19")
  return $ do
    _ <- string "1-0:99.97.0"
    numLogEntries <- valueParensInt
    _ <- valueParentId
    logEntries <- replicateM numLogEntries logEntryParser
    _ <- eol
    return . Just $ failureLogContructor logEntries

logEntryParserP :: P.Member Env r => P.Sem r (Parser (UTCTime, Integer))
logEntryParserP = do
  timeStampStrP <- valueParensParserP valueTimestampParserP
  durationSecondsP <- valueParensParserP valueSecondsParserP
  return $ do
    timeStampStr <- timeStampStrP
    durationSeconds <- durationSecondsP
    return (timeStampStr, durationSeconds)

valueGasParserP :: P.Member Env r => ((UTCTime, Double) -> DsmrField) -> P.Sem r (Parser (Maybe DsmrField))
valueGasParserP gasContructor = do
  timeStampParser <- valueParensParserP valueTimestampParserP
  valueVolumeParser <- valueParensParserP valueVolumeParserP
  return $ do
    _ <- string "0-1:24.2.1"
    _timeStamp <- timeStampParser
    volume <- valueVolumeParser
    _ <- eol
    return . Just $ gasContructor (_timeStamp, volume)

valueVolumeParserP :: P.Member Env r => P.Sem r (Parser Double)
valueVolumeParserP = return $ do
  value <- float
  _ <- string "*m3"
  return value

valueEnergyParserP :: P.Member Env r => P.Sem r (Parser Double)
valueEnergyParserP = return $ do
  value <- float
  _ <- string "*kWh"
  return value

valueCurrentParserP :: P.Member Env r => P.Sem r (Parser Integer)
valueCurrentParserP = return $ do
  value <- decimal
  _ <- string "*A"
  return value

valuePowerParserP :: P.Member Env r => P.Sem r (Parser Double)
valuePowerParserP = return $ do
  value <- float
  _ <- string "*kW"
  return value

valueSecondsParserP :: P.Member Env r => P.Sem r (Parser Integer)
valueSecondsParserP = do
  valueIntegerParser <- valueIntegerParserP
  return $ do
    value <- valueIntegerParser
    _ <- string "*s"
    return value

valueIntegerParserP :: P.Member Env r => P.Sem r (Parser Integer)
valueIntegerParserP = return $ do
  val <- some digitChar
  return (read val :: Integer)

valueIntParserP :: P.Member Env r => P.Sem r (Parser Int)
valueIntParserP = return $ fromInteger <$> decimal

valueStringParserP :: P.Member Env r => P.Sem r (Parser String)
valueStringParserP = return $ many alphaNumChar

valueTimestampParserP :: P.Member Env r => P.Sem r (Parser UTCTime)
valueTimestampParserP = do
  timeZone <- getEnvironmentTimeZone
  century <- getEnvironmentTimeCentury
  return $ do
    timeStr <- (++) <$> count 12 digitChar <*> (string "S" <|> string "W")
    return $ fromJust $ localTimeStampToUTC timeZone century timeStr

runDsmrParser :: P.Members '[Env, P.Output String] r => String -> P.Sem r (Maybe DsmrTelegram)
runDsmrParser input = do
  parser <- dsmrTelegramParserP
  let parseResult = parse parser "" input
  case parseResult of
          Left parseErrorBundle -> do
            P.output (errorBundlePretty parseErrorBundle)
            return Nothing
          Right dsmrTelegram -> return $ Just dsmrTelegram

readMetrics :: P.Members '[Env, DsmrTelegramReader, P.Output String] r => (Maybe DsmrTelegram -> P.Sem r a) -> P.Sem r a
readMetrics callback =
    do
        telegram <- readTelegram
        parseResult <- runDsmrParser telegram
        callback parseResult
