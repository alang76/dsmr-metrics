module TelegramBuilder where

import DsmrMetricsReader.Model
import Data.Time.Clock (UTCTime(..))
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.LocalTime (utcToLocalTime, utcToZonedTime, ZonedTime(..), TimeZone(..))
import TestEnv
import Polysemy as P

buildTelegram :: MeterID -> Integer -> UTCTime -> String -> Double -> Double -> Double -> Double ->
                 Int -> Double -> Double -> Integer -> Integer -> [(UTCTime, Integer)] -> Integer -> Integer -> Integer ->
                 Double -> Double -> Int -> String -> (UTCTime , Double) -> Checksum -> DsmrTelegram
buildTelegram
  meterID
  versionNumber
  timeStamp
  equipmentID
  energyConsumedTariff1
  energyConsumedTariff2
  energyReturnedTariff1
  energyReturnedTariff2
  actualTariffIndicator
  actualPowerConsumption
  actualPowerReturned
  numberOfPowerFailures
  numberOfLongPowerFailures
  powerFailureLog
  numberOfVoltageSagsPhaseL1
  numberOfVoltageSagsPhaseL2
  actualCurrentConsumption
  actualPowerConsumptionPhaseL1
  actualPowerReturnedPhaseL1
  slaveGasMeterDeviceType
  gasMeterSerialNumber
  gasConsumption
  checkSum = 
    let fields = [ VersionNumber                 versionNumber,
                   TimeStamp                     timeStamp,
                   EquipmentID                   equipmentID,
                   EnergyConsumedTariff1         energyConsumedTariff1,
                   EnergyConsumedTariff2         energyConsumedTariff2,
                   EnergyReturnedTariff1         energyReturnedTariff1,
                   EnergyReturnedTariff2         energyReturnedTariff2,
                   ActualTariffIndicator         actualTariffIndicator,
                   ActualPowerConsumption        actualPowerConsumption,
                   ActualPowerReturned           actualPowerReturned,
                   NumberOfPowerFailures         numberOfPowerFailures,
                   NumberOfLongPowerFailures     numberOfLongPowerFailures,
                   PowerFailureLog               powerFailureLog,
                   NumberOfVoltageSagsPhaseL1    numberOfVoltageSagsPhaseL1,
                   NumberOfVoltageSagsPhaseL2    numberOfVoltageSagsPhaseL2,
                   ActualCurrentConsumption      actualCurrentConsumption,
                   ActualPowerConsumptionPhaseL1 actualPowerConsumptionPhaseL1,
                   ActualPowerReturnedPhaseL1    actualPowerReturnedPhaseL1,
                   SlaveGasMeterDeviceType       slaveGasMeterDeviceType,
                   GasMeterSerialNumber          gasMeterSerialNumber,
                   GasConsumption                gasConsumption]
    in DsmrTelegram meterID fields checkSum

serializeTelegram :: P.Member TestEnv r => DsmrTelegram -> P.Sem r String
serializeTelegram (DsmrTelegram meterID fields checkSum) = do
  serializedFields <- concat <$> mapM serializeField fields
  return ("/" ++ meterID ++ "\n" ++ serializedFields ++ "!" ++ show checkSum ++ "\n")


-- TODO DRYify field ID mapping, cleanup on isle 4
serializeField :: P.Member TestEnv r => DsmrField -> P.Sem r String
serializeField field =
  let
    showTimeStamp :: P.Member TestEnv r => UTCTime -> P.Sem r String
    showTimeStamp utcTimeStamp = do
      envTimeZone <- getEnvironmentTimeZone
      let 
        zonedTime =  utcToZonedTime  envTimeZone utcTimeStamp
        localTime = zonedTimeToLocalTime zonedTime
        timeZone = zonedTimeZone zonedTime
        dstPostfix = if timeZoneSummerOnly timeZone then "S" else "W"
      return $ formatTime defaultTimeLocale "%y%m%d%H%M%S" localTime ++ dstPostfix
    showPowerFailureLog :: P.Member TestEnv r => [(UTCTime, Integer)] -> P.Sem r String
    showPowerFailureLog log = do
      logEntries <- concat <$> mapM showLogEntry log
      return $ show (length log) ++ ")" ++ "(0-0:96.7.19)" ++ logEntries
    showLogEntry (entryTimeStamp, entryDuration) = do
      timeStampStr <- showTimeStamp entryTimeStamp
      return $ "(" ++ timeStampStr ++ ")(" ++ show entryDuration ++ "*s)"
  in
    case field of
      VersionNumber                 versionNumber ->                 pure $ "1-3:0.2.8("   ++ show versionNumber ++ ")\n"
      TimeStamp                     timeStamp -> do
        timeStampStr <- showTimeStamp timeStamp
        return $ "0-0:1.0.0("++ timeStampStr ++ ")\n"
      EquipmentID                   equipmentID ->                   pure $ "0-0:96.1.1("  ++ equipmentID ++ ")\n"
      EnergyConsumedTariff1         energyConsumedTariff1 ->         pure $ "1-0:1.8.1("   ++ show energyConsumedTariff1 ++ "*kWh)\n"
      EnergyConsumedTariff2         energyConsumedTariff2 ->         pure $ "1-0:2.8.1("   ++ show energyConsumedTariff2 ++ "*kWh)\n"
      EnergyReturnedTariff1         energyReturnedTariff1 ->         pure $ "1-0:1.8.2("   ++ show energyReturnedTariff1 ++ "*kWh)\n"
      EnergyReturnedTariff2         energyReturnedTariff2 ->         pure $ "1-0:2.8.2("   ++ show energyReturnedTariff2 ++ "*kWh)\n"
      ActualTariffIndicator         actualTariffIndicator ->         pure $ "0-0:96.14.0(" ++ show actualTariffIndicator ++ ")\n"
      ActualPowerConsumption        actualPowerConsumption ->        pure $ "1-0:1.7.0("   ++ show actualPowerConsumption ++ "*kW)\n"
      ActualPowerReturned           actualPowerReturned ->           pure $ "1-0:2.7.0("   ++ show actualPowerReturned ++ "*kW)\n"
      NumberOfPowerFailures         numberOfPowerFailures ->         pure $ "0-0:96.7.21(" ++ show numberOfPowerFailures ++ ")\n"
      NumberOfLongPowerFailures     numberOfLongPowerFailures ->     pure $ "0-0:96.7.9("  ++ show numberOfLongPowerFailures ++ ")\n"
      PowerFailureLog               powerFailureLog -> do
        logStr <- showPowerFailureLog powerFailureLog
        return $ "1-0:99.97.0(" ++ logStr ++ "\n"
      NumberOfVoltageSagsPhaseL1    numberOfVoltageSagsPhaseL1 ->    pure $ "1-0:32.32.0(" ++ show numberOfVoltageSagsPhaseL1 ++ ")\n"
      NumberOfVoltageSagsPhaseL2    numberOfVoltageSagsPhaseL2 ->    pure $ "1-0:32.36.0(" ++ show numberOfVoltageSagsPhaseL2 ++ ")\n"
      ActualCurrentConsumption      actualCurrentConsumption ->      pure $ "1-0:31.7.0("  ++ show actualCurrentConsumption ++ "*A)\n"
      ActualPowerConsumptionPhaseL1 actualPowerConsumptionPhaseL1 -> pure $ "1-0:21.7.0("  ++ show actualPowerConsumptionPhaseL1 ++ "*kW)\n"
      ActualPowerReturnedPhaseL1    actualPowerReturnedPhaseL1 ->    pure $ "1-0:22.7.0("  ++ show actualPowerReturnedPhaseL1 ++ "*kW)\n"
      SlaveGasMeterDeviceType       slaveGasMeterDeviceType ->       pure $ "0-1:24.1.0("  ++ show slaveGasMeterDeviceType ++ ")\n"
      GasMeterSerialNumber          gasMeterSerialNumber ->          pure $ "0-1:96.1.0("  ++ gasMeterSerialNumber ++ ")\n"
      GasConsumption                (timeStampConsumption, volumeConsumption) -> do
        timeStampStr <- showTimeStamp timeStampConsumption
        pure $ "0-1:24.2.1("  ++ timeStampStr ++ ")(" ++ show volumeConsumption ++ "*m3)\n"
