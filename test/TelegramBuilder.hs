module TelegramBuilder (
    createTestTelegram,
    buildTelegram,
    serializeTelegram,
    serializeField,
    tg2
  ) where

import DsmrMetricsReader.Model
import Data.Time.Clock (UTCTime(..))
import Effects.Env
import Util.Time(utcToLocalTimeStamp, localTimeStampToUTC, mkUTCTime)

import qualified Polysemy as P


createTestTelegram :: P.Member Env r => P.Sem r DsmrTelegram
createTestTelegram = do
  timeZone <- getEnvironmentTimeZone
  let
    timeStampUtc = localTimeStampToUTC "200529163319S" timeZone
    log1TimeUtc = localTimeStampToUTC "170326062519S" timeZone
    log2TimeUtc = localTimeStampToUTC "160417043131S" timeZone
    gasTimeUtc = localTimeStampToUTC "200529160000S" timeZone
  pure $ buildTelegram 
      "XMX5LGBBFFB231215493"                              -- meter ID
      42                                                  -- version ID
      timeStampUtc                                        -- timestamp
      "4530303034303031353934373534343134"                -- equipment ID
      14765.683                                           -- energyConsumedTariff1
      0                                                   -- energyConsumedTariff2
      14628.043                                           -- energyReturnedTariff1
      0                                                   -- energyReturnedTariff2
      2                                                   -- actualTariffIndicator
      3.877                                               -- actualPowerConsumption        
      0                                                   -- actualPowerReturned           
      3                                                   -- numberOfPowerFailures         
      2                                                   -- numberOfLongPowerFailures     
      [(log1TimeUtc, 29642045), (log2TimeUtc, 32998738)]  -- powerFailureLog               
      0                                                   -- numberOfVoltageSagsPhaseL1    
      0                                                   -- numberOfVoltageSagsPhaseL2    
      17                                                  -- actualCurrentConsumption      
      3.877                                               -- actualPowerConsumptionPhaseL1 
      0                                                   -- actualPowerReturnedPhaseL1    
      3                                                   -- slaveGasMeterDeviceType       
      "4730303137353931323139313130333134"                -- gasMeterSerialNumber          
      (gasTimeUtc, 5277.053)                              -- gasConsumption 
      7667                                                -- checkSum 


tg2 :: DsmrTelegram
tg2 = 
    DsmrTelegram
      { _meterID = ""
      , _dsmrFields =
          [ VersionNumber { _versionNumber = 0 }
          , TimeStamp { _timeStamp = mkUTCTime (1900, 01, 01) (0, 0, 0) } 
          , EquipmentID { _equipmentID = "" }
          , EnergyConsumedTariff1 { _energyConsumedTariff1 = 0.0 }
          , EnergyConsumedTariff2 { _energyConsumedTariff2 = 0.0 }
          , EnergyReturnedTariff1 { _energyReturnedTariff1 = 0.0 }
          , EnergyReturnedTariff2 { _energyReturnedTariff2 = 0.0 }
          , ActualTariffIndicator { _actualTariffIndicator = 0 }
          , ActualPowerConsumption { _actualPowerConsumption = 0.0 }
          , ActualPowerReturned { _actualPowerReturned = 0.0 }
          , NumberOfPowerFailures { _numberOfPowerFailures = 0 }
          , NumberOfLongPowerFailures { _numberOfLongPowerFailures = 0 }
          , PowerFailureLog { _powerFailureLog = [] }
          , NumberOfVoltageSagsPhaseL1 { _numberOfVoltageSagsPhaseL1 = 0 }
          , NumberOfVoltageSagsPhaseL2 { _numberOfVoltageSagsPhaseL2 = 0 }
          , ActualCurrentConsumption { _actualCurrentConsumption = 0 }
          , ActualPowerConsumptionPhaseL1
              { _actualPowerConsumptionPhaseL1 = 0.0 }
          , ActualPowerReturnedPhaseL1 { _actualPowerReturnedPhaseL1 = 0.0 }
          , SlaveGasMeterDeviceType { _slaveGasMeterDeviceType = 0 }
          , GasMeterSerialNumber { _gasMeterSerialNumber = "" }
          , GasConsumption
              { _gasConsumption = ( mkUTCTime (1900, 01, 01) (0, 0, 0) , 0.0 ) }
          ]
      , _checkSum = 0
      }

buildTelegram :: MeterID -> Integer -> UTCTime -> String -> Double -> Double -> Double -> Double ->
                 Int -> Double -> Double -> Integer -> Integer -> [(UTCTime, Integer)] -> Integer -> Integer -> Integer ->
                 Double -> Double -> Int -> String -> (UTCTime , Double) -> Checksum -> DsmrTelegram
buildTelegram
  _meterID                          -- MeterID
  _versionNumber                    -- Integer
  _timeStamp                        -- UTCTime
  _equipmentID                      -- String
  _energyConsumedTariff1            -- Double
  _energyConsumedTariff2            -- Double
  _energyReturnedTariff1            -- Double
  _energyReturnedTariff2            -- Double
  _actualTariffIndicator            -- Int
  _actualPowerConsumption           -- Double
  _actualPowerReturned              -- Double
  _numberOfPowerFailures            -- Integer
  _numberOfLongPowerFailures        -- Integer
  _powerFailureLog                  -- [(UTCTime, Integer)]
  _numberOfVoltageSagsPhaseL1       -- Integer
  _numberOfVoltageSagsPhaseL2       -- Integer
  _actualCurrentConsumption         -- Integer
  _actualPowerConsumptionPhaseL1    -- Double
  _actualPowerReturnedPhaseL1       -- Double
  _slaveGasMeterDeviceType          -- Int
  _gasMeterSerialNumber             -- String
  _gasConsumption                   -- (UTCTime , Double)
  _checkSum =                       -- Checksum
    let fields = [ VersionNumber                 _versionNumber,
                   TimeStamp                     _timeStamp,
                   EquipmentID                   _equipmentID,
                   EnergyConsumedTariff1         _energyConsumedTariff1,
                   EnergyConsumedTariff2         _energyConsumedTariff2,
                   EnergyReturnedTariff1         _energyReturnedTariff1,
                   EnergyReturnedTariff2         _energyReturnedTariff2,
                   ActualTariffIndicator         _actualTariffIndicator,
                   ActualPowerConsumption        _actualPowerConsumption,
                   ActualPowerReturned           _actualPowerReturned,
                   NumberOfPowerFailures         _numberOfPowerFailures,
                   NumberOfLongPowerFailures     _numberOfLongPowerFailures,
                   PowerFailureLog               _powerFailureLog,
                   NumberOfVoltageSagsPhaseL1    _numberOfVoltageSagsPhaseL1,
                   NumberOfVoltageSagsPhaseL2    _numberOfVoltageSagsPhaseL2,
                   ActualCurrentConsumption      _actualCurrentConsumption,
                   ActualPowerConsumptionPhaseL1 _actualPowerConsumptionPhaseL1,
                   ActualPowerReturnedPhaseL1    _actualPowerReturnedPhaseL1,
                   SlaveGasMeterDeviceType       _slaveGasMeterDeviceType,
                   GasMeterSerialNumber          _gasMeterSerialNumber,
                   GasConsumption                _gasConsumption]
    in DsmrTelegram _meterID fields _checkSum

serializeTelegram :: P.Member Env r => DsmrTelegram -> P.Sem r String
serializeTelegram (DsmrTelegram _meterID fields _checkSum) = do
  serializedFields <- concat <$> mapM serializeField fields
  return ("/" ++ _meterID ++ "\n" ++ serializedFields ++ "!" ++ show _checkSum ++ "\n")


-- TODO DRYify field ID mapping, cleanup on isle 4
serializeField :: P.Member Env r => DsmrField -> P.Sem r String
serializeField field = do
  timeZone <- getEnvironmentTimeZone
  let    
    showPowerFailureLog :: P.Member Env r => [(UTCTime, Integer)] -> P.Sem r String
    showPowerFailureLog _log = do
      logEntries <- concat <$> mapM showLogEntry _log
      return $ show (length _log) ++ ")" ++ "(0-0:96.7.19)" ++ logEntries
    showLogEntry :: (UTCTime, Integer) -> P.Sem r String
    showLogEntry (entryTimeStamp, entryDuration) = do
      let timeStampStr = utcToLocalTimeStamp entryTimeStamp timeZone
      return $ "(" ++ timeStampStr ++ ")(" ++ show entryDuration ++ "*s)"
  case field of
    VersionNumber                 _versionNumber ->                 pure $ "1-3:0.2.8("   ++ show _versionNumber ++ ")\n"
    TimeStamp                     _timeStamp ->                     pure $ "0-0:1.0.0("   ++ utcToLocalTimeStamp _timeStamp timeZone ++ ")\n"
    EquipmentID                   _equipmentID ->                   pure $ "0-0:96.1.1("  ++ _equipmentID ++ ")\n"
    EnergyConsumedTariff1         _energyConsumedTariff1 ->         pure $ "1-0:1.8.1("   ++ show _energyConsumedTariff1 ++ "*kWh)\n"
    EnergyConsumedTariff2         _energyConsumedTariff2 ->         pure $ "1-0:2.8.1("   ++ show _energyConsumedTariff2 ++ "*kWh)\n"
    EnergyReturnedTariff1         _energyReturnedTariff1 ->         pure $ "1-0:1.8.2("   ++ show _energyReturnedTariff1 ++ "*kWh)\n"
    EnergyReturnedTariff2         _energyReturnedTariff2 ->         pure $ "1-0:2.8.2("   ++ show _energyReturnedTariff2 ++ "*kWh)\n"
    ActualTariffIndicator         _actualTariffIndicator ->         pure $ "0-0:96.14.0(" ++ show _actualTariffIndicator ++ ")\n"
    ActualPowerConsumption        _actualPowerConsumption ->        pure $ "1-0:1.7.0("   ++ show _actualPowerConsumption ++ "*kW)\n"
    ActualPowerReturned           _actualPowerReturned ->           pure $ "1-0:2.7.0("   ++ show _actualPowerReturned ++ "*kW)\n"
    NumberOfPowerFailures         _numberOfPowerFailures ->         pure $ "0-0:96.7.21(" ++ show _numberOfPowerFailures ++ ")\n"
    NumberOfLongPowerFailures     _numberOfLongPowerFailures ->     pure $ "0-0:96.7.9("  ++ show _numberOfLongPowerFailures ++ ")\n"
    PowerFailureLog               _powerFailureLog -> do
      logStr <- showPowerFailureLog _powerFailureLog
      return $ "1-0:99.97.0(" ++ logStr ++ "\n"
    NumberOfVoltageSagsPhaseL1    _numberOfVoltageSagsPhaseL1 ->    pure $ "1-0:32.32.0(" ++ show _numberOfVoltageSagsPhaseL1 ++ ")\n"
    NumberOfVoltageSagsPhaseL2    _numberOfVoltageSagsPhaseL2 ->    pure $ "1-0:32.36.0(" ++ show _numberOfVoltageSagsPhaseL2 ++ ")\n"
    ActualCurrentConsumption      _actualCurrentConsumption ->      pure $ "1-0:31.7.0("  ++ show _actualCurrentConsumption ++ "*A)\n"
    ActualPowerConsumptionPhaseL1 _actualPowerConsumptionPhaseL1 -> pure $ "1-0:21.7.0("  ++ show _actualPowerConsumptionPhaseL1 ++ "*kW)\n"
    ActualPowerReturnedPhaseL1    _actualPowerReturnedPhaseL1 ->    pure $ "1-0:22.7.0("  ++ show _actualPowerReturnedPhaseL1 ++ "*kW)\n"
    SlaveGasMeterDeviceType       _slaveGasMeterDeviceType ->       pure $ "0-1:24.1.0("  ++ show _slaveGasMeterDeviceType ++ ")\n"
    GasMeterSerialNumber          _gasMeterSerialNumber ->          pure $ "0-1:96.1.0("  ++ _gasMeterSerialNumber ++ ")\n"
    GasConsumption                (timeStampConsumption, volumeConsumption) -> pure $ "0-1:24.2.1("  ++ utcToLocalTimeStamp timeStampConsumption timeZone ++ ")(" ++ show volumeConsumption ++ "*m3)\n"
