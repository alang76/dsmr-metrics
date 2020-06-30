module DsmrMetricsReader.Model where

import Data.Time (UTCTime(UTCTime))
import Control.Lens.TH (makeLenses, makePrisms)

data DsmrTelegram = DsmrTelegram {
  _meterID :: MeterID,
  _dsmrFields :: [DsmrField],
  _checkSum :: Checksum} -- ignored for now TODO
  deriving (Eq, Show)
type MeterID = String
type Checksum = Integer

data DsmrField =
      VersionNumber                 { _versionNumber                  :: Integer }
    | TimeStamp                     { _timeStamp                      :: UTCTime }
    | EquipmentID                   { _equipmentID                    :: String }
    | EnergyConsumedTariff1         { _energyConsumedTariff1          :: Double }               --kilo Watt hour
    | EnergyConsumedTariff2         { _energyConsumedTariff2          :: Double }               --kilo Watt hour
    | EnergyReturnedTariff1         { _energyReturnedTariff1          :: Double }               --kilo Watt hour
    | EnergyReturnedTariff2         { _energyReturnedTariff2          :: Double }               --kilo Watt hour
    | ActualTariffIndicator         { _actualTariffIndicator          :: Int }
    | ActualPowerConsumption        { _actualPowerConsumption         :: Double }               --kilo Watt
    | ActualPowerReturned           { _actualPowerReturned            :: Double }               --kilo Watt
    | NumberOfPowerFailures         { _numberOfPowerFailures          :: Integer }
    | NumberOfLongPowerFailures     { _numberOfLongPowerFailures      :: Integer }
    | PowerFailureLog               { _powerFailureLog                :: [(UTCTime, Integer)] } -- [(timestamp of failure end, duration in seconds)]
    | NumberOfVoltageSagsPhaseL1    { _numberOfVoltageSagsPhaseL1     :: Integer }
    | NumberOfVoltageSagsPhaseL2    { _numberOfVoltageSagsPhaseL2     :: Integer }
    | ActualCurrentConsumption      { _actualCurrentConsumption       :: Integer }              -- Amperes
    | ActualPowerConsumptionPhaseL1 { _actualPowerConsumptionPhaseL1  :: Double }               -- kilo Watt
    | ActualPowerReturnedPhaseL1    { _actualPowerReturnedPhaseL1     :: Double }               -- kilo Watt
    | SlaveGasMeterDeviceType       { _slaveGasMeterDeviceType        :: Int }
    | GasMeterSerialNumber          { _gasMeterSerialNumber           :: String }
    | GasConsumption                { _gasConsumption                 :: (UTCTime , Double) }   -- (timestamp, amount in m3)
    deriving (Eq, Show)

$(makeLenses ''DsmrField)
$(makePrisms ''DsmrField)
$(makeLenses ''DsmrTelegram)
