module Main where

import Data.Function ((&))
import qualified Effects.AsyncEffect as AE
import qualified Effects.DsmrTelegramReader as DTR
import qualified Polysemy as P
import qualified Polysemy.Output as P

import DsmrMetricsReader (DsmrTelegram(..), DsmrField(..), readMetrics)
import PrometheusMetricsServer (
    MetricsWebServer, 
    runMetricsServer, 
    runWebServerIO, 
    UpdatePrometheusMetric,
    runUpdatePrometheusMetricsIO,
    updateEnergyConsumedTariff1,
    updateEnergyConsumedTariff2,
    updateEnergyReturnedTariff1,
    updateEnergyReturnedTariff2,
    updateActualTariffIndicator,
    updateActualPowerConsumption,
    updateActualPowerReturned,
    updateNumberOfPowerFailures,
    updateNumberOfPowerLongFailures,
    updateActualCurrentConsumption,
    updateGasConsumption,
    updateNothing
    )

processCallbackUpdatePrometheusMetrics :: P.Member UpdatePrometheusMetric r => DsmrTelegram -> P.Sem r ()
processCallbackUpdatePrometheusMetrics (DsmrTelegram _ fields _) = mapM_ fieldToMetric fields
    where
      fieldToMetric ::  P.Member UpdatePrometheusMetric r => DsmrField -> P.Sem r ()
      fieldToMetric (VersionNumber _)                                                 = updateNothing
      fieldToMetric (TimeStamp _)                                                     = updateNothing
      fieldToMetric (EquipmentID _)                                                   = updateNothing
      fieldToMetric (EnergyConsumedTariff1 energyConsumedTarrif1)                     = updateEnergyConsumedTariff1 energyConsumedTarrif1
      fieldToMetric (EnergyConsumedTariff2 energyConsumedTarrif2)                     = updateEnergyConsumedTariff2 energyConsumedTarrif2
      fieldToMetric (EnergyReturnedTariff1 energyReturnedTarrif1)                     = updateEnergyReturnedTariff1 energyReturnedTarrif1
      fieldToMetric (EnergyReturnedTariff2 energyReturnedTarrif2)                     = updateEnergyReturnedTariff2 energyReturnedTarrif2
      fieldToMetric (ActualTariffIndicator actualTariffIndicator)                     = updateActualTariffIndicator actualTariffIndicator
      fieldToMetric (ActualPowerConsumption actualPowerConsumption)                   = updateActualPowerConsumption actualPowerConsumption
      fieldToMetric (ActualPowerReturned actualPowerReturned)                         = updateActualPowerReturned actualPowerReturned
      fieldToMetric (NumberOfPowerFailures numbnerOfPowerFailures)                    = updateNumberOfPowerFailures numbnerOfPowerFailures
      fieldToMetric (NumberOfLongPowerFailures numberOfLongPowerFailures)             = updateNumberOfPowerLongFailures numberOfLongPowerFailures
      fieldToMetric (PowerFailureLog _)                                               = updateNothing
      fieldToMetric (NumberOfVoltageSagsPhaseL1 _)                                    = updateNothing
      fieldToMetric (NumberOfVoltageSagsPhaseL2 _)                                    = updateNothing
      fieldToMetric (ActualCurrentConsumption actualCurrentConsumption)               = updateActualCurrentConsumption actualCurrentConsumption
      fieldToMetric (ActualPowerConsumptionPhaseL1 _)                                 = updateNothing
      fieldToMetric (ActualPowerReturnedPhaseL1 _)                                    = updateNothing
      fieldToMetric (SlaveGasMeterDeviceType _)                                       = updateNothing
      fieldToMetric (GasMeterSerialNumber _)                                          = updateNothing
      fieldToMetric (GasConsumption (gasConsumptionTimeStamp,gasConsumptionVolume))   = updateGasConsumption gasConsumptionTimeStamp gasConsumptionVolume

runApp :: P.Members '[AE.Async, MetricsWebServer, P.Output String, DTR.DsmrTelegramReader, UpdatePrometheusMetric] r => P.Sem r ()
runApp = do
  P.output "Starting metrics serving thread"
  metricsThread <- AE.async runMetricsServer
  P.output "Starting DSMR reading thread"
  readDsmrThread <- AE.async $ readMetrics processCallbackUpdatePrometheusMetrics
  (_, runCoreResult) <- AE.awaitAny [metricsThread, readDsmrThread]
  P.output "Program terminated."
  P.output ("Core result: " ++ show runCoreResult)
-- kill all threads when one of the main threads ended
  AE.cancel metricsThread
  --AE.cancel readDsmrThread

main :: IO ()
main =
  runApp
    & runWebServerIO
    & P.runOutputSem (P.embed . putStrLn)
    & DTR.runTelegramReaderFakeIO
    & runUpdatePrometheusMetricsIO
    & AE.asyncToIO
    & P.runM
