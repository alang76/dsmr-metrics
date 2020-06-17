{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}


module Main where

import qualified Control.Concurrent.Async as Async
import qualified Polysemy as P
import qualified Polysemy.Output as PO
import Data.Function ((&))
import qualified Effects.AsyncEffect as AE

import DsmrMetricsReader (DsmrTelegram(..), DsmrField(..), readMetrics, TelegramReader, readTelegram, CallbackEffect, processCallback)
import PrometheusMetricsServer (incCounter, pageVisits, WebServer(..), runMetricsServer, runWebServerIO, UpdatePrometheusMetrics(..), updatePrometheusGas)

processCallbackUpdatePrometheusMetrics
    :: Member (CallbackEffect) r
    => DsmrTelegram -> Sem (CallbackEffect ': r) a
    -> Sem (UpdatePrometheusMetrics ': r) a
processCallbackUpdatePrometheusMetrics (DsmrTelegram meterID fields checkSum) = reinterpret $ map fieldToMetric fields
  where
    fieldToMetric (VersionNumber num) = = undefined
    fieldToMetric (TimeStamp ts) = undefined
    fieldToMetric (EquipmentID eid) = undefined
    fieldToMetric (EnergyConsumedTariff1 nrg) = undefined
    fieldToMetric (EnergyConsumedTariff2 nrg) = undefined
    fieldToMetric (EnergyReturnedTariff1 nrg) = undefined
    fieldToMetric (EnergyReturnedTariff2 nrg)  = undefined
    fieldToMetric (ActualTariffIndicator nrg)  = undefined
    fieldToMetric (ActualPowerConsumption pwr) = undefined
    fieldToMetric (ActualPowerReturned pwr) = undefined
    fieldToMetric (NumberOfPowerFailures nf) = undefined
    fieldToMetric (NumberOfLongPowerFailures nlf) = undefined
    fieldToMetric (PowerFailureLog flog) = undefined
    fieldToMetric (NumberOfVoltageSagsPhaseL1 sags) = undefined
    fieldToMetric (NumberOfVoltageSagsPhaseL2 sags) = undefined
    fieldToMetric (ActualCurrentConsumption crnt) = undefined
    fieldToMetric (ActualPowerConsumptionPhaseL1 pwr) = undefined
    fieldToMetric (ActualPowerReturnedPhaseL1 pwr) = undefined
    fieldToMetric (SlaveGasMeterDeviceType dtype) = undefined
    fieldToMetric (GasMeterSerialNumber serial) = undefined
    fieldToMetric (GasConsumption (timeStamp, volume)) = updateMetricGas (timeStamp, volume)


runApp :: P.Members '[AE.Async, WebServer, PO.Output String, CallbackEffect, TelegramReader] r => P.Sem r ()
runApp = do
  metricsThread <- AE.async $ runMetricsServer
  --readDsmrThread <- AE.async $ readMetrics processMetricsCallback
  (_, runCoreResult) <- AE.awaitAny [metricsThread] --, readDsmrThread

  -- kill all threads when one of the main threads ended
  AE.cancel metricsThread
  --AE.cancel readDsmrThread

main :: IO ()
main = do
  runApp
    & AE.asyncToIO
    & runWebServerIO
    & PO.runOutputSem (P.embed . putStrLn)
    & P.runM