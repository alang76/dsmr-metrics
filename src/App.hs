module App (
    runApp,
    runOutputAsLogAction)
where

import Prelude hiding (log)
import Data.Hashable(hash)
import Control.Monad (replicateM_) --forever
import Colog.Polysemy (Log, log)
import Effects.DsmrTelegramReader(DsmrTelegramReader(..))
import Effects.Async(Async(..), async,awaitAny,cancel)
import qualified Polysemy as P
import qualified Polysemy.Output as P
import qualified Polysemy.Error as P
import qualified Network.Wai.Middleware.Prometheus as PM
import Model.DsmrTelegram(DsmrTelegram(..), DsmrField(..))
import DsmrTelegramParser(runDsmrParser)
import Effects.ServeMetrics(ServeMetrics, serveMetrics,  runPrometheusMetricsServerIO)
import Effects.UpdatePrometheusMetric(
    UpdatePrometheusMetric,
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
import Effects.Env(Env, getConfiguration)
import Configuration(webServerConfig, listenPort)
import Effects.DsmrTelegramReader(readTelegram)
import Events.DsmrMetricEvent(DsmrMetricEvent(..))
import Exceptions.DsmrMetricException(DsmrMetricException(..))

processCallbackUpdatePrometheusMetrics :: P.Members '[P.Output DsmrMetricEvent, UpdatePrometheusMetric] r => Maybe DsmrTelegram -> P.Sem r ()
processCallbackUpdatePrometheusMetrics Nothing = pure ()
processCallbackUpdatePrometheusMetrics (Just (DsmrTelegram _ fields _)) = mapM_ fieldToMetric fields
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

runOutputAsLogAction :: P.Sem (P.Output DsmrMetricEvent:r) a -> P.Sem (Log String:r) a
runOutputAsLogAction = P.reinterpret $  \case
  P.Output event -> log @String (show event)
{-# INLINE runOutputAsLogAction #-}

readAndParseTelegram :: P.Members '[Env, DsmrTelegramReader, P.Output DsmrMetricEvent] r => (Maybe DsmrTelegram -> P.Sem r a) -> P.Sem r a
readAndParseTelegram callback = do
  telegram <- readTelegram
  parseResult <- runDsmrParser telegram
  callback parseResult

runApp :: P.Members '[
    Async
  , ServeMetrics
  , P.Output DsmrMetricEvent
  , DsmrTelegramReader
  , UpdatePrometheusMetric
  , P.Error DsmrMetricException
  , Env] r => P.Sem r ()
runApp = do
  P.output ProgramStarted
  metricsThread <- async serveMetrics
  P.output $ MetricsServerThreadStarted (hash metricsThread)
  --readDsmrThread <- async . forever $ readAndParseTelegram processCallbackUpdatePrometheusMetrics
  readDsmrThread <- async . (replicateM_ 10) $ readAndParseTelegram processCallbackUpdatePrometheusMetrics
  P.output $ DsmrTelegramReaderThreadStarted (hash readDsmrThread)
  (completedThreadId, _) <- fmap (\(thread, res) -> (hash $ thread, res)) $  awaitAny [metricsThread, readDsmrThread]
  P.output $ ThreadTerminated completedThreadId
  P.output ProgramTerminated 
  -- kill all threads when one of the main threads ended
  cancel metricsThread
  cancel readDsmrThread
