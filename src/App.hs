module App (
    runApp,
    runOutputIO)
where

import Prelude hiding (log)
import Data.Hashable(hash)
import Control.Monad (forever) -- replicateM_
import Data.Function((&))
import Effects.DsmrTelegramReader(DsmrTelegramReader(..), readTelegram)
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
import Events.DsmrMetricEvent(DsmrMetricEvent(..))
import Exceptions.DsmrMetricException(DsmrMetricException(..))
import System.Mem(performMajorGC)

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

runOutputIO :: P.Member (P.Embed IO) r => P.Sem (P.Output DsmrMetricEvent:r) a -> P.Sem r a
runOutputIO = P.interpret $  \case
  P.Output event -> P.embed . putStrLn $ show event
{-# INLINE runOutputIO #-}

readAndParseTelegram :: P.Members '[Env, DsmrTelegramReader, P.Output DsmrMetricEvent, P.Embed IO] r => (Maybe DsmrTelegram -> P.Sem r a) -> P.Sem r a
readAndParseTelegram callback = do
  telegram <- readTelegram
  parseResult <- runDsmrParser telegram
  res <- callback parseResult
  P.embed performMajorGC
  return res

runApp :: P.Members '[
    Async
  , ServeMetrics
  , P.Output DsmrMetricEvent
  , DsmrTelegramReader
  , UpdatePrometheusMetric
  , P.Error DsmrMetricException
  , Env
  , P.Embed IO] r => P.Sem r ()
runApp = do
  P.output ProgramStarted
  metricsThread <- async serveMetrics
  P.output $ MetricsServerThreadStarted (hash metricsThread)
  -- run the output effect per iteration, to prevent space leaking
  readDsmrThread <- async . forever . runOutputIO $ readAndParseTelegram processCallbackUpdatePrometheusMetrics
  --readDsmrThread <- async . replicateM_ 1000 $  readAndParseTelegram processCallbackUpdatePrometheusMetrics
  P.output $ DsmrTelegramReaderThreadStarted (hash readDsmrThread)
  (completedThreadId, _) <- (\(thread, res) -> (hash thread, res)) <$> awaitAny [metricsThread, readDsmrThread]
  P.output $ ThreadTerminated completedThreadId
  P.output ProgramTerminated 
  -- kill all threads when one of the main threads ended
  cancel metricsThread
  cancel readDsmrThread