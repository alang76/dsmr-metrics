{-# LANGUAGE OverloadedStrings #-}

module PrometheusMetricsServer
    (   runMetricsServer,
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
        updateNothing,
        MetricsWebServer(..)
    ) where

import qualified Network.Wai.Handler.Warp as W
import qualified Network.Wai as W
import qualified Network.Wai.Middleware.Prometheus as PM
import qualified Prometheus as PM
import qualified Prometheus.Metric.GHC as PM
import Data.Time (UTCTime)
import Data.Text (Text, pack)
import Polysemy as P
import Polysemy.Output as P

metricVectorGasConsumption :: PM.Vector Text PM.Gauge
metricVectorGasConsumption = 
               PM.unsafeRegister
             $ PM.vector (pack "timestamp") 
             $ PM.gauge (PM.Info "GasConsumption" "The gas consumption in m3 at the time of timestamp")

metricGauge :: Text -> Text -> PM.Gauge
metricGauge gaugeId gaugeDescription = PM.unsafeRegister
            $ PM.gauge 
            $ PM.Info gaugeId gaugeDescription

data UpdatePrometheusMetric m a where
    UpdateEnergyConsumedTariff1 :: Double -> UpdatePrometheusMetric m ()
    UpdateEnergyConsumedTariff2 :: Double -> UpdatePrometheusMetric m ()
    UpdateEnergyReturnedTariff1 :: Double -> UpdatePrometheusMetric m ()
    UpdateEnergyReturnedTariff2 :: Double -> UpdatePrometheusMetric m ()
    UpdateActualTariffIndicator :: Int -> UpdatePrometheusMetric m ()
    UpdateActualPowerConsumption :: Double -> UpdatePrometheusMetric m ()
    UpdateActualPowerReturned :: Double -> UpdatePrometheusMetric m ()
    UpdateNumberOfPowerFailures :: Integer -> UpdatePrometheusMetric m ()
    UpdateNumberOfPowerLongFailures :: Integer -> UpdatePrometheusMetric m ()
    UpdateActualCurrentConsumption :: Integer -> UpdatePrometheusMetric m ()
    UpdateGasConsumption :: UTCTime -> Double -> UpdatePrometheusMetric m ()
    UpdateNothing :: UpdatePrometheusMetric m ()

P.makeSem ''UpdatePrometheusMetric

runUpdatePrometheusMetricsIO :: P.Member (P.Embed IO) r => P.Sem (UpdatePrometheusMetric ': r) a -> P.Sem r a
runUpdatePrometheusMetricsIO = 
    let
        interpretSetGauge :: P.Member (P.Embed IO) r => PM.Gauge -> Double -> P.Sem r ()
        interpretSetGauge gaugeMetric value = P.embed $ do
            let (_, ioAction) = PM.runMonitor $ PM.setGauge gaugeMetric value
            ioAction
            pure ()
        
        interpretSetVector :: P.Member (P.Embed IO) r => PM.Vector Text PM.Gauge -> Text -> (PM.Gauge -> IO()) -> P.Sem r ()
        interpretSetVector vector label metricUpdater = P.embed $ do
            let (_, ioAction) = PM.runMonitor $ PM.withLabel vector label metricUpdater
            ioAction
            pure ()
    in
        P.interpret $ \case
            UpdateEnergyConsumedTariff1 energyConsumedTariff1_-> interpretSetGauge (metricGauge "energyConsumedTariff1" "The energy consumed while tariff 1 was active in kWh") energyConsumedTariff1_
            UpdateEnergyConsumedTariff2 energyConsumedTariff2_ -> interpretSetGauge (metricGauge "energyConsumedTariff2" "The energy consumed while tariff 2 was active in kWh") energyConsumedTariff2_
            UpdateEnergyReturnedTariff1 energyReturnedTariff1_ -> interpretSetGauge (metricGauge "energyReturnedTariff1" "The energy returned while tariff 1 was active in kWh") energyReturnedTariff1_
            UpdateEnergyReturnedTariff2 energyReturnedTariff2_ -> interpretSetGauge (metricGauge "energyReturnedTariff2" "The energy returned while tariff 1 was active in kWh") energyReturnedTariff2_
            UpdateActualTariffIndicator energyActualTariffIndicator_ -> interpretSetGauge (metricGauge "energyActualTariffIndicator" "Indicates the currently active tariff") (fromIntegral energyActualTariffIndicator_)
            UpdateActualPowerConsumption updateActualPowerConsumption_ -> interpretSetGauge (metricGauge "updateActualPowerConsumption" "The actual power consumption in kW") updateActualPowerConsumption_
            UpdateActualPowerReturned updateActualPowerReturned_ -> interpretSetGauge (metricGauge "updateActualPowerReturned" "The actual power being returned in kW") updateActualPowerReturned_ 
            UpdateNumberOfPowerFailures updateNumberOfPowerFailures_ -> interpretSetGauge (metricGauge "updateNumberOfPowerFailures" "The number of power failures") (fromIntegral updateNumberOfPowerFailures_)
            UpdateNumberOfPowerLongFailures updateNumberOfPowerLongFailures_ -> interpretSetGauge (metricGauge "updateNumberOfPowerLongFailures" "The number of long power failures" ) (fromIntegral updateNumberOfPowerLongFailures_)
            UpdateActualCurrentConsumption updateActualCurrentConsumption_ -> interpretSetGauge (metricGauge "updateActualCurrentConsumption" "The actual current consumtion in A") (fromIntegral updateActualCurrentConsumption_)
            UpdateGasConsumption updateGasConsumptionTimeStamp updateGasConsumptionVolume -> interpretSetVector metricVectorGasConsumption (pack . show $ updateGasConsumptionTimeStamp) (`PM.setGauge` updateGasConsumptionVolume)
            UpdateNothing -> pure ()

data MetricsWebServer m a where
    RunServer ::  Int -> W.Application -> MetricsWebServer m ()

P.makeSem ''MetricsWebServer

runWebServerIO  :: P.Member (P.Embed IO) r => P.Sem (MetricsWebServer ': r) a -> P.Sem r a
runWebServerIO = P.interpret $ \case 
    (RunServer port app) -> P.embed $ do
        _ <- PM.register PM.ghcMetrics
        W.run port app

runMetricsServer :: Members '[MetricsWebServer, Output String] r => Sem r ()
runMetricsServer = do
    let port = 3000
    output $ "Listening at http://localhost:" ++ show port ++ "/"
    -- Register the GHC runtime metrics. For these to work, the app must be run
    -- with the `+RTS -T` command line options.

    -- Instrument the app with the prometheus middlware using the default
    -- `PrometheusSettings`. This will cause the app to dump the metrics when
    -- the /metrics endpoint is accessed.
    runServer port (PM.prometheus PM.def PM.metricsApp)