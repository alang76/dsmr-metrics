{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}


module PrometheusMetricsServer
    (   incCounter,
        pageVisits,
        runMetricsServer,
        runWebServerIO,
        UpdatePrometheusMetrics,
        updateMetricGasConsumption
        WebServer(..)
    ) where

import Network.Wai.Handler.Warp as W (run)
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType)

import Control.Monad.IO.Class (MonadIO)

import qualified Data.ByteString.Lazy as LBS
import qualified Network.Wai as Wai
import qualified Network.Wai.Middleware.Prometheus as P
import qualified Prometheus as P
import qualified Prometheus.Metric.GHC as P

import Polysemy
import Polysemy.Output


incCounter :: P.Counter -> IO ()
incCounter ctr = P.incCounter ctr 

pageVisits :: P.Counter
pageVisits = P.unsafeRegister
           $ P.counter
           -- Each metric provided by the base library takes an Info value that
           -- gives the name of the metric and a help string that describes the
           -- value that the metric represents.
           $ P.Info "page_visits" "The number of visits to the index page."


data UpdatePrometheusMetrics m a where
    UpdateMetricGasConsumption :: (UTCTime, Double) -> UpdatePrometheusMetric m ()
    -- todo, map other fields

makeSem ''UpdatePrometheusMetrics


data WebServer m a where
    RunServer ::  Int -> Wai.Application -> WebServer m ()

makeSem ''WebServer

runWebServerIO  :: Member (Embed IO) r => Sem (WebServer ': r) a -> Sem r a
runWebServerIO = interpret $ \case (RunServer port app) -> embed $ W.run port app

--data MetricsRegistry m a where
--    Register :: P.Metric s -> MetricsRegistry m ()

--makeSem ''MetricsRegistry

--registerPrometheus :: (MonadIO m, Member (Embed m) r) => Sem (MetricsRegistry ': r) a -> Sem r a
--registerPrometheus = interpret $ \case (Register metric) -> fixit $ P.register metric


runMetricsServer :: Members '[WebServer, Output String] r => Sem r ()
runMetricsServer = do
    let port = 3000
    output $ "Listening at http://localhost:" ++ show port ++ "/"
    -- Register the GHC runtime metrics. For these to work, the app must be run
    -- with the `+RTS -T` command line options.
 --   _ <- register P.ghcMetrics
    -- Instrument the app with the prometheus middlware using the default
    -- `PrometheusSettings`. This will cause the app to dump the metrics when
    -- the /metrics endpoint is accessed.
    runServer port (P.prometheus P.def P.metricsApp)

{-
runPrometheusMetricsServer :: IO ()
runPrometheusMetricsServer = do
    runMetricsServer & runWebServerIO
                     & runOutputSem (embed . putStrLn)
                     & runM
                               -- & registerPrometheus

-}