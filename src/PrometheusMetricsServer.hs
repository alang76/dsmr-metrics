{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module PrometheusMetricsServer
    (   incCounter,
        pageVisits,
        runMetricsServer
    ) where

import Network.Wai.Handler.Warp as W (run)
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType)

import qualified Data.ByteString.Lazy as LBS
import qualified Network.Wai as Wai
import qualified Network.Wai.Middleware.Prometheus as P
import qualified Prometheus as P
import qualified Prometheus.Metric.GHC as P

import Polysemy.Output
import Polysemy


incCounter :: P.Counter -> IO ()
incCounter ctr = P.incCounter ctr 

pageVisits :: P.Counter
pageVisits = P.register
           $ P.counter
           -- Each metric provided by the base library takes an Info value that
           -- gives the name of the metric and a help string that describes the
           -- value that the metric represents.
           $ P.Info "page_visits" "The number of visits to the index page."

data WebServer m a where
    RunServer ::  WebServer m ()

makeSem ''WebServer

runWebserverIO  :: Member (Embed IO) r => Sem (WebServer ': r) a
runWebserverIO port = interpret $ \case (RunServer) -> W.run port (P.prometheus P.def P.metricsApp)

data MetricsRegistry m a where
    Register :: P.Metric s -> MetricsRegistry m ()

makeSem ''MetricsRegistry

registerPrometheus :: Member (Embed IO) r => Sem (MetricsRegistry ': r) a
registerPrometheus = interpret $ \case (Register metric) -> P.register metric

runMetricsServer :: Members '[WebServer, MetricsRegistry, Output] m => Sem m ()
runMetricsServer = do
    let port = 3000
    output $ "Listening at http://localhost:" ++ show port ++ "/"
    -- Register the GHC runtime metrics. For these to work, the app must be run
    -- with the `+RTS -T` command line options.
    _ <- register P.ghcMetrics
    -- Instrument the app with the prometheus middlware using the default
    -- `PrometheusSettings`. This will cause the app to dump the metrics when
    -- the /metrics endpoint is accessed.
    runServer port (P.prometheus P.def P.metricsApp)

runPrometheusMetricsServer :: IO ()
runPrometheusMetricsServer = runMetricsServer
                                &  3000
                                & registerPrometheus
                                & runOutputSem putStrLn