{-# LANGUAGE OverloadedStrings #-}

module PrometheusMetricsServer
    (   incCounter,
        pageVisits,
        runMetricsServer
    ) where

import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType)

import qualified Data.ByteString.Lazy as LBS
import qualified Network.Wai as Wai
import qualified Network.Wai.Middleware.Prometheus as P
import qualified Prometheus as P
import qualified Prometheus.Metric.GHC as P

incCounter :: P.Counter -> IO ()
incCounter ctr = P.incCounter ctr 

pageVisits :: P.Counter
pageVisits = P.unsafeRegister
           $ P.counter
           -- Each metric provided by the base library takes an Info value that
           -- gives the name of the metric and a help string that describes the
           -- value that the metric represents.
           $ P.Info "page_visits" "The number of visits to the index page."

runMetricsServer :: IO ()
runMetricsServer = do
    let port = 3000
    putStrLn $ "Listening at http://localhost:" ++ show port ++ "/"
    -- Register the GHC runtime metrics. For these to work, the app must be run
    -- with the `+RTS -T` command line options.
    _ <- P.register P.ghcMetrics
    -- Instrument the app with the prometheus middlware using the default
    -- `PrometheusSettings`. This will cause the app to dump the metrics when
    -- the /metrics endpoint is accessed.
    run port (P.prometheus P.def P.metricsApp)