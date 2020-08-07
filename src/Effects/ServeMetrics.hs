
{-# LANGUAGE AllowAmbiguousTypes #-}

module Effects.ServeMetrics
    (   ServeMetrics(..),
        serveMetrics,
        runPrometheusMetricsServerIO,
    ) where

import Control.Monad.IO.Class(MonadIO)
import Configuration(Configuration(..), WebServerConfig(..))
import Events.DsmrMetricEvent(DsmrMetricEvent(..))
import Exceptions.DsmrMetricException(DsmrMetricException)
import Effects.Env(Env, getConfiguration)
import qualified Network.Wai.Handler.Warp as W
import qualified Network.Wai as W
import qualified Network.Wai.Middleware.Prometheus as PM
import qualified Prometheus as PM
import qualified Prometheus.Metric.GHC as PM
import qualified Polysemy as P
import qualified Polysemy.Output as P
import qualified Polysemy.Error as P

data ServeMetrics m a where
    ServeMetrics :: ServeMetrics m ()

P.makeSem ''ServeMetrics

-- TODO: catch IO Exceptions (such as port in use)
runPrometheusMetricsServerIO  :: P.Members '[P.Output DsmrMetricEvent, Env, P.Embed IO, P.Error DsmrMetricException ] r => P.Sem (ServeMetrics ': r) a -> P.Sem r a
runPrometheusMetricsServerIO = P.interpret $ \case
    ServeMetrics -> do
        port <- listenPort . webServerConfig <$> getConfiguration 
        P.output . CheckPoint $ "Starting prometheus metrics server at http://localhost:" ++ show port ++ "/"
            
        -- Register the GHC runtime metrics. For these to work, the app must be run
        -- with the `+RTS -T` command line options.

        -- Instrument the app with the prometheus middlware using the default
        -- `PrometheusSettings`. This will cause the app to dump the metrics when
        -- the /metrics endpoint is accessed.        
        P.embed $ do
            _ <- PM.register PM.ghcMetrics
            W.run port (PM.prometheus PM.def PM.metricsApp)
