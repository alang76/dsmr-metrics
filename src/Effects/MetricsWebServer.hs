module Effects.MetricsWebServer
    (   runMetricsServer,
        runWebServerIO,
        MetricsWebServer(..)
    ) where

import qualified Network.Wai.Handler.Warp as W
import qualified Network.Wai as W
import qualified Prometheus as PM
import qualified Prometheus.Metric.GHC as PM
import Polysemy as P

data MetricsWebServer m a where
    RunMetricsServer ::  Int -> W.Application -> MetricsWebServer m ()

P.makeSem ''MetricsWebServer

runWebServerIO  :: P.Member (P.Embed IO) r => P.Sem (MetricsWebServer ': r) a -> P.Sem r a
runWebServerIO = P.interpret $ \case 
    (RunMetricsServer port app) -> P.embed $ do
        _ <- PM.register PM.ghcMetrics
        W.run port app

