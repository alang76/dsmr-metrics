module TestEffects.ServeMetrics
    ( runMetricsServerFakePure
    , runMetricsServerFakeIO
    )
where

import qualified Polysemy as P
import Effects.ServeMetrics(ServeMetrics(..))
import Control.Concurrent(threadDelay)

runMetricsServerFakePure  :: P.Sem (ServeMetrics ': r) a -> P.Sem r a
runMetricsServerFakePure = P.interpret $ \case 
    ServeMetrics ->  pure () -- web server terminates immediately

runMetricsServerFakeIO  :: P.Member (P.Embed IO) r => P.Sem (ServeMetrics ': r) a -> P.Sem r a
runMetricsServerFakeIO = P.interpret $ \case 
    ServeMetrics ->  P.embed $ threadDelay (1000 * 1000) -- 1 sec