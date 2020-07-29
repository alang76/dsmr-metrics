module TestEffects.MetricsWebServer
    ( runWebServerFakePure
    , runWebServerFakeIO
    )
where

import qualified Polysemy as P
import Effects.MetricsWebServer(MetricsWebServer(..))
import Control.Concurrent(threadDelay)


runWebServerFakePure  :: P.Sem (MetricsWebServer ': r) a -> P.Sem r a
runWebServerFakePure = P.interpret $ \case 
    (RunMetricsServer _ _) ->  pure () -- web server terminates immediately

runWebServerFakeIO  :: P.Member (P.Embed IO) r => P.Sem (MetricsWebServer ': r) a -> P.Sem r a
runWebServerFakeIO = P.interpret $ \case 
    (RunMetricsServer _ _) ->  P.embed $ threadDelay (1000 * 1000) -- 1 sec