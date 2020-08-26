module Main where
import Effects.DsmrTelegramReader(runTelegramReaderSerial) --runTelegramReaderFake
import Effects.ServeMetrics(runPrometheusMetricsServerIO)
import Effects.UpdatePrometheusMetric(runUpdatePrometheusMetricsIO)
import Effects.Env(runEnvIO)
import App(runApp, runOutputIO)
import qualified Effects.Async as AE
import qualified Polysemy as P
import qualified Polysemy.Error as P
import qualified Polysemy.Output as P
import Data.Function((&))
import Exceptions.DsmrMetricException(DsmrMetricException(..))
import Events.DsmrMetricEvent(DsmrMetricEvent(..))

runHandled :: P.Members '[P.Output DsmrMetricEvent, P.Embed IO] r => P.Sem r ()
runHandled = do
  res <- runApp
    & runPrometheusMetricsServerIO
    & runTelegramReaderSerial --runTelegramReaderFake
    & runEnvIO
    & runUpdatePrometheusMetricsIO
    & AE.asyncToIO
    & P.runError @DsmrMetricException
  case res of
    Left exc -> P.output . FatalExceptionDetected $ show exc
    Right _ -> P.output $ ProgramTerminated

main :: IO ()
main = do
    runHandled
    & runOutputIO
    & P.runM

