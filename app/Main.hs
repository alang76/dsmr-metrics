module Main where

import Colog.Polysemy (runLogAction)
import Colog.Core.IO(logStringStdout)
import Effects.DsmrTelegramReader(runTelegramReaderSerial)
import Effects.MetricsWebServer(runWebServerIO)
import Effects.UpdatePrometheusMetric(runUpdatePrometheusMetricsIO)
import Effects.Env(runEnvIO)
import App(runApp, runOutputAsLogAction)
import qualified Effects.Async as AE
import qualified Polysemy as P
import qualified Polysemy.Error as P
import qualified Polysemy.Output as P
import Data.Function((&))
import Exceptions.DsmrMetricException(DsmrMetricException(..))
import Events.DsmrMetricEvent(DsmrMetricEvent(..))

runHandled :: P.Members '[P.Output DsmrMetricEvent, P.Final IO, P.Embed IO] r => P.Sem r ()
runHandled = do
  res <- runApp
    & runWebServerIO
    & runTelegramReaderSerial
    & runEnvIO
    & runUpdatePrometheusMetricsIO
    & AE.asyncToIO
    & P.errorToIOFinal @DsmrMetricException
  case res of
    Left exc -> P.output . FatalExceptionDetected $ show exc
    Right _ -> P.output $ ProgramTerminated

main :: IO ()
main = do
    runHandled
    & runOutputAsLogAction
    & runLogAction @IO logStringStdout
    & P.embedToFinal @IO
    & P.runFinal
