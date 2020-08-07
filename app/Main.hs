module Main where

import Colog.Polysemy (runLogAction)
import Colog.Core.Action(LogAction(..))
import Control.Monad.IO.Class(MonadIO, liftIO)
import System.IO(hFlush, stdout)
import Effects.DsmrTelegramReader(runTelegramReaderSerial)
import Effects.ServeMetrics(runPrometheusMetricsServerIO)
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
    & runPrometheusMetricsServerIO
    & runTelegramReaderSerial
    & runEnvIO
    & runUpdatePrometheusMetricsIO
    & AE.asyncToIO
    & P.errorToIOFinal @DsmrMetricException
  case res of
    Left exc -> P.output . FatalExceptionDetected $ show exc
    Right _ -> P.output $ ProgramTerminated

logStringStdoutFlushed :: MonadIO m => LogAction m String
logStringStdoutFlushed = LogAction $ liftIO . (\str -> do { putStrLn str; hFlush stdout })
{-# INLINE logStringStdoutFlushed #-}
{-# SPECIALIZE logStringStdoutFlushed :: LogAction IO String #-}

main :: IO ()
main = do
    runHandled
    & runOutputAsLogAction
    & runLogAction @IO logStringStdoutFlushed
    & P.embedToFinal @IO
    & P.runFinal


-- DEBUG START

telegram :: String
telegram =
    "/XMX5LGBBFFB231215493\n\
    \1-3:0.2.8(42)\n\
    \0-0:1.0.0(200529163319S)\n\
    \0-0:96.1.1(4530303034303031353934373534343134)\n\
    \1-0:1.8.1(014765.683*kWh)\n\
    \1-0:2.8.1(000000.000*kWh)\n\
    \1-0:1.8.2(014628.043*kWh)\n\
    \1-0:2.8.2(000000.000*kWh)\n\
    \0-0:96.14.0(0002)\n\
    \1-0:1.7.0(03.877*kW)\n\
    \1-0:2.7.0(00.000*kW)\n\
    \0-0:96.7.21(00003)\n\
    \0-0:96.7.9(00002)\n\
    \1-0:99.97.0(2)(0-0:96.7.19)(170326062519S)(0029642045*s)(160417043131S)(0032998738*s)\n\
    \1-0:32.32.0(00000)\n\
    \1-0:32.36.0(00000)\n\
    \0-0:96.13.1()\n\
    \0-0:96.13.0()\n\
    \1-0:31.7.0(017*A)\n\
    \1-0:21.7.0(03.877*kW)\n\
    \1-0:22.7.0(00.000*kW)\n\
    \0-1:24.1.0(003)\n\
    \0-1:96.1.0(4730303137353931323139313130333134)\n\
    \0-1:24.2.1(200529160000S)(05277.053*m3)\n\
    \!7A67\n"

runHandledLoop :: P.Members '[P.Output DsmrMetricEvent, P.Final IO, P.Embed IO] r => P.Sem r ()
runHandledLoop = do
  res <- runApp
    & runPrometheusMetricsServerIO
    & runTelegramReaderSerial
    & runEnvIO
    & runUpdatePrometheusMetricsIO
    & AE.asyncToIO
    & P.errorToIOFinal @DsmrMetricException
  case res of
    Left exc -> P.output . FatalExceptionDetected $ show exc
    Right _ -> P.output $ ProgramTerminated


-- DEBUG END
