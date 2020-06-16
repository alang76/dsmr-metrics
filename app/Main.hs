module Main where

import DsmrMetricsReader as DMR
import PrometheusMetricsServer as PS
import qualified Control.Concurrent.Async as Async

processMetricsCallback :: DMR.DsmrTelegram -> IO ()
processMetricsCallback (DsmrTelegram _ [PowerConsumedTariff1 amount] _)  = PS.incCounter pageVisits
processMetricsCallback (DsmrTelegram _ _ _)  = PS.incCounter pageVisits

main :: IO ()
main = do
  --let config = undefined
  --let logger = undefined

  -- start threads
  metricsThread <- Async.async $ PS.runMetricsServer 
  readDsmrThread <- Async.async $ DMR.readMetrics processMetricsCallback

  --postLog logger LogInfo "DsmrReader running."

  -- Everything should stop when any of these stops
  (_, runCoreResult) <- Async.waitAny [metricsThread, readDsmrThread]
  --logRunCoreResult logger runCoreResult

  -- kill all threads when one of the main threads ended
  Async.cancel metricsThread
  Async.cancel readDsmrThread
