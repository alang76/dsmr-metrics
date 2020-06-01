module Main where

import DsmrMetricsReader
import PrometheusMetricsServer
import qualified Control.Concurrent.Async as Async

main :: IO ()
main =
  --let config = undefined
  --let logger = undefined

  -- start threads
  let processMetricsCallback = registerMetrics
  metricsThread <- Async.async $ readMetrics processMetricsCallback
  readDsmrThread <- Async.async $ readDsmrMetrics

  postLog logger LogInfo "DsmrReader running."

  -- Everything should stop when any of these stops
  (_, runCoreResult) <- Async.waitAny [metricsThread, readDsmrThread]
  logRunCoreResult logger runCoreResult

  -- kill all threads when one of the main threads ended
  Async.cancel metricsThread
  Async.cancel readDsmrThread
