{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}


module Main where

import qualified Control.Concurrent.Async as Async
import qualified Polysemy as PS

import qualified AsyncEffect as AE

import DsmrMetricsReader (DsmrTelegram(..), DsmrField(..), readMetrics)
import PrometheusMetricsServer (incCounter, pageVisits, runMetricsServer)

processMetricsCallback :: DsmrTelegram -> IO ()
processMetricsCallback (DsmrTelegram _ [EnergyConsumedTariff1 amount] _)  = incCounter pageVisits
processMetricsCallback (DsmrTelegram _ _ _)  = incCounter pageVisits

runApp :: PS.Members '[AE.Async] r => PS.Sem r ()
runApp = do
  metricsThread <- AE.async $ runMetricsServer 
  readDsmrThread <- AE.async $ readMetrics processMetricsCallback
  (_, runCoreResult) <- AE.awaitAny [metricsThread, readDsmrThread]

  -- kill all threads when one of the main threads ended
  AE.cancel metricsThread
  AE.cancel readDsmrThread

main :: IO ()
main = do
    runApp & asyncToIOFinal
