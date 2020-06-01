module DsmrMetricsReader
    ( readMetrics,
      DsmrMetric(PowerConsumedTariff1, PowerConsumedTariff2, PowerReturnedTariff1, PowerReturnedTariff2)
    ) where

import Control.Concurrent

data DsmrMetric =
      PowerConsumedTariff1 Int
    | PowerConsumedTariff2 Int
    | PowerReturnedTariff1 Int
    | PowerReturnedTariff2 Int


readMetrics :: (DsmrMetric -> IO ()) ->  IO ()
readMetrics callback =
    do
        let newMetric = PowerConsumedTariff1 0
        _ <- callback newMetric
        Control.Concurrent.threadDelay 5000000
        readMetrics callback
