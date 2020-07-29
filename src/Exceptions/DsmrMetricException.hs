module Exceptions.DsmrMetricException
  ( DsmrMetricException(..)
  )
where

data DsmrMetricException = 
          ConfigurationException String
        | SerialPortException String
        | SomeOtherException String
    deriving (Eq, Show)

