module Exceptions.DsmrMetricException
  ( DsmrMetricException(..)
  )
where

data DsmrMetricException = 
          ConfigurationException String
        | SerialPortException String
        | SomeOtherException String
        | DebugException String
    deriving (Eq, Show)

