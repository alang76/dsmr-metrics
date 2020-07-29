module TestEffects.Env
    ( runEnvPureTest
    , runEnvPureTestConfigThrows
    )
where

import qualified Polysemy as P
import qualified Polysemy.Error as P
import Data.Time.LocalTime(TimeZone(..))
import Effects.Env(Env(..))
import Configuration(Configuration(..), SerialConfig(..), BaudRate(..))
import Exceptions.DsmrMetricException(DsmrMetricException(..))
    
runEnvPureTest :: P.Sem (Env ': r) a -> P.Sem r a
runEnvPureTest = P.interpret $ \case
  GetEnvironmentTimeZone -> return $ TimeZone 120 True "TTZ"
  GetEnvironmentTimeCentury -> return 20
  GetEnvironmentConfiguration -> return $ Right Configuration {serialConfig = SerialConfig { serialPort = "/dev/fake", serialBaudRate = BR110 }}

runEnvPureTestConfigThrows :: P.Member (P.Error DsmrMetricException) r => P.Sem (Env ': r) a -> P.Sem r a
runEnvPureTestConfigThrows = P.interpret $ \case
  GetEnvironmentTimeZone -> return $ TimeZone 120 True "TTZ"
  GetEnvironmentTimeCentury -> return 20
  GetEnvironmentConfiguration -> P.throw $ ConfigurationException "Fake exception"
