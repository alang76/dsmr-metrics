module TestEffects.Env
    ( runEnvPureTest
    , runEnvPureTestConfigThrows
    )
where

import qualified Polysemy as P
import qualified Polysemy.Error as P
import Data.Time.LocalTime(TimeZone(..))
import Effects.Env(Env(..))
import Configuration(Configuration(..), SerialConfig(..), WebServerConfig(..), BaudRate(..))
import Exceptions.DsmrMetricException(DsmrMetricException(..))


testConfig :: Configuration
testConfig = 
  Configuration {
    serialConfig = SerialConfig { 
      port = "/dev/fake", 
      baudRate = BR115200,
      bufferSize = 40000},
    webServerConfig = WebServerConfig {
      listenPort = 3000
    }
  }
    
runEnvPureTest :: P.Sem (Env ': r) a -> P.Sem r a
runEnvPureTest = P.interpret $ \case
  GetEnvironmentTimeZone -> return $ TimeZone 120 True "TTZ"
  GetEnvironmentTimeCentury -> return 20
  GetEnvironmentConfiguration -> return $ Right testConfig

runEnvPureTestConfigThrows :: P.Member (P.Error DsmrMetricException) r => P.Sem (Env ': r) a -> P.Sem r a
runEnvPureTestConfigThrows = P.interpret $ \case
  GetEnvironmentTimeZone -> return $ TimeZone 120 True "TTZ"
  GetEnvironmentTimeCentury -> return 20
  GetEnvironmentConfiguration -> P.throw $ ConfigurationException "Fake exception"
