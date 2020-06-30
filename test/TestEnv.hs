module TestEnv where

import Data.Time.LocalTime (TimeZone(..), getCurrentTimeZone)
import Polysemy as P

data TestEnv m a where
  GetEnvironmentTimeZone :: TestEnv m TimeZone

P.makeSem ''TestEnv

runEnvIO :: P.Member (P.Embed IO) r => P.Sem (TestEnv ': r) a -> P.Sem r a
runEnvIO = P.interpret $ \case 
  GetEnvironmentTimeZone -> P.embed $  getCurrentTimeZone

runEnvPureTest :: P.Sem (TestEnv ': r) a -> P.Sem r a
runEnvPureTest = P.interpret $ \case 
  GetEnvironmentTimeZone -> return $ TimeZone 120 True "TTZ"
