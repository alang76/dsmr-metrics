module TestEffects.Env
    ( runEnvPureTest
    )
where

import qualified Polysemy as P
import Data.Time.LocalTime(TimeZone(..))
import Effects.Env(Env(..))
    
runEnvPureTest :: P.Sem (Env ': r) a -> P.Sem r a
runEnvPureTest = P.interpret $ \case
  GetEnvironmentTimeZone -> return $ TimeZone 120 True "TTZ"
  GetEnvironmentTimeCentury -> return 20