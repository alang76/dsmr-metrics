module Effects.Env (
    Env(..),
    getEnvironmentTimeZone,
    runEnvIO
    ) where

import Data.Time.LocalTime(getCurrentTimeZone, TimeZone(..))
import Polysemy as P

data Env m a where
  GetEnvironmentTimeZone :: Env m TimeZone

P.makeSem ''Env

runEnvIO :: P.Member (P.Embed IO) r => P.Sem (Env ': r) a -> P.Sem r a
runEnvIO = P.interpret $ \case GetEnvironmentTimeZone -> P.embed getCurrentTimeZone
