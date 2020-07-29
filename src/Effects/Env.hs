module Effects.Env 
  ( Env(..)
  , getEnvironmentTimeZone
  , getEnvironmentTimeCentury
  , getEnvironmentConfiguration
  , runEnvIO ) 
where

import Data.Time.Clock(utctDay, getCurrentTime)
import Data.Time.Calendar(toGregorian)
import Data.Time.LocalTime(getCurrentTimeZone, TimeZone(..))
import Configuration(loadConfig, Configuration(..))
import Polysemy as P

data Env m a where
  GetEnvironmentTimeZone :: Env m TimeZone
  GetEnvironmentTimeCentury :: Env m Integer
  GetEnvironmentConfiguration :: Env m (Either String Configuration)
P.makeSem ''Env

runEnvIO :: P.Member (P.Embed IO) r => P.Sem (Env ': r) a -> P.Sem r a
runEnvIO = P.interpret $ \case 
  GetEnvironmentTimeZone -> P.embed getCurrentTimeZone
  GetEnvironmentTimeCentury -> do
    curTimeUtc <- P.embed getCurrentTime 
    let (year, _, _) = toGregorian $ utctDay curTimeUtc
    return $ floor ((fromInteger year :: Double) / 100)
  GetEnvironmentConfiguration -> P.embed loadConfig
{-# INLINE runEnvIO #-}