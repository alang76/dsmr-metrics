{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}


module Effects.DsmrTelegramReader (
    DsmrTelegramReader(..),
    readTelegram,
    runTelegramReaderSerial,
    runTelegramReaderFakeIO,
    runTelegramReaderFakePure
    ) where

import Control.Concurrent (threadDelay)
import Polysemy as P

import Debug.Trace (trace) -- DEBUG ONLY!! TODO: REMOVE

data DsmrTelegramReader m a where
  ReadTelegram :: DsmrTelegramReader m String

P.makeSem ''DsmrTelegramReader

readSerial :: IO String
readSerial = undefined

runTelegramReaderSerial :: P.Member (P.Embed IO) r => P.Sem (DsmrTelegramReader ': r) String -> P.Sem r String
runTelegramReaderSerial = P.interpret $ \case ReadTelegram -> P.embed readSerial

--runWebServerIO  :: P.Member (P.Embed IO) r => P.Sem (MetricsWebServer ': r) a -> P.Sem r a
--runWebServerIO = P.interpret $ \case (RunServer port app) -> P.embed $ W.run port app

runTelegramReaderFakeIO :: P.Member (P.Embed IO) r => P.Sem (DsmrTelegramReader ': r) a -> P.Sem r a
runTelegramReaderFakeIO = trace "running reader fake IO" $ P.interpret $ \case 
  ReadTelegram ->  P.embed $ do
    threadDelay 5000000
    pure telegram

runTelegramReaderFakePure :: P.Sem (DsmrTelegramReader ': r) a  -> P.Sem r a
runTelegramReaderFakePure = P.interpret $ \case
  ReadTelegram -> return telegram

telegram :: String
telegram = 
    "/XMX5LGBBFFB231215493\n\
    \1-3:0.2.8(42)\n\
    \0-0:1.0.0(200529163319S)\n\
    \0-0:96.1.1(4530303034303031353934373534343134)\n\
    \1-0:1.8.1(014765.683*kWh)\n\
    \1-0:2.8.1(000000.000*kWh)\n\
    \1-0:1.8.2(014628.043*kWh)\n\
    \1-0:2.8.2(000000.000*kWh)\n\
    \0-0:96.14.0(0002)\n\
    \1-0:1.7.0(03.877*kW)\n\
    \1-0:2.7.0(00.000*kW)\n\
    \0-0:96.7.21(00003)\n\
    \0-0:96.7.9(00002)\n\
    \1-0:99.97.0(2)(0-0:96.7.19)(170326062519S)(0029642045*s)(160417043131S)(0032998738*s)\n\
    \1-0:32.32.0(00000)\n\
    \1-0:32.36.0(00000)\n\
    \0-0:96.13.1()\n\
    \0-0:96.13.0()\n\
    \1-0:31.7.0(017*A)\n\
    \1-0:21.7.0(03.877*kW)\n\
    \1-0:22.7.0(00.000*kW)\n\
    \0-1:24.1.0(003)\n\
    \0-1:96.1.0(4730303137353931323139313130333134)\n\
    \0-1:24.2.1(200529160000S)(05277.053*m3)\n\
    \!7667\n"