{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module PolyWeb (WebServer, PendingWebRequest, startWebServer,
  respondWebRequest, getBody, runWebServerFinal) where
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.HTTP.Types.Status as HTTP
import Polysemy
import Polysemy.Final
import Data.Functor
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

newtype PendingWebRequest =
  PendingWebRequest (Wai.Response -> IO Wai.ResponseReceived)

data WebServer m a where
  StartWebServer :: Warp.Port -> (
    Wai.Request -> PendingWebRequest -> m Wai.ResponseReceived) ->
    WebServer m ()
  RespondWebRequest :: PendingWebRequest -> Wai.Response ->
                       WebServer m Wai.ResponseReceived
  GetBody :: Int -> Wai.Request -> WebServer m (Maybe BS.ByteString)
makeSem ''WebServer

runStartWebServer :: forall rInitial r f.
  (Member (Final IO) r, Functor f) =>
  Warp.Port -> (
    Wai.Request -> PendingWebRequest ->
    Sem rInitial Wai.ResponseReceived) ->
  Sem (WithTactics WebServer f (Sem rInitial) r) (f ())
runStartWebServer port app = do
  s0 <- getInitialStateT
  appFnS <- bindT $ uncurry app
  ins <- getInspectorT
  let
    appFn :: (Wai.Request, PendingWebRequest) ->
             Sem r (Maybe Wai.ResponseReceived)
    appFn = runWebServerFinal . (fmap (inspect ins)) . appFnS . (s0 $>)
  withStrategicToFinal $ do
    appFnS' <- bindS (raise . appFn)
    ins' <- getInspectorS
    s1 <- getInitialStateS
    let
      appFn' :: (Wai.Request, PendingWebRequest) ->
                IO (Maybe Wai.ResponseReceived)
      appFn' = (fmap (join . inspect ins')) . appFnS' . (s1 $>)
    return $ do
      let
        doRequestIO :: Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) ->
                       IO Wai.ResponseReceived
        doRequestIO req respond = do
          maybeRR <- appFn' (req, PendingWebRequest respond)
          case maybeRR of
            Just rr -> return rr
            Nothing -> respond $
              Wai.responseLBS (HTTP.status500) [] "Internal server error"
          
      Warp.run port $ \req reply -> doRequestIO req reply
      return $ s1 $> s0

runRespondWebRequest :: forall rInitial r f.
  (Member (Final IO) r, Functor f) =>
  PendingWebRequest -> Wai.Response ->
  Sem (WithTactics WebServer f (Sem rInitial) r) (f Wai.ResponseReceived)
runRespondWebRequest (PendingWebRequest respond) resp = do
  s0 <- getInitialStateT
  withStrategicToFinal $ do
    s1 <- getInitialStateS
    return $ do
      rr <- respond (resp)
      return $ s1 $> (s0 $> rr)

runGetBody :: forall rInitial r f.
  (Member (Final IO) r, Functor f) =>
  Int -> Wai.Request ->
  Sem (WithTactics WebServer f (Sem rInitial) r) (f (Maybe BS.ByteString))
runGetBody maxLen req = do
  body <- embedFinal $ Wai.lazyRequestBody req
  let strictBody = LBS.toStrict $ LBS.take (fromIntegral $ maxLen + 1) body
  if BS.length strictBody > maxLen
    then pureT Nothing
    else pureT (Just strictBody)

runWebServerFinal :: Member (Final IO) r =>
                     Sem (WebServer ': r) a -> Sem r a
runWebServerFinal =
  interpretH (\v -> case v of
                 StartWebServer port app -> runStartWebServer port app
                 RespondWebRequest reqId response -> runRespondWebRequest reqId response
                 GetBody maxLen req -> runGetBody maxLen req
             )