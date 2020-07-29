module AppSpec
  ( testApp
  )
where

import Prelude hiding (log)
import Test.Hspec (shouldBe, it, describe, hspec, runIO)
import App (runApp)
import Data.Function((&))

import Effects.DsmrTelegramReader(DsmrTelegramReader)
import Effects.MetricsWebServer(MetricsWebServer)
import Effects.UpdatePrometheusMetric(UpdatePrometheusMetric)
import Effects.Env(Env)

import TestEffects.Env(runEnvPureTest, runEnvPureTestConfigThrows)
import TestEffects.DsmrTelegramReader(runTelegramReaderFakePure)
import TestEffects.MetricsWebServer(runWebServerFakePure) --runWebServerFakeIO
import TestEffects.UpdatePrometheusMetric(runUpdatePrometheusMetricsFake)
import Safe(headMay)
import Effects.Async(asyncToIO)
import qualified Polysemy as P
import qualified Polysemy.Output as P
import qualified Polysemy.Error as P
import Exceptions.DsmrMetricException(DsmrMetricException(..))
import Events.DsmrMetricEvent(DsmrMetricEvent(..), ThreadID)
{--
simulateMain :: (P.Sem (MetricsWebServer ': r) a -> P.Sem r a) ->
                (P.Sem (DsmrTelegramReader ': r) a -> P.Sem r a) ->
                (P.Sem (Env ': r) a -> P.Sem r a) ->
                (P.Sem (UpdatePrometheusMetric:r) a -> P.Sem r a) ->
                IO ([DsmrMetricEvent], Either DsmrMetricException ([String], ()))
simulateMain webServerEffect telegramReaderEffect envEffect updateMetricEffect = runApp
                & webServerEffect
                & telegramReaderEffect
                & envEffect
                & updateMetricEffect
                & P.runOutputList @String
                & asyncToIO
                & P.errorToIOFinal @DsmrMetricException
                & P.runOutputList @DsmrMetricEvent
                & P.embedToFinal @IO
                & P.runFinal

                --}

simulateMain :: (forall r. P.Sem (MetricsWebServer ': r) () -> P.Sem r ())
             -> (forall r. P.Member Env r => P.Sem (DsmrTelegramReader ': r) () -> P.Sem r ())
             -> (forall r. P.Member (P.Error DsmrMetricException) r => P.Sem (Env ': r) () -> P.Sem r ())
             -> (forall r. P.Sem (UpdatePrometheusMetric ': r) () -> P.Sem (P.Output String ': r) ())
             -> IO ([DsmrMetricEvent], Either DsmrMetricException ([String], ()))
simulateMain webServerEffect telegramReaderEffect envEffect updateMetricEffect = runApp
                & webServerEffect
                & telegramReaderEffect
                & envEffect
                & updateMetricEffect
                & P.runOutputList @String
                & asyncToIO
                & P.errorToIOFinal @DsmrMetricException
                & P.runOutputList @DsmrMetricEvent
                & P.embedToFinal @IO
                & P.runFinal

isProgramStarted :: DsmrMetricEvent -> Bool
isProgramStarted ProgramStarted = True
isProgramStarted _ = False

isProgramTerminated :: DsmrMetricEvent -> Bool
isProgramTerminated ProgramTerminated = True
isProgramTerminated _ = False

-- isDsmrTelegramReaderThreadStarted :: DsmrMetricEvent -> Bool
-- isDsmrTelegramReaderThreadStarted (DsmrTelegramReaderThreadStarted _) = True
-- isDsmrTelegramReaderThreadStarted _ = False

getThreadID :: DsmrMetricEvent -> Maybe ThreadID
getThreadID (MetricsWebServerThreadStarted threadID) = Just threadID
getThreadID (DsmrTelegramReaderThreadStarted threadID) = Just threadID
getThreadID _ = Nothing

isMetricsWebServerThreadStarted :: DsmrMetricEvent -> Bool
isMetricsWebServerThreadStarted (MetricsWebServerThreadStarted _) = True
isMetricsWebServerThreadStarted _ = False

isThreadTerminated :: Maybe ThreadID -> DsmrMetricEvent -> Bool
isThreadTerminated matchID (ThreadTerminated threadID) = matchThreadID threadID matchID
isThreadTerminated _ _ = False

matchThreadID :: ThreadID -> Maybe ThreadID -> Bool
matchThreadID threadID maybeMatchID =
  case maybeMatchID of
    Just matchID -> threadID == matchID
    Nothing -> True

-- isDsmrTelegramReceived :: DsmrMetricEvent -> Bool
-- isDsmrTelegramReceived (DsmrTelegramReceived _) = True
-- isDsmrTelegramReceived _ = False

-- isDsmrTelegramParseError :: DsmrMetricEvent -> Bool
-- isDsmrTelegramParseError (DsmrTelegramParseError _) = True
-- isDsmrTelegramParseError _ = False

-- isDsmrTelegramParsed :: DsmrMetricEvent -> Bool
-- isDsmrTelegramParsed (DsmrTelegramParsed _) = True
-- isDsmrTelegramParsed _ = False

-- isInvalidConfigurationDetected :: DsmrMetricEvent -> Bool
-- isInvalidConfigurationDetected (InvalidConfigurationDetected _) = True
-- isInvalidConfigurationDetected _ = False

-- isFatalExceptionDetected :: DsmrMetricEvent -> Bool
-- isFatalExceptionDetected (FatalExceptionDetected _) = True
-- isFatalExceptionDetected _ = False


testApp :: IO ()
testApp = do
  hspec $
    describe "TestApp" $ do
      (events, _)           <- runIO $ simulateMain runWebServerFakePure runTelegramReaderFakePure runEnvPureTest             runUpdatePrometheusMetricsFake
      (_ , resConfigThrown) <- runIO $ simulateMain runWebServerFakePure runTelegramReaderFakePure runEnvPureTestConfigThrows runUpdatePrometheusMetricsFake

      it "Received program started event just once" $
        ((>0) . length . filter isProgramStarted $ events) `shouldBe` True

      it "Received program terminated event just once" $
        ((>0) . length . filter isProgramTerminated $ events) `shouldBe` True

      it "Program terminated event is the last event" $
        (last events) `shouldBe` ProgramTerminated

      it "Metrics web server thread was terminated first" $
        let
          metricsThreadIDStarted = headMay . filter isMetricsWebServerThreadStarted $ events
          matchesTerminatedID = case metricsThreadIDStarted of
            Nothing -> False
            Just metricsStartedEvent ->
              (>0) . length . filter (isThreadTerminated (getThreadID metricsStartedEvent)) $ events
        in
          matchesTerminatedID `shouldBe` True

      it "Configuration exception is raised to toplevel" $
        resConfigThrown `shouldBe ` Left (ConfigurationException "Fake exception")
