module AppSpec
  ( testApp
  )
where

import Prelude hiding (log)
import Test.Hspec (shouldBe, it, describe, hspec, runIO)
import App (runApp)
import Data.Function((&))

import Effects.DsmrTelegramReader(DsmrTelegramReader)
import Effects.ServeMetrics(ServeMetrics)
import Effects.UpdatePrometheusMetric(UpdatePrometheusMetric)
import Effects.Env(Env)

import TestEffects.Env(runEnvPureTest, runEnvPureTestConfigThrows)
import TestEffects.DsmrTelegramReader(runTelegramReaderFakePure)
import TestEffects.ServeMetrics(runMetricsServerFakePure) --runMetricsServerFakeIO
import TestEffects.UpdatePrometheusMetric(runUpdatePrometheusMetricsFake)
import Safe(headMay)
import Effects.Async(asyncToIO)
import qualified Polysemy as P
import qualified Polysemy.Output as P
import qualified Polysemy.Error as P
import Exceptions.DsmrMetricException(DsmrMetricException(..))
import Events.DsmrMetricEvent(DsmrMetricEvent(..), ThreadID)

simulateMain :: (forall r. P.Sem (ServeMetrics ': r) () -> P.Sem r ())
             -> (forall r. P.Member Env r => P.Sem (DsmrTelegramReader ': r) () -> P.Sem r ())
             -> (forall r. P.Member (P.Error DsmrMetricException) r => P.Sem (Env ': r) () -> P.Sem r ())
             -> (forall r. P.Sem (UpdatePrometheusMetric ': r) () -> P.Sem (P.Output String ': r) ())
             -> IO ([DsmrMetricEvent], Either DsmrMetricException ([String], ()))
simulateMain serveMetricsEffect telegramReaderEffect envEffect updateMetricEffect = runApp
                & serveMetricsEffect
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

getThreadID :: DsmrMetricEvent -> Maybe ThreadID
getThreadID (MetricsServerThreadStarted threadID) = Just threadID
getThreadID (DsmrTelegramReaderThreadStarted threadID) = Just threadID
getThreadID _ = Nothing

isMetricsWebServerThreadStarted :: DsmrMetricEvent -> Bool
isMetricsWebServerThreadStarted (MetricsServerThreadStarted _) = True
isMetricsWebServerThreadStarted _ = False

isThreadTerminated :: Maybe ThreadID -> DsmrMetricEvent -> Bool
isThreadTerminated matchID (ThreadTerminated threadID) = matchThreadID threadID matchID
isThreadTerminated _ _ = False

matchThreadID :: ThreadID -> Maybe ThreadID -> Bool
matchThreadID threadID maybeMatchID =
  case maybeMatchID of
    Just matchID -> threadID == matchID
    Nothing -> True

testApp :: IO ()
testApp = do
  hspec $
    describe "TestApp" $ do
      (events, _)           <- runIO $ simulateMain runMetricsServerFakePure runTelegramReaderFakePure runEnvPureTest             runUpdatePrometheusMetricsFake
      (_ , resConfigThrown) <- runIO $ simulateMain runMetricsServerFakePure runTelegramReaderFakePure runEnvPureTestConfigThrows runUpdatePrometheusMetricsFake

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
      
      --TODO: Test following:
      -- test whether succesfully parsed telegram is passed to callback
      -- force readTelegram thread termination
      -- simulate reading of bad (non-parseable) data 
      -- simulate web server IO error
      
