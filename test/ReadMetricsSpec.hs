

module ReadMetricsSpec where

import Test.Hspec (shouldBe, it, describe, hspec)
--import Test.QuickCheck (property) -- TODO: check out quickcheck

import Control.Lens --((&), (^..), (^.), Lens') -- TODO: fix import
import Data.Time.LocalTime(TimeZone(..))

import qualified Polysemy as P
import qualified Polysemy.Output as P

import Effects.DsmrTelegramReader
import DsmrMetricsReader.Internal

import TelegramBuilder(serializeTelegram, createTestTelegram)
import Effects.Env

runEnvPureTest :: P.Sem (Env ': r) a -> P.Sem r a
runEnvPureTest = P.interpret $ \case 
  GetEnvironmentTimeZone -> return $ TimeZone 120 True "TTZ"

runTelegramReaderFakePureTest :: P.Member Env r => P.Sem (DsmrTelegramReader ': r) a  -> P.Sem r a
runTelegramReaderFakePureTest = 
    P.interpret $ \case
      ReadTelegram -> do
        _telegram <- createTestTelegram 
        serializeTelegram _telegram

testReadMetrics :: IO ()
testReadMetrics =
    hspec $
      describe "TestReadMetrics" $ do
        _telegram <- createTestTelegram & runEnvPureTest & P.runM
        (outputList, maybeTelegram) <- readMetrics pure
          & runTelegramReaderFakePureTest
          & runEnvPureTest
          & P.runOutputList
          & P.runM

        it "no entries in output list" $
          (outputList :: [String]) `shouldBe` []
        it "parsed telegram should match input telegram" $
          maybeTelegram `shouldBe` Just _telegram
