{-# LANGUAGE BangPatterns #-}

module ReadMetricsSpec where

import Test.Hspec (shouldBe, it, describe, hspec)
--import Test.QuickCheck (property) -- TODO: check out quickcheck
import Data.Maybe (isJust)
import Data.Time.Format (parseTimeOrError, defaultTimeLocale)
import Control.DeepSeq (deepseq)
import Control.Lens --((&), (^..), (^.), Lens') -- TODO: fix import
import Safe (headMay)
import qualified Polysemy as P
import qualified Polysemy.Output as P

import Effects.DsmrTelegramReader
import DsmrMetricsReader.Internal
import DsmrMetricsReader.Model
import SampleInput
import TelegramBuilder
import TestEnv


testTelegram :: DsmrTelegram
testTelegram = 
   let 
    createUtcTime = parseTimeOrError True defaultTimeLocale "%y%m%d%H%M%S" . init
  in
    buildTelegram 
        "XMX5LGBBFFB231215493"                                                                  -- meter ID
        42                                                                                      -- version ID
        (createUtcTime "200529163319S")                                                         -- timestamp
        "4530303034303031353934373534343134"                                                    -- equipment ID
        14765.683                                                                               -- energyConsumedTariff1
        0                                                                                       -- energyConsumedTariff2
        14628.043                                                                               -- energyReturnedTariff1
        0                                                                                       -- energyReturnedTariff2
        2                                                                                       -- actualTariffIndicator
        3.877                                                                                   -- actualPowerConsumption        
        0                                                                                       -- actualPowerReturned           
        3                                                                                       -- numberOfPowerFailures         
        2                                                                                       -- numberOfLongPowerFailures     
        [(createUtcTime "170326062519S", 29642045), (createUtcTime "160417043131S", 32998738)]  -- powerFailureLog               
        0                                                                                       -- numberOfVoltageSagsPhaseL1    
        0                                                                                       -- numberOfVoltageSagsPhaseL2    
        17                                                                                      -- actualCurrentConsumption      
        3.877                                                                                   -- actualPowerConsumptionPhaseL1 
        0                                                                                       -- actualPowerReturnedPhaseL1    
        3                                                                                       -- slaveGasMeterDeviceType       
        "4730303137353931323139313130333134"                                                    -- gasMeterSerialNumber          
        (createUtcTime "200529160000S", 5277.053)                                               -- gasConsumption 
        7667                                                                                    -- checkSum 

runTelegramReaderFakePureTest :: P.Member TestEnv r => P.Sem (DsmrTelegramReader ': r) a  -> P.Sem r a
runTelegramReaderFakePureTest = 
    P.interpret $ \case
      ReadTelegram -> serializeTelegram testTelegram

testReadMetrics :: IO ()
testReadMetrics =
    hspec $
      describe "TestReadMetrics" $ do
        let testCallback !maybeTelegram = pure $ it "parsed telegram should match input telegram" $ maybeTelegram `shouldBe` Just testTelegram --TODO check result somehow of callback
        (outputList, res) <- readMetrics testCallback
          & runTelegramReaderFakePureTest
          & runEnvPureTest
          & P.runOutputList
          & P.runM

        it "no entries in output list" $
            outputList `shouldBe` []


{-
readMetrics :: (P.Members '[DsmrTelegramReader, P.Output String] r) => (DsmrTelegram -> P.Sem r ()) -> P.Sem r ()
readMetrics callback =
    do
        telegram <- readTelegram
        parseResult <- runDsmrParser telegram
        case parseResult of
          Just dsmrTelegram -> callback dsmrTelegram
          Nothing -> pure ()
-}