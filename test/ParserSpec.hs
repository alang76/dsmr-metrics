module ParserSpec where

import Test.Hspec (shouldBe, it, describe, hspec)
import Data.Maybe (isJust)
import Data.Function ((&))
import Control.Lens ((^..))
import Control.Lens.Combinators (_Just)
import Data.Time (UTCTime(UTCTime), TimeOfDay(TimeOfDay), fromGregorian, timeOfDayToTime)
import Safe (headMay)
import qualified Effects.DsmrTelegramReader as DTR
import qualified Polysemy as P
import qualified Polysemy.Output as P
import SampleInput
import DsmrMetricsReader.Model
import DsmrMetricsReader.Internal

runParser :: (P.Members '[DTR.DsmrTelegramReader, P.Output String] r) => P.Sem r (Maybe DsmrTelegram)
runParser = do 
  telegram <- DTR.readTelegram
  runDsmrParser telegram

testParser :: IO ()
testParser = do
  parsedTelegram <- 
      runParser
        & P.runOutputSem (P.embed . putStrLn)
        & DTR.runTelegramReaderFakePure
        & P.runM

  hspec $
    describe "DsmrTelegramParser" $ do
    it "telegram can be parsed succesfully" $
      isJust parsedTelegram `shouldBe` True

    it "parsed telegram contains expected version header" $
      headMay (parsedTelegram  ^.. _Just . dsmrFields . traverse . versionNumber) `shouldBe` Just (42 :: Integer)

    it "parsed telegram contains expected timestamp" $
      headMay (parsedTelegram  ^.. _Just . dsmrFields . traverse . timeStamp) `shouldBe` Just (mkUTCTime (2020, 5, 29) (18, 33, 19) :: UTCTime)

    it "parsed telegram contains expected EquipmentID" $
      headMay (parsedTelegram  ^.. _Just . dsmrFields . traverse . equipmentID)  `shouldBe` Just "4530303034303031353934373534343134"

    it "parsed telegram contains expected EnergyConsumedTariff1" $
      headMay (parsedTelegram  ^.. _Just . dsmrFields . traverse . energyConsumedTariff1) `shouldBe` Just 014765.683

    it "parsed telegram contains expected EnergyConsumedTariff2" $
      headMay (parsedTelegram  ^.. _Just . dsmrFields . traverse . energyConsumedTariff2) `shouldBe` Just 0

    it "parsed telegram contains expected EnergyReturnedTariff1" $
      headMay (parsedTelegram  ^.. _Just . dsmrFields . traverse . energyReturnedTariff1) `shouldBe` Just 014628.043

    it "parsed telegram contains expected EnergyReturnedTariff2" $
      headMay (parsedTelegram  ^.. _Just . dsmrFields . traverse . energyReturnedTariff2) `shouldBe` Just 0

    it "parsed telegram contains expected ActualTariffIndicator" $
      headMay (parsedTelegram  ^.. _Just . dsmrFields . traverse . actualTariffIndicator) `shouldBe` Just 2

    it "parsed telegram contains expected ActualPowerConsumption" $
      headMay (parsedTelegram  ^.. _Just . dsmrFields . traverse . actualPowerConsumption) `shouldBe` Just 03.877

    it "parsed telegram contains expected ActualPowerReturned" $
      headMay (parsedTelegram  ^.. _Just . dsmrFields . traverse . actualPowerReturned) `shouldBe` Just (0 :: Double)
    
    it "parsed telegram contains expected NumberOfPowerFailures" $
      headMay (parsedTelegram  ^.. _Just . dsmrFields . traverse . numberOfPowerFailures) `shouldBe` Just 3

    it "parsed telegram contains expected numberOfLongPowerFailures" $
      headMay (parsedTelegram  ^.. _Just . dsmrFields . traverse . numberOfLongPowerFailures) `shouldBe` Just 2

    it "parsed telegram contains expected powerFailureLog" $
      headMay (parsedTelegram  ^.. _Just . dsmrFields . traverse . powerFailureLog) `shouldBe` Just [(mkUTCTime (2017, 3, 26) (08, 25, 19), 0029642045),(mkUTCTime (2016, 4, 17) (06, 31, 31), 0032998738)]

    it "parsed telegram contains expected numberOfVoltageSagsPhaseL1" $
      headMay (parsedTelegram  ^.. _Just . dsmrFields . traverse . numberOfVoltageSagsPhaseL1) `shouldBe` Just 0

    it "parsed telegram contains expected numberOfVoltageSagsPhaseL2" $
      headMay (parsedTelegram  ^.. _Just . dsmrFields . traverse . numberOfVoltageSagsPhaseL2) `shouldBe` Just 0

    it "parsed telegram contains expected actualCurrentConsumption" $
      headMay (parsedTelegram  ^.. _Just . dsmrFields . traverse . actualCurrentConsumption) `shouldBe` Just 17

    it "parsed telegram contains expected actualPowerConsumptionPhaseL1" $
      headMay (parsedTelegram  ^.. _Just . dsmrFields . traverse . actualPowerConsumptionPhaseL1) `shouldBe` Just 03.877

    it "parsed telegram contains expected actualPowerReturnedPhaseL1" $
      headMay (parsedTelegram  ^.. _Just . dsmrFields . traverse . actualPowerReturnedPhaseL1) `shouldBe` Just 0

    it "parsed telegram contains expected slaveGasMeterDeviceType" $
      headMay (parsedTelegram  ^.. _Just . dsmrFields . traverse . slaveGasMeterDeviceType) `shouldBe` Just 3

    it "parsed telegram contains expected gasMeterSerialNumber" $
      headMay (parsedTelegram  ^.. _Just . dsmrFields . traverse . gasMeterSerialNumber) `shouldBe` Just "4730303137353931323139313130333134"

    it "parsed telegram contains expected gasConsumption" $
      headMay (parsedTelegram  ^.. _Just . dsmrFields . traverse . gasConsumption) `shouldBe` Just (mkUTCTime (2020, 5, 29) (18, 00, 00), 05277.053)
