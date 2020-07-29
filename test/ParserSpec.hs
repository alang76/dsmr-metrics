{-# LANGUAGE OverloadedStrings #-}

module ParserSpec where  

import Test.Hspec (shouldBe, it, describe, hspec)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hedgehog (tripping, checkParallel, forAll, property, Gen, Property, Group(..), Range)
import Data.Maybe (isJust)
import Data.Function ((&))
import Data.Time.Clock(UTCTime(..))
import Control.Lens ((^..))
import Control.Lens.Combinators (_Just)
import Safe (headMay)
import qualified Polysemy as P
import qualified Polysemy.Output as P
import Model.DsmrTelegram(
    DsmrTelegram(..)
  , gasConsumption
  , dsmrFields
  , gasMeterSerialNumber
  , slaveGasMeterDeviceType
  , actualPowerReturnedPhaseL1
  , actualPowerConsumptionPhaseL1
  , actualCurrentConsumption
  , numberOfVoltageSagsPhaseL2
  , numberOfVoltageSagsPhaseL1
  , powerFailureLog
  , numberOfLongPowerFailures
  , numberOfPowerFailures
  , actualPowerReturned
  , actualPowerConsumption,actualTariffIndicator,energyReturnedTariff2,energyReturnedTariff1,energyConsumedTariff2,energyConsumedTariff1,equipmentID,timeStamp,versionNumber)
import DsmrTelegramParser.Internal(runDsmrParser)
import Util.Time(mkUTCTime)
import TimeSpec(genUTCTime)
import TelegramBuilder(buildTelegram, serializeTelegram)
import Effects.Env(Env(..))
import Effects.DsmrTelegramReader(DsmrTelegramReader(..), readTelegram)
import TestEffects.DsmrTelegramReader(runTelegramReaderFakePure)
import TestEffects.Env(runEnvPureTest)
import Events.DsmrMetricEvent(DsmrMetricEvent(..))


maxValueInt :: Int
maxValueInt = maxBound

maxValueInteger :: Integer
maxValueInteger = 9999999999999999999999999999

maxValueDouble :: Double
maxValueDouble = 9999999999999999999999999999

genStrVar :: Int -> Gen String
genStrVar l = Gen.string (Range.linear 0 l) Gen.alphaNum

genStrConst :: Int -> Gen String
genStrConst l = Gen.string (Range.constant l l) Gen.alphaNum

genDouble :: Gen Double
genDouble = Gen.double (Range.linearFrac 0 maxValueDouble)

genInteger :: Gen Integer
genInteger = Gen.integral (Range.linear 0 maxValueInteger)

genInt :: Gen Int
genInt = Gen.int (Range.linear 0 maxValueInt :: Range Int)

genStampedValue:: Gen a -> Gen (UTCTime, a)
genStampedValue genValue = do
  ts <- genUTCTime
  v <- genValue
  return (ts, v)

genPowerFailureLog :: Gen [(UTCTime, Integer)]
genPowerFailureLog = Gen.list (Range.linear 0 10000) (genStampedValue genInteger)

genTelegram :: Gen DsmrTelegram
genTelegram = 
  buildTelegram 
  <$> genStrVar 96              -- meterIdGen                        
  <*> genInteger                -- versionNumberGen                  
  <*> genUTCTime                -- timeStampGen                      
  <*> genStrVar 96              -- equipmentIDGen                    
  <*> genDouble                 -- energyConsumedTariff1Gen          
  <*> genDouble                 -- energyConsumedTariff2Gen          
  <*> genDouble                 -- energyReturnedTariff1Gen          
  <*> genDouble                 -- energyReturnedTariff2Gen          
  <*> genInt                    -- actualTariffIndicatorGen          
  <*> genDouble                 -- actualPowerConsumptionGen         
  <*> genDouble                 -- actualPowerReturnedGen            
  <*> genInteger                -- numberOfPowerFailuresGen          
  <*> genInteger                -- numberOfLongPowerFailuresGen      
  <*> genPowerFailureLog        -- powerFailureLogGen                
  <*> genInteger                -- numberOfVoltageSagsPhaseL1Gen     
  <*> genInteger                -- numberOfVoltageSagsPhaseL2Gen     
  <*> genInteger                -- actualCurrentConsumptionGen       
  <*> genDouble                 -- actualPowerConsumptionPhaseL1Gen  
  <*> genDouble                 -- actualPowerReturnedPhaseL1Gen     
  <*> genInt                    -- slaveGasMeterDeviceTypeGen        
  <*> genStrVar 96              -- gasMeterSerialNumberGen           
  <*> genStampedValue genDouble -- gasConsumptionGen                 
  <*> genInteger                -- checkSumGen                       

propTripTelegram :: Property
propTripTelegram = property $ do
  t <- forAll genTelegram
  tripping t prettyTelegram parseTelegram

hedgehogTests:: IO Bool
hedgehogTests =
  checkParallel $ Group "Hedgehog parser tests" [ ("prop_tripping_telegram", propTripTelegram) ]

prettyTelegram :: DsmrTelegram -> String
prettyTelegram telegram =
  serializeTelegram telegram
    & runEnvPureTest
    & P.run

parseTelegram :: String -> Maybe DsmrTelegram
parseTelegram telegram = 
  runDsmrParser telegram
    & P.ignoreOutput
    & runEnvPureTest
    & P.run

runParser :: (P.Members '[DsmrTelegramReader, P.Output DsmrMetricEvent, Env] r) => P.Sem r (Maybe DsmrTelegram)
runParser = do
  telegram <- readTelegram
  runDsmrParser telegram

isParsedEvent :: DsmrMetricEvent -> Bool
isParsedEvent (DsmrTelegramParsed _) = True
isParsedEvent _ = False

testParser :: IO ()
testParser = do
  (outputList, parsedTelegram) <-
      runParser
        & runTelegramReaderFakePure
        & runEnvPureTest
        & P.runOutputList
        & P.runM

  -- hspec tests
  hspec $
    describe "DsmrTelegramParser" $ do
      it "telegram can be parsed succesfully" $
        isJust parsedTelegram `shouldBe` True

      -- TODO: Properly factor out the common code
      it "parsed telegram contains expected version header" $
        headMay (parsedTelegram  ^.. _Just . dsmrFields . traverse . versionNumber) `shouldBe` Just (42 :: Integer)

      it "parsed telegram contains expected timestamp" $
        headMay (parsedTelegram  ^.. _Just . dsmrFields . traverse . timeStamp) `shouldBe` Just (mkUTCTime (2020, 5, 29) (14, 33, 19) :: UTCTime)

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
        headMay (parsedTelegram  ^.. _Just . dsmrFields . traverse . powerFailureLog) `shouldBe` Just [(mkUTCTime (2017, 3, 26) (04, 25, 19), 0029642045),(mkUTCTime (2016, 4, 17) (02, 31, 31), 0032998738)]

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
        headMay (parsedTelegram  ^.. _Just . dsmrFields . traverse . gasConsumption) `shouldBe` Just (mkUTCTime (2020, 5, 29) (14, 00, 00), 05277.053)
      
      it "produced a single telegram parsed event" $
        (length . filter (==True) . map isParsedEvent $ outputList) `shouldBe` 1

  -- hedgehog tests
  res <- hedgehogTests
  putStrLn $ "result of hedgehog tests: " ++ show res
  return ()
