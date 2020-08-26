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
  , DsmrField(..)
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

getStrCheckSum :: Int -> Gen String
getStrCheckSum l = Gen.string (Range.constant l l) (Gen.choice [Gen.upper, Gen.digit])

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
  <*> genDouble                 -- energyReturnedTariff1Gen
  <*> genDouble                 -- energyConsumedTariff2Gen
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
  <*> getStrCheckSum 4          -- checkSumGen

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



getFields ::  Applicative f => (DsmrField -> f DsmrField) -> Maybe DsmrTelegram -> f (Maybe DsmrTelegram)
getFields = _Just . dsmrFields . traverse

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

      it "parsed telegram contains expected version header" $
        headMay (parsedTelegram  ^.. getFields . versionNumber) `shouldBe` Just (42 :: Integer)

      it "parsed telegram contains expected timestamp" $
        headMay (parsedTelegram  ^.. getFields . timeStamp) `shouldBe` Just (mkUTCTime (2020, 5, 29) (14, 33, 19) :: UTCTime)

      it "parsed telegram contains expected EquipmentID" $
        headMay (parsedTelegram  ^.. getFields . equipmentID)  `shouldBe` Just "4530303034303031353934373534343134"

      it "parsed telegram contains expected EnergyConsumedTariff1" $
        headMay (parsedTelegram  ^.. getFields . energyConsumedTariff1) `shouldBe` Just 014765.683

      it "parsed telegram contains expected EnergyConsumedTariff2" $
        headMay (parsedTelegram  ^.. getFields . energyConsumedTariff2) `shouldBe` Just 014628.043

      it "parsed telegram contains expected EnergyReturnedTariff1" $
        headMay (parsedTelegram  ^.. getFields . energyReturnedTariff1) `shouldBe` Just 0

      it "parsed telegram contains expected EnergyReturnedTariff2" $
        headMay (parsedTelegram  ^.. getFields . energyReturnedTariff2) `shouldBe` Just 0

      it "parsed telegram contains expected ActualTariffIndicator" $
        headMay (parsedTelegram  ^.. getFields . actualTariffIndicator) `shouldBe` Just 2

      it "parsed telegram contains expected ActualPowerConsumption" $
        headMay (parsedTelegram  ^.. getFields . actualPowerConsumption) `shouldBe` Just 03.877

      it "parsed telegram contains expected ActualPowerReturned" $
        headMay (parsedTelegram  ^.. getFields . actualPowerReturned) `shouldBe` Just (0 :: Double)

      it "parsed telegram contains expected NumberOfPowerFailures" $
        headMay (parsedTelegram  ^.. getFields . numberOfPowerFailures) `shouldBe` Just 3

      it "parsed telegram contains expected numberOfLongPowerFailures" $
        headMay (parsedTelegram  ^.. getFields . numberOfLongPowerFailures) `shouldBe` Just 2

      it "parsed telegram contains expected powerFailureLog" $
        headMay (parsedTelegram  ^.. getFields . powerFailureLog) `shouldBe` Just [(mkUTCTime (2017, 3, 26) (04, 25, 19), 0029642045),(mkUTCTime (2016, 4, 17) (02, 31, 31), 0032998738)]

      it "parsed telegram contains expected numberOfVoltageSagsPhaseL1" $
        headMay (parsedTelegram  ^.. getFields . numberOfVoltageSagsPhaseL1) `shouldBe` Just 0

      it "parsed telegram contains expected numberOfVoltageSagsPhaseL2" $
        headMay (parsedTelegram  ^.. getFields . numberOfVoltageSagsPhaseL2) `shouldBe` Just 0

      it "parsed telegram contains expected actualCurrentConsumption" $
        headMay (parsedTelegram  ^.. getFields . actualCurrentConsumption) `shouldBe` Just 17

      it "parsed telegram contains expected actualPowerConsumptionPhaseL1" $
        headMay (parsedTelegram  ^.. getFields . actualPowerConsumptionPhaseL1) `shouldBe` Just 03.877

      it "parsed telegram contains expected actualPowerReturnedPhaseL1" $
        headMay (parsedTelegram  ^.. getFields . actualPowerReturnedPhaseL1) `shouldBe` Just 0

      it "parsed telegram contains expected slaveGasMeterDeviceType" $
        headMay (parsedTelegram  ^.. getFields . slaveGasMeterDeviceType) `shouldBe` Just 3

      it "parsed telegram contains expected gasMeterSerialNumber" $
        headMay (parsedTelegram  ^.. getFields . gasMeterSerialNumber) `shouldBe` Just "4730303137353931323139313130333134"

      it "parsed telegram contains expected gasConsumption" $
        headMay (parsedTelegram  ^.. getFields . gasConsumption) `shouldBe` Just (mkUTCTime (2020, 5, 29) (14, 00, 00), 05277.053)

      it "produced a single telegram parsed event" $
        (length . filter (==True) . map isParsedEvent $ outputList) `shouldBe` 1

      -- TODO: ensure non-parseable data is handled correctly

  -- hedgehog tests
  res <- hedgehogTests
  putStrLn $ "result of hedgehog tests: " ++ show res
  return ()
