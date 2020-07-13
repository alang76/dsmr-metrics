module ParserSpec where

import Test.Hspec (shouldBe, it, describe, hspec)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Hedgehog as H
import qualified Test.Hspec.Hedgehog as H
import Data.Maybe (isJust)
import Data.Function ((&))
import Data.Time.LocalTime(TimeZone(..))
import Data.Time.Clock(UTCTime(..), secondsToDiffTime)
import Data.Time.Calendar(fromGregorian)
import Control.Lens ((^..))
import Control.Lens.Combinators (_Just)
import Safe (headMay)
import qualified Effects.DsmrTelegramReader as DTR
import qualified Polysemy as P
import qualified Polysemy.Output as P
import DsmrMetricsReader.Model
import DsmrMetricsReader.Internal
import Effects.Env(Env(..))
import Util.Time(mkUTCTime)
import TelegramBuilder(buildTelegram, serializeTelegram)


maxValueInt :: Int
maxValueInt = maxBound

maxValueInteger :: Integer
maxValueInteger = 9999999999999999999999999999

maxValueDouble :: Double
maxValueDouble = 9999999999999999999999999999

genStrVar :: Int -> H.Gen String
genStrVar l = Gen.string (Range.linear 0 l) Gen.alphaNum

genStrConst :: Int -> H.Gen String
genStrConst l = Gen.string (Range.constant l l) Gen.alphaNum

genDouble :: H.Gen Double
genDouble = Gen.double (Range.linearFrac 0 maxValueDouble)

genInteger :: H.Gen Integer
genInteger = Gen.integral (Range.linear 0 maxValueInteger)

genInt :: H.Gen Int
genInt = Gen.int ((Range.linear 0 maxValueInt) :: H.Range Int)

genUTCTime :: H.Gen UTCTime
genUTCTime = do
    y <- toInteger <$> Gen.int (Range.constant 2000 2099) -- Serialized timestamps can only represent the current century, so this will start failing in 2100
    m <- Gen.int (Range.constant 1 12)
    d <- Gen.int (Range.constant 1 28)
    let day = fromGregorian y m d
    secs <- toInteger <$> Gen.int (Range.constant 0 86401)
    let diff = secondsToDiffTime secs
    pure $ UTCTime day diff

genStampedValue:: H.Gen a -> H.Gen (UTCTime, a)
genStampedValue genValue = do
  ts <- genUTCTime
  v <- genValue
  return (ts, v)

genPowerFailureLog :: H.Gen [(UTCTime, Integer)]
genPowerFailureLog = Gen.list (Range.linear 0 10000) (genStampedValue genInteger)

genTelegram :: H.Gen DsmrTelegram
genTelegram = do
  let
  meterIdGen                          <- genStrVar 96
  versionNumberGen                    <- genInteger
  timeStampGen                        <- genUTCTime
  equipmentIDGen                      <- genStrVar 96
  energyConsumedTariff1Gen            <- genDouble
  energyConsumedTariff2Gen            <- genDouble
  energyReturnedTariff1Gen            <- genDouble
  energyReturnedTariff2Gen            <- genDouble
  actualTariffIndicatorGen            <- genInt
  actualPowerConsumptionGen           <- genDouble
  actualPowerReturnedGen              <- genDouble
  numberOfPowerFailuresGen            <- genInteger
  numberOfLongPowerFailuresGen        <- genInteger
  powerFailureLogGen                  <- genPowerFailureLog
  numberOfVoltageSagsPhaseL1Gen       <- genInteger
  numberOfVoltageSagsPhaseL2Gen       <- genInteger
  actualCurrentConsumptionGen         <- genInteger
  actualPowerConsumptionPhaseL1Gen    <- genDouble
  actualPowerReturnedPhaseL1Gen       <- genDouble
  slaveGasMeterDeviceTypeGen          <- genInt
  gasMeterSerialNumberGen             <- genStrVar 96
  gasConsumptionGen                   <- genStampedValue genDouble
  checkSumGen                         <- genInteger
  return $ buildTelegram
    meterIdGen
    versionNumberGen
    timeStampGen
    equipmentIDGen
    energyConsumedTariff1Gen
    energyConsumedTariff2Gen
    energyReturnedTariff1Gen
    energyReturnedTariff2Gen
    actualTariffIndicatorGen
    actualPowerConsumptionGen
    actualPowerReturnedGen
    numberOfPowerFailuresGen
    numberOfLongPowerFailuresGen
    powerFailureLogGen
    numberOfVoltageSagsPhaseL1Gen
    numberOfVoltageSagsPhaseL2Gen
    actualCurrentConsumptionGen
    actualPowerConsumptionPhaseL1Gen
    actualPowerReturnedPhaseL1Gen
    slaveGasMeterDeviceTypeGen
    gasMeterSerialNumberGen
    gasConsumptionGen
    checkSumGen


propTripTelegram :: H.PropertyT IO ()
propTripTelegram = do
  t <- H.forAll genTelegram
  H.tripping t prettyTelegram parseTelegram

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


-- TODO move to a central location and remove duplication
runEnvPureTest :: P.Sem (Env ': r) a -> P.Sem r a
runEnvPureTest = P.interpret $ s\case
  GetEnvironmentTimeZone -> return $ TimeZone 120 True "TTZ"
  GetEnvironmentTimeCentury -> return 20

runParser :: (P.Members '[DTR.DsmrTelegramReader, P.Output String, Env] r) => P.Sem r (Maybe DsmrTelegram)
runParser = do
  telegram <- DTR.readTelegram
  runDsmrParser telegram

testParser :: IO ()
testParser = do
  parsedTelegram <-
      runParser
        & runEnvPureTest
        & DTR.runTelegramReaderFakePure
        & P.runOutputSem (P.embed . putStrLn)
        & P.runM

  hspec $
    describe "DsmrTelegramParser" $ do
    it "telegram can be parsed succesfully" $
      isJust parsedTelegram `shouldBe` True

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

    -- property based tests
    it "roundtripping telegram parser works as expected" $
      H.hedgehog $ propTripTelegram




