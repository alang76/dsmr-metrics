import Test.Hspec (shouldThrow, shouldBe, anyException, it, describe, hspec)
import Test.QuickCheck (property)
import Control.Exception (evaluate)
import Data.Maybe (isJust)
import Data.Void (Void)
import Control.Lens --((&), (^..), (^.), Lens')
import Control.Lens.Getter
import Control.Lens.Prism (_Right, _Just)
import Text.Megaparsec (ParseErrorBundle)
import DsmrMetricsReader.Internal
import Safe (headMay)


testTelegram :: String
testTelegram = 
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


  


{-
Meter ID: LGBBFFB231215493
1-3:0.2.8(42) <- Version number (4.2)
0-0:1.0.0(200529163319S) <- Timestamp of the message in TST (YYMMDDhhmmssX)  (DST active (summer time): X=S, DST not active (winter time): X=W)
0-0:96.1.1(4530303034303031353934373534343134) <- Equipment identifier
1-0:1.8.1(014765.683*kWh) <- Meter Reading electricity delivered *to* client (Tariff 1) in 0,001 kWh
1-0:2.8.1(000000.000*kWh) <- Meter Reading electricity delivered *by* client (Tariff 1) in 0,001 kWh
1-0:1.8.2(014628.043*kWh) <- Meter Reading electricity delivered *to* client (Tariff 2) in 0,001 kWh
1-0:2.8.2(000000.000*kWh) <- Meter Reading electricity delivered *by* client (Tariff 2) in 0,001 kWh
0-0:96.14.0(0002) <- Current tariff indicator electricity.
1-0:1.7.0(03.877*kW) <- Actual electricity power consumed by client  (+P) in 1 Watt resolution (all phases)
1-0:2.7.0(00.000*kW) <- Actual electricity power returned to net (-P) in 1 Watt resolution (all phases)
0-0:96.7.21(00003) <- Number of power failures in any phase
0-0:96.7.9(00002) <- Number of long power failures in any phase
1-0:99.97.0(2)(0-0:96.7.19)(170326062519S)(0029642045*s)(160417043131S)(0032998738*s) <- Power Failure Event Log (long power failures), Format: Timestamp (YYMMDDhhmmssX), duration in seconds.. out for a year in 2016 and 2017?? weird..
1-0:32.32.0(00000) <- Number of voltage sags in phase L1
1-0:32.36.0(00000) <- Number of voltage sags in phase L2
0-0:96.13.1() <- ??
0-0:96.13.0() <- ??
1-0:31.7.0(017*A) <- Actual current consumption
1-0:21.7.0(03.877*kW) <- Actual electricity power consumed by client (+P) in 1 Watt resolution (phase L1)
1-0:22.7.0(00.000*kW) <- Actual electricity power returned to net (+P) in 1 Watt resolution (phase L1)
0-1:24.1.0(003) <- Slave (Gas meter) device type
0-1:96.1.0(4730303137353931323139313130333134) <-  Gas meter serial number
0-1:24.2.1(200529160000S)(05277.053*m3) <- Gas meter time stamp + value
!7667 <- CRC16 checksum of entire telegram (from / to !)
-}

main :: IO ()
main = do
  parsedTelegram <- runDsmrParserPrintError testTelegram

  hspec $
    describe "DsmrTelegramParser" $ do
    it "telegram can be parsed succesfully" $
      isJust parsedTelegram `shouldBe` True

    it "parsed telegram contains expected version header" $
      (headMay $ parsedTelegram  ^.. _Just . dsmrFields . traverse . versionNumber) `shouldBe` Just (42 :: Integer)

    it "parsed telegram contains expected timestamp" $
      (headMay $ parsedTelegram  ^.. _Just . dsmrFields . traverse . timeStamp) `shouldBe` Just (mkUTCTime (2020, 5, 29) (18, 33, 19))

    it "parsed telegram contains expected EquipmentID" $
       (headMay $ parsedTelegram  ^.. _Just . dsmrFields . traverse . equipmentID)  `shouldBe` (Just "4530303034303031353934373534343134")

    it "parsed telegram contains expected EnergyConsumedTariff1" $
      (headMay $ parsedTelegram  ^.. _Just . dsmrFields . traverse . energyConsumedTariff1) `shouldBe` (Just 014765.683)

    it "parsed telegram contains expected EnergyConsumedTariff2" $
      (headMay $ parsedTelegram  ^.. _Just . dsmrFields . traverse . energyConsumedTariff2) `shouldBe` (Just 0)

    it "parsed telegram contains expected EnergyReturnedTariff1" $
      (headMay $ parsedTelegram  ^.. _Just . dsmrFields . traverse . energyReturnedTariff1) `shouldBe` (Just 014628.043)

    it "parsed telegram contains expected EnergyReturnedTariff2" $
      (headMay $ parsedTelegram  ^.. _Just . dsmrFields . traverse . energyReturnedTariff2) `shouldBe` (Just 0)

    it "parsed telegram contains expected ActualTariffIndicator" $
      (headMay $ parsedTelegram  ^.. _Just . dsmrFields . traverse . actualTariffIndicator) `shouldBe` (Just 2)

    it "parsed telegram contains expected ActualPowerConsumption" $
      (headMay $ parsedTelegram  ^.. _Just . dsmrFields . traverse . actualPowerConsumption) `shouldBe` (Just 03.877)

    it "parsed telegram contains expected ActualPowerReturned" $
      (headMay $ parsedTelegram  ^.. _Just . dsmrFields . traverse . actualPowerReturned) `shouldBe` (Just (0 :: Double))
    
    it "parsed telegram contains expected NumberOfPowerFailures" $
      (headMay $ parsedTelegram  ^.. _Just . dsmrFields . traverse . numberOfPowerFailures) `shouldBe` (Just 3)

    it "parsed telegram contains expected numberOfLongPowerFailures" $
      (headMay $ parsedTelegram  ^.. _Just . dsmrFields . traverse . numberOfLongPowerFailures) `shouldBe` (Just 2)

    it "parsed telegram contains expected powerFailureLog" $
      (headMay $ parsedTelegram  ^.. _Just . dsmrFields . traverse . powerFailureLog) `shouldBe` Just [(mkUTCTime (2017, 3, 26) (08, 25, 19), 0029642045),(mkUTCTime (2016, 4, 17) (06, 31, 31), 0032998738)]

    it "parsed telegram contains expected numberOfVoltageSagsPhaseL1" $
      (headMay $ parsedTelegram  ^.. _Just . dsmrFields . traverse . numberOfVoltageSagsPhaseL1) `shouldBe` (Just 0)

    it "parsed telegram contains expected numberOfVoltageSagsPhaseL2" $
      (headMay $ parsedTelegram  ^.. _Just . dsmrFields . traverse . numberOfVoltageSagsPhaseL2) `shouldBe` (Just 0)

    it "parsed telegram contains expected actualCurrentConsumption" $
      (headMay $ parsedTelegram  ^.. _Just . dsmrFields . traverse . actualCurrentConsumption) `shouldBe` (Just 17)

    it "parsed telegram contains expected actualPowerConsumptionPhaseL1" $
      (headMay $ parsedTelegram  ^.. _Just . dsmrFields . traverse . actualPowerConsumptionPhaseL1) `shouldBe` (Just 03.877)

    it "parsed telegram contains expected actualPowerReturnedPhaseL1" $
      (headMay $ parsedTelegram  ^.. _Just . dsmrFields . traverse . actualPowerReturnedPhaseL1) `shouldBe` (Just 0)

    it "parsed telegram contains expected slaveGasMeterDeviceType" $
      (headMay $ parsedTelegram  ^.. _Just . dsmrFields . traverse . slaveGasMeterDeviceType) `shouldBe` (Just 3)

    it "parsed telegram contains expected gasMeterSerialNumber" $
      (headMay $ parsedTelegram  ^.. _Just . dsmrFields . traverse . gasMeterSerialNumber) `shouldBe` (Just "4730303137353931323139313130333134")

    it "parsed telegram contains expected gasConsumption" $
      (headMay $ parsedTelegram  ^.. _Just . dsmrFields . traverse . gasConsumption) `shouldBe` (Just (mkUTCTime (2020, 5, 29) (18, 00, 00), 05277.053))

