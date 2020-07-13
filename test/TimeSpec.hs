{-# LANGUAGE OverloadedStrings #-}

module TimeSpec 
    ( testTime
    , genUTCTime
    ) 
where

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hedgehog (tripping, checkParallel, forAll, property, Gen, Property, Group(..), Range)
import Data.Time.Clock(UTCTime(..), secondsToDiffTime)
import Data.Time.Calendar(fromGregorian)
import Data.Time.LocalTime(TimeZone(..))
import Util.Time(localTimeStampToUTC, utcToLocalTimeStamp)

genUTCTime :: Gen UTCTime
genUTCTime = do
    y <- toInteger <$> Gen.int (Range.constant 2000 2099) -- Serialized timestamps can only represent the current century, so this will start failing in 2100
    m <- Gen.int (Range.constant 1 12)
    d <- Gen.int (Range.constant 1 28)
    let day = fromGregorian y m d
    secs <- toInteger <$> Gen.int (Range.constant 0 86400)
    let diff = secondsToDiffTime secs
    pure $ UTCTime day diff
    
propTripTime :: Property
propTripTime = property $ do
    let tz = TimeZone 120 True "TTZ"
    t <- forAll genUTCTime
    tripping t (utcToLocalTimeStamp tz) (localTimeStampToUTC tz 20)

testTime :: IO ()
testTime = do
    res <- checkParallel $ Group "Hedgehog time tests" [ ("prop_tripping_time", propTripTime) ]
    putStrLn $ "Time test result: " ++ show res
    return ()