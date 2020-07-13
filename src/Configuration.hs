{-# LANGUAGE OverloadedStrings #-}

module Configuration
    (
        Configuration(..)
        , loadConfig
        , SerialConfig(..)
        , CommSpeed(..)
    )
where

import Data.Aeson (eitherDecode, parseJSON, FromJSON, Value(Object), (.:), withText)
import Data.Word (Word16)
import Data.ByteString.Lazy.Char8 (pack,ByteString)
import System.Hardware.Serialport (CommSpeed(..))
import Control.Exception(try, SomeException)

data Configuration = Configuration {
    serialConfig :: SerialConfig
    --, loglevel :: LogLevel -- todo, implement proper logging
} deriving Show


data LogLevel = Debug | Warning | Info | Error | Critical deriving Show

data SerialConfig = SerialConfig {
    serialPort :: String, --"/dev/ttyUSB0" (Linux), "COM 3" (Windows)
    serialBaudRate :: CommSpeed
} deriving (Show)

instance FromJSON CommSpeed where
    parseJSON  = withText "CommSpeed" $ \case
        "CS110"    -> return CS110
        "CS300"    -> return CS300
        "CS600"    -> return CS600
        "CS1200"   -> return CS1200
        "CS2400"   -> return CS2400
        "CS4800"   -> return CS4800
        "CS9600"   -> return CS9600
        "CS19200"  -> return CS19200
        "CS38400"  -> return CS38400
        "CS57600"  -> return CS57600
        "CS115200" -> return CS115200


instance FromJSON SerialConfig where
    parseJSON (Object obj) = SerialConfig
        <$> obj .: "port"
        <*> obj .: "baudrate"
    parseJSON obj = fail $ show obj

instance FromJSON Configuration where
    parseJSON (Object obj) = Configuration 
        <$> obj .: "serial"
    parseJSON obj = fail $ show obj


config = pack "{ \"database\": { \"host\": \"db\", \"port\": 1234, \"username\": \"ledger\",  \"password\": \"ledger\", \"database_name\": \"ledger\" } }"


loadConfig :: IO (Either String Configuration)
loadConfig = do
    eitherConfigFile <- try (readFile "dsmr-metrics.cfg") :: IO (Either SomeException String)
    case eitherConfigFile of 
        Left ex -> return $ Left ("Error opening config file: " ++ show ex)
        Right configFile -> return $ eitherDecode (pack configFile)


