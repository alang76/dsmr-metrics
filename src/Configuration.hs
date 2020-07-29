{-# LANGUAGE OverloadedStrings #-}

module Configuration
    (
        Configuration(..)
        , loadConfig
        , SerialConfig(..)
        , BaudRate(..)
        , ConfigurationException(..)
        , WebServerConfig(..)
    )
where


import Data.Aeson (eitherDecode, parseJSON, FromJSON, Value(Object), (.:), withText)
import Data.ByteString.Lazy.Char8 (pack, ByteString)
import Control.Exception(Exception, SomeException, try, throw)
import Data.Text(unpack)
import qualified Polysemy as P
import qualified Polysemy.Error as P
import Exceptions.DsmrMetricException(DsmrMetricException)

data ConfigurationException = ConfigurationException String deriving Show

instance Exception ConfigurationException

data Configuration = Configuration {
    serialConfig :: SerialConfig,
    webServerConfig :: WebServerConfig
} deriving Show


data LogLevel = Debug | Warning | Info | Error | Critical deriving Show

data SerialConfig = SerialConfig {
    serialPort :: String, --"/dev/ttyUSB0" (Linux), "COM 3" (Windows)
    serialBaudRate :: BaudRate
} deriving (Show)

data WebServerConfig = WebServerConfig {
    listenPort :: Int
} deriving (Show)

data BaudRate = 
      BR110
    | BR300
    | BR600
    | BR1200
    | BR2400
    | BR4800
    | BR9600
    | BR19200
    | BR38400
    | BR57600
    | BR115200
    deriving (Show, Eq)

instance FromJSON BaudRate where
    parseJSON  = withText "BaudRate" $ \case
        "110"    -> return BR110
        "300"    -> return BR300
        "600"    -> return BR600
        "1200"   -> return BR1200
        "2400"   -> return BR2400
        "4800"   -> return BR4800
        "9600"   -> return BR9600
        "19200"  -> return BR19200
        "38400"  -> return BR38400
        "57600"  -> return BR57600
        "115200" -> return BR115200
        x -> fail $ "Error parsing baudrate config value, got '" ++ unpack x ++ "' which is not one of the allowed values ('110', '300', '600', '1200', '2400', '4800', '9600', '19200', '38400', '57600' or '11520')."


instance FromJSON WebServerConfig where
    parseJSON (Object obj) = WebServerConfig 
        <$> obj .: "listenPort"
    parseJSON obj = fail $ show obj

instance FromJSON SerialConfig where
    parseJSON (Object obj) = SerialConfig
        <$> obj .: "port"
        <*> obj .: "baudrate"
    parseJSON obj = fail $ show obj

instance FromJSON Configuration where
    parseJSON (Object obj) = Configuration 
        <$> obj .: "serial"
        <*> obj .: "webServer"
    parseJSON obj = fail $ show obj


loadConfig :: IO (Either String Configuration)
loadConfig = do
    eitherConfigFile <- try (readFile "dsmr-metrics.cfg") :: IO (Either SomeException String)
    case eitherConfigFile of 
        Left ex -> return $ Left ("Error opening config file: " ++ show ex)
        Right configFile -> return $ eitherDecode (pack configFile)


