module Effects.DsmrTelegramReader (
    DsmrTelegramReader(..),
    readTelegram,
    runTelegramReaderSerial,
    ) where

import Data.Function((&))
import Data.Either(fromRight)
import Control.Concurrent (threadDelay)
import Polysemy as P
import qualified Data.ByteString.Char8 as B
import System.Hardware.Serialport (openSerial, closeSerial, recv, defaultSerialSettings, CommSpeed(..), commSpeed)
import Effects.Env(Env, getEnvironmentConfiguration)
import Configuration(Configuration(..), SerialConfig(..), serialBaudRate, serialPort)

data DsmrTelegramReader m a where
  ReadTelegram :: DsmrTelegramReader m String

P.makeSem ''DsmrTelegramReader

readSerial :: String -> CommSpeed -> IO String
readSerial port baudRate = do
 s <- openSerial port defaultSerialSettings { commSpeed = baudRate }
 lineInput <- recv s 10000
 closeSerial s
 return $ B.unpack lineInput

runTelegramReaderSerial :: P.Members '[Env, P.Embed IO] r => P.Sem (DsmrTelegramReader ': r) a -> P.Sem r a
runTelegramReaderSerial = P.interpret $ \case 
  ReadTelegram -> do
    eitherCfg <- getEnvironmentConfiguration
    case eitherCfg of
      Left str -> error $ "Failed to get configuration from environment: " ++ str
      Right (cfg :: Configuration) -> do
        let serCfg = serialConfig cfg
        let (port, baudRate) = (\c -> (serialPort c, serialBaudRate c)) $ serCfg
        P.embed $ readSerial port baudRate


