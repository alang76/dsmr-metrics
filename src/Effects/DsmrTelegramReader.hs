{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}

module Effects.DsmrTelegramReader (
    DsmrTelegramReader(..),
    readTelegram,
    runTelegramReaderSerial,
    ) where

import qualified Polysemy as P
import qualified Polysemy.Error as P
import Control.Exception(bracket, IOException)
import Control.Applicative((<|>))
import System.Hardware.Serialport (SerialPort, openSerial, closeSerial, recv, defaultSerialSettings, CommSpeed(..), commSpeed)
import Effects.Env(Env, getConfiguration)
import Configuration(Configuration(..), SerialConfig(..), BaudRate(..), serialBaudRate, serialPort)
import qualified Data.ByteString.Char8 as B
import Data.Void (Void)
import Data.List(foldl')
import Text.Megaparsec(Parsec, try, parse, some, anySingle, manyTill_, count)
import Text.Megaparsec.Char(char, eol, digitChar, upperChar)
import Exceptions.DsmrMetricException(DsmrMetricException(..))

data DsmrTelegramReader m a where
  ReadTelegram :: DsmrTelegramReader m String

P.makeSem ''DsmrTelegramReader

type Parser = Parsec Void String

class CharOrStr a where
  toStr :: a -> String

instance CharOrStr Char where
  toStr x = [x]

instance CharOrStr String where
  toStr = id

infixl 4 <++>

(<++>) :: (CharOrStr a, CharOrStr b) => Parser a -> Parser b -> Parser String
f <++> g = (\x y -> toStr x ++ toStr y) <$> f <*> g

lastN' :: Int -> [a] -> [a]
lastN' n xs = foldl' (const . drop 1) xs (drop n xs)

beginMarkerP :: Parser String
beginMarkerP = char '/' <++> some (upperChar <|> digitChar) <++> eol

endMarkerP :: Parser String
endMarkerP = char '!' <++> count 4 (upperChar <|> digitChar) <++> eol

parseTelegramP :: Parser String
parseTelegramP = do
  (_, beginMarker) <- manyTill_ anySingle beginMarkerP
  (telegram, endMarker) <- manyTill_ anySingle (try endMarkerP)
  return $ beginMarker++telegram++endMarker

extractTelegram :: String -> Maybe String
extractTelegram telegram =
  case parse parseTelegramP "" telegram of
    Left _ -> Nothing
    Right t -> Just t

readTelegramSerial :: SerialPort -> IO String
readTelegramSerial s = readTelegramSerial' s ""
  where
    readTelegramSerial' :: SerialPort -> String -> IO String
    readTelegramSerial' s' buffer = do
      lineInputBS <- recv s' 5000
      let maxTelegramSize = 40000 --TODO: make configurable
          buffer' = buffer ++ B.unpack lineInputBS
          maybeTelegram = extractTelegram buffer'
      case maybeTelegram of
        Just r -> return r
        Nothing -> readTelegramSerial' s (lastN' maxTelegramSize buffer')

runTelegramReaderSerial :: P.Members '[Env, P.Embed IO, P.Error DsmrMetricException] r => P.Sem (DsmrTelegramReader ': r) a -> P.Sem r a
runTelegramReaderSerial = P.interpret $ \case
  ReadTelegram -> do
    cfg <- getConfiguration
    let serCfg = serialConfig cfg
    let (port, baudRate) = (\c -> (serialPort c, serialBaudRate c)) serCfg
    let handleSerialException = P.fromExceptionVia (\(e :: IOException) -> SerialPortException (show e))
    let openSerialPort = openSerial port defaultSerialSettings {commSpeed = toCommSpeed(baudRate)} 
    handleSerialException $ bracket openSerialPort closeSerial readTelegramSerial  -- TODO: prevent serial port from having to be opened and closed each time
{-# INLINE runTelegramReaderSerial #-}

toCommSpeed :: BaudRate -> CommSpeed
toCommSpeed = \case
        BR110    ->  CS110
        BR300    ->  CS300
        BR600    ->  CS600
        BR1200   ->  CS1200
        BR2400   ->  CS2400
        BR4800   ->  CS4800
        BR9600   ->  CS9600
        BR19200  ->  CS19200
        BR38400  ->  CS38400
        BR57600  ->  CS57600
        BR115200 ->  CS115200