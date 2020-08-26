{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}

module Effects.DsmrTelegramReader (
    DsmrTelegramReader(..),
    readTelegram,
    runTelegramReaderSerial,
    runTelegramReaderFake
    ) where

import qualified Polysemy as P
import qualified Polysemy.Error as P
import Control.Exception(bracket, IOException)
import Control.Applicative((<|>))
import Control.Lens((^.))
import System.Hardware.Serialport (SerialPort, openSerial, closeSerial, recv, defaultSerialSettings, CommSpeed(..), commSpeed)
import Effects.Env(Env, getConfiguration)
import Configuration(Configuration(..), SerialConfig(..), BaudRate(..), baudRate, port)
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

readTelegramSerial :: Int -> SerialPort -> IO String
readTelegramSerial bufferSize s = readTelegramSerial' s ""
  where
    readTelegramSerial' :: SerialPort -> String -> IO String
    readTelegramSerial' s' buffer = do
      lineInputBS <- recv s' 5000
      let maxTelegramSize = bufferSize
          buffer' = buffer ++ B.unpack lineInputBS
          maybeTelegram = extractTelegram buffer'
      case maybeTelegram of
        Just r -> return r
        Nothing -> readTelegramSerial' s (lastN' maxTelegramSize buffer')

runTelegramReaderSerial :: P.Members '[Env, P.Embed IO, P.Error DsmrMetricException] r => P.Sem (DsmrTelegramReader ': r) a -> P.Sem r a
runTelegramReaderSerial = P.interpret $ \case
  ReadTelegram -> do
    cfg <- getConfiguration
    let (port', baudRate', bufferSize') = (\c -> (port c, baudRate c, bufferSize c)) $ serialConfig cfg
    let handleSerialException = P.fromExceptionVia (\(e :: IOException) -> SerialPortException (show e))
    let openSerialPort = openSerial port' defaultSerialSettings {commSpeed = toCommSpeed baudRate'} 
    
    handleSerialException $ bracket openSerialPort closeSerial (readTelegramSerial bufferSize')  -- TODO: prevent serial port from having to be opened and closed each time
{-# INLINE runTelegramReaderSerial #-}


-- BEGIN DEBUG
telegram :: String
telegram = 
    "/XMX5LGBBFFB231215493\n\
    \\n\
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
    \!7B6A\n"


runTelegramReaderFake :: P.Sem (DsmrTelegramReader ': r) a -> P.Sem r a
runTelegramReaderFake = P.interpret $ \case
  ReadTelegram -> return telegram
{-# INLINE runTelegramReaderFake #-}
-- END DEBUG

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