module TestEffects.DsmrTelegramReader
    ( runTelegramReaderFakePure
    )
where

import qualified Polysemy as P
import Effects.DsmrTelegramReader
import TelegramBuilder(telegram)

runTelegramReaderFakePure :: P.Sem (DsmrTelegramReader ': r) a  -> P.Sem r a
runTelegramReaderFakePure = P.interpret $ \case
  ReadTelegram -> return telegram