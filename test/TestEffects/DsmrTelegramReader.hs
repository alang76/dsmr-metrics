module TestEffects.DsmrTelegramReader
    ( runTelegramReaderFakePure
    )
where

import qualified Polysemy as P
import Effects.DsmrTelegramReader
import TelegramBuilder(telegram)
import Effects.Env(Env, getEnvironmentConfiguration)

runTelegramReaderFakePure :: P.Member Env r => P.Sem (DsmrTelegramReader ': r) a  -> P.Sem r a
runTelegramReaderFakePure = P.interpret $ \case
  ReadTelegram -> do
    config <- getEnvironmentConfiguration -- fake use of config
    case config of
      Left _ -> return telegram
      Right _ -> return telegram
