module DsmrTelegramParser
    ( DsmrTelegram (..), 
      DsmrField (..), 
      runDsmrParser
    ) where

import DsmrTelegramParser.Internal
import Model.DsmrTelegram