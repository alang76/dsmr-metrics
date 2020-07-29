module Events.DsmrMetricEvent
  (   DsmrMetricEvent(..)
    , ThreadID
  )
where

import Model.DsmrTelegram(DsmrTelegram)

type ThreadID = Int

data DsmrMetricEvent
  = ProgramStarted
  | DsmrTelegramReaderThreadStarted ThreadID
  | MetricsWebServerThreadStarted ThreadID
  | ThreadTerminated ThreadID
  | DsmrTelegramReceived String
  | DsmrTelegramParseError String
  | DsmrTelegramParsed DsmrTelegram
  | InvalidConfigurationDetected String
  | FatalExceptionDetected String
  | CheckPoint String
  | ProgramTerminated
  deriving Eq


instance Show DsmrMetricEvent where
  show ProgramStarted = "Program started"
  show (DsmrTelegramReaderThreadStarted threadID) = "Started DSMR reading thread, thread id: " ++ show threadID
  show (MetricsWebServerThreadStarted threadID) = "Started metrics serving thread, thread id: " ++ show threadID
  show (ThreadTerminated threadID) = "Thread was terminated, thread id: " ++ show threadID
  show (DsmrTelegramReceived dsmrTelegramStr) = "Received a telegram: " ++ dsmrTelegramStr
  show (DsmrTelegramParseError parseErrorStr) = "Failed to parse telegram input: " ++ parseErrorStr
  show (DsmrTelegramParsed dsmrTelegram) = "Succesfully parsed a telegram: " ++ show dsmrTelegram
  show (InvalidConfigurationDetected str) = "Invalid configuration: " ++ show str
  show (FatalExceptionDetected str) = "Fatal exception: " ++ str
  show (CheckPoint str) = "Checkpoint: " ++ str
  show ProgramTerminated = "Program terminated"
