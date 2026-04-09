module Lore.Logger
  ( LoggerHandle (..),
    LogLevel (..),
    LogMessage (..),
    MonadLogger (..),
    debug,
    info,
    warn,
    err,
    prettyLoggerHandle,
    noLogHandle,
  )
where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import System.IO (hPutStrLn, stderr)

newtype LoggerHandle = LoggerHandle
  { putLog :: LogMessage -> IO ()
  }

data LogLevel
  = Debug
  | Info
  | Warning
  | Error
  deriving (Eq, Ord, Show)

data LogMessage = LogMessage
  { timestamp :: UTCTime,
    level :: LogLevel,
    content :: String
  }

class (MonadIO m) => MonadLogger m where
  getLoggerHandle :: m LoggerHandle

logMsg :: (MonadLogger m) => LogLevel -> String -> m ()
logMsg level content = do
  LoggerHandle putLog <- getLoggerHandle
  timestamp <- liftIO getCurrentTime
  let logMessage = LogMessage {timestamp, level, content}
  liftIO $ putLog logMessage

debug :: (MonadLogger m) => String -> m ()
debug = logMsg Debug

info :: (MonadLogger m) => String -> m ()
info = logMsg Info

warn :: (MonadLogger m) => String -> m ()
warn = logMsg Warning

err :: (MonadLogger m) => String -> m ()
err = logMsg Error

prettyLoggerHandle :: LogLevel -> LoggerHandle
prettyLoggerHandle minimumLevel = LoggerHandle $ \msg -> do
  if msg.level >= minimumLevel
    then hPutStrLn stderr (formatLog msg)
    else pure ()
  where
    formatLog LogMessage {timestamp, level, content} =
      let severityStr = case level of
            Debug -> "[Debug]    "
            Info -> "[Info]     "
            Warning -> "[Warning]  "
            Error -> "[Error]    "
          timestampStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S%3Q " timestamp
       in timestampStr <> severityStr <> content

noLogHandle :: LoggerHandle
noLogHandle = LoggerHandle $ \_ -> pure ()
