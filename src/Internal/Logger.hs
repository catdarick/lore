module Internal.Logger where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime)

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

loggerHandle'Pretty :: LoggerHandle
loggerHandle'Pretty = LoggerHandle $ \msg -> do
  putStrLn (formatLog msg)
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
