module LoggerSpec
  ( spec,
  )
where

import Control.Exception (bracket, evaluate)
import Data.List (isInfixOf)
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import qualified GHC.IO.Handle as IO
import Lore.Logger (LogLevel (..), LogMessage (..), prettyLoggerHandle, putLog)
import Lore.Session (SessionConfig (..), defaultSessionConfig)
import System.Directory (removeFile)
import System.IO (hClose, hFlush, openTempFile, stderr)
import System.IO.Error (catchIOError)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec =
  describe "logger" do
    it "writes messages at or above the configured level to stderr" do
      stderrOutput <-
        captureStderr do
          let loggerHandle = prettyLoggerHandle Warning
          putLog loggerHandle infoMessage
          putLog loggerHandle warningMessage

      stderrOutput `shouldSatisfy` isInfixOf "warning message"
      stderrOutput `shouldSatisfy` not . isInfixOf "info message"

    it "does not print logs by default" do
      stderrOutput <-
        captureStderr do
          putLog defaultSessionConfig.loggerHandle warningMessage

      stderrOutput `shouldBe` ""

infoMessage :: LogMessage
infoMessage =
  LogMessage
    { timestamp = sampleTimestamp,
      level = Info,
      content = "info message"
    }

warningMessage :: LogMessage
warningMessage =
  LogMessage
    { timestamp = sampleTimestamp,
      level = Warning,
      content = "warning message"
    }

sampleTimestamp :: UTCTime
sampleTimestamp =
  UTCTime
    { utctDay = fromGregorian 2026 1 1,
      utctDayTime = secondsToDiffTime 0
    }

captureStderr :: IO () -> IO String
captureStderr action =
  bracket acquireCapture releaseCapture useCapture
  where
    acquireCapture = do
      (capturePath, captureHandle) <- openTempFile "/tmp" "lore-logger-stderr"
      originalStderr <- IO.hDuplicate stderr
      pure (capturePath, captureHandle, originalStderr)

    releaseCapture (capturePath, captureHandle, originalStderr) = do
      IO.hDuplicateTo originalStderr stderr
      ignoreIo (hClose originalStderr)
      ignoreIo (hClose captureHandle)
      ignoreIo (removeFile capturePath)

    useCapture (capturePath, captureHandle, originalStderr) = do
      IO.hDuplicateTo captureHandle stderr
      action
      hFlush stderr
      IO.hDuplicateTo originalStderr stderr
      hClose captureHandle
      stderrOutput <- readFile capturePath
      _ <- evaluate (length stderrOutput)
      pure stderrOutput

ignoreIo :: IO () -> IO ()
ignoreIo ioAction =
  catchIOError ioAction (const (pure ()))
