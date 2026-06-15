module InterpreterSpec
  ( spec,
  )
where

import Control.Exception (bracket, evaluate)
import Control.Monad.IO.Class (liftIO)
import Data.List (isInfixOf)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified GHC
import qualified GHC.IO.Handle as IO
import qualified GHC.Utils.Outputable as Outputable
import Lore.Diagnostics (Diagnostic (..))
import Lore.Interpreter (executeStatement, getTypeOfExpression)
import Lore.Session (SessionConfig (..), defaultSessionConfig)
import System.Directory (removeFile)
import System.FilePath ((</>))
import System.IO (Handle, hClose, hFlush, hPutStrLn, openTempFile, stderr, stdout)
import System.IO.Error (catchIOError)
import Test.Hspec
import TestSupport (fixtureLore, fixtureLoreAt, fixtureLoreAtWithConfig, withFixtureCopy, withFixtureSpec)

spec :: Spec
spec =
  withFixtureSpec do
    describe "interpreter" do
      it "executes statements against project modules loaded as default imports" \fixture -> do
        result <-
          fixtureLore fixture do
            executeStatement "lookupOrZero [(\"left\", 3)] \"left\""

        result `shouldBe` Right "3"

      it "uses symbols from multiple project modules without explicit imports" \fixture -> do
        result <-
          fixtureLore fixture do
            executeStatement "(crossModuleSeed, supportStep 4)"

        result `shouldBe` Right "(5,9)"

      it "returns the inferred type of an expression in the default project context" \fixture -> do
        result <-
          fixtureLore fixture do
            getTypeOfExpression "lookupOrZero [(\"left\", 3)]"

        renderType result `shouldBe` "String -> Int"

      it "returns diagnostics instead of throwing for parse failures" \fixture -> do
        result <-
          fixtureLore fixture do
            executeStatement "map (+1 [1, 2 :: Int]"

        case result of
          Left diagnostics -> do
            diagnostics `shouldSatisfy` (not . null)
            any (\diagnostic -> "parse error" `T.isInfixOf` diagnostic.diagnosticMessage) diagnostics `shouldBe` True
          Right rendered ->
            expectationFailure ("Expected parse failure, got: " <> show rendered)

      it "executes IO expressions instead of wrapping them in show" \fixture -> do
        result <-
          fixtureLore fixture do
            executeStatement "pure (3 :: Int)"

        result `shouldBe` Right "3"

      it "captures stdout produced by IO expressions" \fixture -> do
        result <-
          fixtureLore fixture do
            executeStatement "putStrLn \"123\\n345\""

        result `shouldBe` Right "123\n345"

      it "captures stderr produced by IO expressions" \fixture -> do
        result <-
          fixtureLore fixture do
            executeStatement "System.IO.hPutStrLn System.IO.stderr \"stderr output\""

        result `shouldBe` Right "stderr output"

      it "returns combined output for IO expressions that also produce a final result" \fixture -> do
        result <-
          fixtureLore fixture do
            executeStatement "print \"side\" >> pure (3 :: Int)"

        result `shouldBe` Right "\"side\"\n3"

      it "restores the previous stdout and stderr destinations after success and failure" \fixture -> do
        ((successfulResult, failedResult), hostOutput) <-
          captureHostProcessOutput $
            fixtureLore fixture do
              successfulResult <- executeStatement "putStrLn \"captured-success\""
              liftIO do
                hPutStrLn stdout "host-stdout-after-success"
                hPutStrLn stderr "host-stderr-after-success"
                hFlush stdout
                hFlush stderr
              failedResult <- executeStatement "putStrLn \"captured-before-failure\" >> fail \"expected failure\""
              liftIO do
                hPutStrLn stdout "host-stdout-after-failure"
                hPutStrLn stderr "host-stderr-after-failure"
                hFlush stdout
                hFlush stderr
              pure (successfulResult, failedResult)

        successfulResult `shouldBe` Right "captured-success"
        case failedResult of
          Left diagnostics ->
            diagnostics
              `shouldSatisfy` any (any (T.isInfixOf "captured-before-failure") . diagnosticHints)
          Right rendered ->
            expectationFailure ("Expected runtime failure, got: " <> show rendered)
        hostOutput `shouldSatisfy` isInfixOf "host-stdout-after-success"
        hostOutput `shouldSatisfy` isInfixOf "host-stderr-after-success"
        hostOutput `shouldSatisfy` isInfixOf "host-stdout-after-failure"
        hostOutput `shouldSatisfy` isInfixOf "host-stderr-after-failure"
        hostOutput `shouldSatisfy` not . isInfixOf "captured-success"
        hostOutput `shouldSatisfy` not . isInfixOf "captured-before-failure"

      it "keeps successfully loaded modules in context even when another module fails to compile" \fixture -> do
        withFixtureCopy fixture \fixtureRoot -> do
          TIO.writeFile
            (fixtureRoot </> "src" </> "Broken.hs")
            "module Broken where\n\nbrokenValue = doesNotExist\n"

          result <-
            fixtureLoreAt fixture fixtureRoot do
              executeStatement "lookupOrZero [(\"left\", 7)] \"left\""

          result `shouldBe` Right "7"

      it "can use a custom Prelude import module" \fixture -> do
        withFixtureCopy fixture \fixtureRoot -> do
          TIO.writeFile
            (fixtureRoot </> "src" </> "CustomPrelude.hs")
            "module CustomPrelude (module Prelude, nub) where\n\nimport Prelude\nimport Data.List (nub)\n"

          let sessionConfig = sessionConfigWithCustomPrelude (Just "CustomPrelude")

          result <-
            fixtureLoreAtWithConfig fixture sessionConfig fixtureRoot do
              executeStatement "nub ['a', 'a', 'b']"

          result `shouldBe` Right "\"ab\""

          ty <-
            fixtureLoreAtWithConfig fixture sessionConfig fixtureRoot do
              getTypeOfExpression "nub ['a', 'a', 'b']"

          renderType ty `shouldBe` "[Char]"

data HostProcessCapture = HostProcessCapture
  { capturePath :: FilePath,
    captureHandle :: Handle,
    originalStdout :: Handle,
    originalStderr :: Handle
  }

captureHostProcessOutput :: IO a -> IO (a, String)
captureHostProcessOutput action =
  bracket acquireHostCapture releaseHostCapture useHostCapture
  where
    acquireHostCapture = do
      (capturePath, captureHandle) <- openTempFile "/tmp" "lore-interpreter-host-output"
      originalStdout <- IO.hDuplicate stdout
      originalStderr <- IO.hDuplicate stderr
      pure HostProcessCapture {capturePath, captureHandle, originalStdout, originalStderr}

    releaseHostCapture HostProcessCapture {capturePath, captureHandle, originalStdout, originalStderr} = do
      IO.hDuplicateTo originalStdout stdout
      IO.hDuplicateTo originalStderr stderr
      ignoreIo (hClose originalStdout)
      ignoreIo (hClose originalStderr)
      ignoreIo (hClose captureHandle)
      ignoreIo (removeFile capturePath)

    useHostCapture HostProcessCapture {capturePath, captureHandle, originalStdout, originalStderr} = do
      IO.hDuplicateTo captureHandle stdout
      IO.hDuplicateTo captureHandle stderr
      result <- action
      hFlush stdout
      hFlush stderr
      IO.hDuplicateTo originalStdout stdout
      IO.hDuplicateTo originalStderr stderr
      hClose captureHandle
      capturedOutput <- readFile capturePath
      _ <- evaluate (length capturedOutput)
      pure (result, capturedOutput)

ignoreIo :: IO () -> IO ()
ignoreIo ioAction =
  catchIOError ioAction (const (pure ()))

sessionConfigWithCustomPrelude :: Maybe T.Text -> SessionConfig
sessionConfigWithCustomPrelude customPrelude =
  defaultSessionConfig
    { customPrelude = customPrelude
    }

renderType :: GHC.Type -> String
renderType =
  Outputable.showSDocUnsafe . Outputable.ppr
