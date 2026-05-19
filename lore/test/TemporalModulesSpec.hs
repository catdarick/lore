module TemporalModulesSpec (spec) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Lore (createTemporalModule)
import Lore.Diagnostics (Diagnostic (..))
import Lore.HomeModules (LoadHomeModulesResult (..), defaultLoadHomeModulesOptions, loadHomeModules)
import Lore.Interpreter (executeStatement)
import System.Directory (doesFileExist, removeFile)
import System.FilePath ((</>))
import Test.Hspec
import TestSupport (fixtureLoreAt, withFixtureCopy)

spec :: Spec
spec =
  describe "temporal modules" do
    it "creates multiple temporal modules and loads them on reload" do
      withFixtureCopy \fixtureRoot -> do
        (firstPath, secondPath, evalResult) <-
          fixtureLoreAt fixtureRoot do
            firstPath <- createTemporalModule
            secondPath <- createTemporalModule
            liftIO $ writeModule firstPath "tempOneValue" "11"
            liftIO $ writeModule secondPath "tempTwoValue" "31"
            _ <- loadHomeModules defaultLoadHomeModulesOptions
            evalResult <- executeStatement "(tempOneValue, tempTwoValue)"
            pure (firstPath, secondPath, evalResult)

        doesFileExist firstPath `shouldReturn` True
        doesFileExist secondPath `shouldReturn` True
        evalResult `shouldBe` Right "(11,31)"

    it "keeps only existing temporal modules on subsequent reloads" do
      withFixtureCopy \fixtureRoot -> do
        (temporalPath, beforeDeleteResult, afterDeleteResult) <-
          fixtureLoreAt fixtureRoot do
            temporalPath <- createTemporalModule
            liftIO $ writeModule temporalPath "ephemeralValue" "42"
            _ <- loadHomeModules defaultLoadHomeModulesOptions
            beforeDeleteResult <- executeStatement "ephemeralValue"
            liftIO $ removeFile temporalPath
            _ <- loadHomeModules defaultLoadHomeModulesOptions
            afterDeleteResult <- executeStatement "ephemeralValue"
            pure (temporalPath, beforeDeleteResult, afterDeleteResult)

        beforeDeleteResult `shouldBe` Right "42"
        afterDeleteResult `shouldSatisfy` isMissingSymbolFailure
        doesFileExist temporalPath `shouldReturn` False

    it "disables warning-as-error behavior for temporal modules" do
      withFixtureCopy \fixtureRoot -> do
        enableWarningErrors fixtureRoot
        (loadResult, evalResult) <-
          fixtureLoreAt fixtureRoot do
            temporalPath <- createTemporalModule
            liftIO $ appendUntypedBinding temporalPath "warningValue = 5"
            loadResult <- loadHomeModules defaultLoadHomeModulesOptions
            evalResult <- executeStatement "warningValue"
            pure (loadResult, evalResult)

        loadResult.loadHomeModulesSucceeded `shouldBe` True
        evalResult `shouldBe` Right "5"

writeModule :: FilePath -> String -> String -> IO ()
writeModule modulePath bindingName bindingValue =
  TIO.appendFile modulePath content
  where
    content =
      T.unlines
        [ "",
          T.pack bindingName <> " :: Int",
          T.pack bindingName <> " = " <> T.pack bindingValue
        ]

appendUntypedBinding :: FilePath -> T.Text -> IO ()
appendUntypedBinding modulePath bindingLine =
  TIO.appendFile modulePath (T.unlines ["", bindingLine])

enableWarningErrors :: FilePath -> IO ()
enableWarningErrors fixtureRoot = do
  let packageFile = fixtureRoot </> "package.yaml"
  packageSource <- TIO.readFile packageFile
  TIO.writeFile packageFile $
    packageSource
      <> T.unlines
        [ "",
          "ghc-options:",
          "- -Wall",
          "- -Werror"
        ]

isMissingSymbolFailure :: Either [Diagnostic] String -> Bool
isMissingSymbolFailure = \case
  Left diagnostics ->
    any (T.isInfixOf "not in scope" . diagnosticMessage) diagnostics
  Right _ ->
    False
