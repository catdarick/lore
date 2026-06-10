module SessionEnvironmentSpec
  ( spec,
  )
where

import Control.Exception (bracket)
import qualified Data.Text as T
import Lore
  ( ParallelWorkersCount (..),
    SessionConfig (..),
    SessionConfigError (..),
    defaultSessionConfig,
    loadSessionConfigFromEnvironment,
  )
import Lore.Internal.Session.Environment (applySessionEnvironmentVariables)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import Test.Hspec

spec :: Spec
spec =
  describe "loadSessionConfigFromEnvironment" do
    it "uses defaultSessionConfig when no session environment variables are set" do
      withSessionEnvironment [] do
        config <- shouldLoadSessionConfig
        config `shouldMatchDefaultSessionConfig` defaultSessionConfig

    it "applies each environment variable independently over defaults" do
      withSessionEnvironment [("LORE_PROJECT_ROOT", "/tmp/project root")] do
        config <- shouldLoadSessionConfig
        config.projectRoot `shouldBe` "/tmp/project root"

      withSessionEnvironment [("LORE_GHC_WORK_DIR", "/tmp/ghc work")] do
        config <- shouldLoadSessionConfig
        config.ghcWorkDir `shouldBe` "/tmp/ghc work"

      withSessionEnvironment [("LORE_CUSTOM_PRELUDE", "  CustomPrelude  ")] do
        config <- shouldLoadSessionConfig
        config.customPrelude `shouldBe` Just "CustomPrelude"

      withSessionEnvironment [("LORE_PARALLEL_WORKERS_LIMIT", "4")] do
        config <- shouldLoadSessionConfig
        config.parallelWorkersLimit `shouldBe` ThisWorkersCount 4

      withSessionEnvironment [("LORE_LOG_LEVEL", "debug")] do
        config <- shouldLoadSessionConfig
        config.projectRoot `shouldBe` defaultSessionConfig.projectRoot

      withSessionEnvironment [("LORE_DEFAULT_TEST_ARGS", "--match \"prefix sample\"")] do
        config <- shouldLoadSessionConfig
        config.testSuiteDefaultArguments `shouldBe` ["--match", "prefix sample"]

    it "composes all session environment variables" do
      withSessionEnvironment
        [ ("LORE_PROJECT_ROOT", "/tmp/project root"),
          ("LORE_GHC_WORK_DIR", "/tmp/ghc work"),
          ("LORE_CUSTOM_PRELUDE", "CustomPrelude"),
          ("LORE_PARALLEL_WORKERS_LIMIT", "2"),
          ("LORE_LOG_LEVEL", "warning"),
          ("LORE_DEFAULT_TEST_ARGS", "--arg1 'two words'")
        ]
        do
          config <- shouldLoadSessionConfig
          config.projectRoot `shouldBe` "/tmp/project root"
          config.ghcWorkDir `shouldBe` "/tmp/ghc work"
          config.customPrelude `shouldBe` Just "CustomPrelude"
          config.parallelWorkersLimit `shouldBe` ThisWorkersCount 2
          config.testSuiteDefaultArguments `shouldBe` ["--arg1", "two words"]

    it "parses and validates worker counts" do
      withSessionEnvironment [("LORE_PARALLEL_WORKERS_LIMIT", " AuTo ")] do
        config <- shouldLoadSessionConfig
        config.parallelWorkersLimit `shouldBe` WorkersAsNumProcessors

      withSessionEnvironment [("LORE_PARALLEL_WORKERS_LIMIT", "1")] do
        config <- shouldLoadSessionConfig
        config.parallelWorkersLimit `shouldBe` ThisWorkersCount 1

      shouldReject "LORE_PARALLEL_WORKERS_LIMIT" "0"
      shouldReject "LORE_PARALLEL_WORKERS_LIMIT" "-1"
      shouldReject "LORE_PARALLEL_WORKERS_LIMIT" "many"

    it "parses log levels and aliases case-insensitively" do
      mapM_
        ( \rawValue ->
            withSessionEnvironment [("LORE_LOG_LEVEL", rawValue)] do
              config <- shouldLoadSessionConfig
              config.projectRoot `shouldBe` defaultSessionConfig.projectRoot
        )
        ["debug", "INFO", "warning", "warn", "ERROR"]

      shouldReject "LORE_LOG_LEVEL" "verbose"

    it "validates empty paths and custom preludes" do
      shouldReject "LORE_PROJECT_ROOT" ""
      shouldReject "LORE_GHC_WORK_DIR" ""
      shouldReject "LORE_CUSTOM_PRELUDE" "   "

      withSessionEnvironment [("LORE_PROJECT_ROOT", "  spaced path  ")] do
        config <- shouldLoadSessionConfig
        config.projectRoot `shouldBe` "  spaced path  "

    it "handles missing, blank, valid, and malformed default test arguments" do
      withSessionEnvironment [] do
        config <- shouldLoadSessionConfig
        config.testSuiteDefaultArguments `shouldBe` []

      withSessionEnvironment [("LORE_DEFAULT_TEST_ARGS", "   \t  ")] do
        config <- shouldLoadSessionConfig
        config.testSuiteDefaultArguments `shouldBe` []

      withSessionEnvironment [("LORE_DEFAULT_TEST_ARGS", "--match \"some test\" --flag")] do
        config <- shouldLoadSessionConfig
        config.testSuiteDefaultArguments `shouldBe` ["--match", "some test", "--flag"]

      shouldReject "LORE_DEFAULT_TEST_ARGS" "--match \"unterminated"

    it "reports the environment variable name and bad value in errors" do
      withSessionEnvironment [("LORE_DEFAULT_TEST_ARGS", "'unterminated")] do
        result <- loadSessionConfigFromEnvironment
        case result of
          Left err@InvalidSessionEnvironmentVariable {} -> do
            err.environmentVariableName `shouldBe` "LORE_DEFAULT_TEST_ARGS"
            err.environmentVariableValue `shouldBe` "'unterminated"
            T.null err.environmentVariableExpectation `shouldBe` False
          Right _ ->
            expectationFailure "Expected invalid environment variable error"

shouldLoadSessionConfig :: IO SessionConfig
shouldLoadSessionConfig = do
  result <- loadSessionConfigFromEnvironment
  case result of
    Left err ->
      ioError (userError ("Expected valid session config, got: " <> show err))
    Right config ->
      pure config

shouldReject :: String -> String -> Expectation
shouldReject name value = do
  let result =
        applySessionEnvironmentVariables
          defaultSessionConfig
          [(name, Just value)]
  case result of
    Left err@InvalidSessionEnvironmentVariable {} -> do
      err.environmentVariableName `shouldBe` name
      err.environmentVariableValue `shouldBe` value
    Right _ ->
      expectationFailure ("Expected invalid environment variable for " <> name)

shouldMatchDefaultSessionConfig :: SessionConfig -> SessionConfig -> Expectation
shouldMatchDefaultSessionConfig actual expected = do
  actual.projectRoot `shouldBe` expected.projectRoot
  actual.ghcWorkDir `shouldBe` expected.ghcWorkDir
  actual.projectProviderOverride `shouldBe` expected.projectProviderOverride
  actual.customPrelude `shouldBe` expected.customPrelude
  actual.parallelWorkersLimit `shouldBe` expected.parallelWorkersLimit
  actual.testSuiteDefaultArguments `shouldBe` expected.testSuiteDefaultArguments
  actual.isTestSuiteFunctionalityRequired `shouldBe` expected.isTestSuiteFunctionalityRequired

withSessionEnvironment :: [(String, String)] -> IO a -> IO a
withSessionEnvironment overrides action =
  bracket
    saveEnvironment
    restoreEnvironment
    (const (setOverridesAndRun action))
  where
    setOverridesAndRun bracketedAction = do
      mapM_ unsetEnv sessionEnvironmentVariableNames
      mapM_ (uncurry setEnv) overrides
      bracketedAction

    saveEnvironment =
      traverse (\name -> (name,) <$> lookupEnv name) sessionEnvironmentVariableNames

    restoreEnvironment savedValues =
      mapM_ restoreEnvVar savedValues

sessionEnvironmentVariableNames :: [String]
sessionEnvironmentVariableNames =
  [ "LORE_PROJECT_ROOT",
    "LORE_GHC_WORK_DIR",
    "LORE_CUSTOM_PRELUDE",
    "LORE_PARALLEL_WORKERS_LIMIT",
    "LORE_LOG_LEVEL",
    "LORE_DEFAULT_TEST_ARGS"
  ]

restoreEnvVar :: (String, Maybe String) -> IO ()
restoreEnvVar (name, maybeValue) =
  case maybeValue of
    Nothing -> unsetEnv name
    Just value -> setEnv name value
