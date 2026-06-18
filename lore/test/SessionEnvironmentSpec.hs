module SessionEnvironmentSpec
  ( spec,
  )
where

import Control.Exception (bracket)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Yaml as Y
import Lore
  ( ConfigError (..),
    LoadedConfigDocument (..),
    ParallelWorkersCount (..),
    SessionConfig (..),
    defaultSessionConfig,
    loadStartupConfig,
    startupSessionConfig,
  )
import Lore.Internal.Session.Environment
  ( applySessionConfigOverrides,
    loadSessionEnvironmentOverrides,
    parseSessionConfigOverrides,
    parseSessionEnvironmentOverrides,
  )
import qualified Lore.Internal.Session.Environment as SessionEnvironment
import System.Environment (lookupEnv, setEnv, unsetEnv)
import Test.Hspec

spec :: Spec
spec =
  describe "session configuration resolution" do
    it "uses defaultSessionConfig when no session overrides are set" do
      withSessionEnvironment [] do
        config <- shouldLoadEnvironmentSessionConfig
        config `shouldMatchDefaultSessionConfig` defaultSessionConfig

    it "applies each environment variable independently over defaults" do
      withSessionEnvironment [("LORE_PROJECT_ROOT", "/tmp/project root")] do
        config <- shouldLoadEnvironmentSessionConfig
        config.projectRoot `shouldBe` "/tmp/project root"

      withSessionEnvironment [("LORE_GHC_WORK_DIR", "/tmp/ghc work")] do
        config <- shouldLoadEnvironmentSessionConfig
        config.ghcWorkDir `shouldBe` "/tmp/ghc work"

      withSessionEnvironment [("LORE_CUSTOM_PRELUDE", "  CustomPrelude  ")] do
        config <- shouldLoadEnvironmentSessionConfig
        config.customPrelude `shouldBe` Just "CustomPrelude"

      withSessionEnvironment [("LORE_PARALLEL_WORKERS_LIMIT", "4")] do
        config <- shouldLoadEnvironmentSessionConfig
        config.parallelWorkersLimit `shouldBe` ThisWorkersCount 4

      withSessionEnvironment [("LORE_LOG_LEVEL", "debug")] do
        config <- shouldLoadEnvironmentSessionConfig
        config.projectRoot `shouldBe` defaultSessionConfig.projectRoot

      withSessionEnvironment [("LORE_DEFAULT_TEST_ARGS", "--match \"prefix sample\"")] do
        config <- shouldLoadEnvironmentSessionConfig
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
          config <- shouldLoadEnvironmentSessionConfig
          config.projectRoot `shouldBe` "/tmp/project root"
          config.ghcWorkDir `shouldBe` "/tmp/ghc work"
          config.customPrelude `shouldBe` Just "CustomPrelude"
          config.parallelWorkersLimit `shouldBe` ThisWorkersCount 2
          config.testSuiteDefaultArguments `shouldBe` ["--arg1", "two words"]

    it "parses and validates worker counts" do
      withSessionEnvironment [("LORE_PARALLEL_WORKERS_LIMIT", " AuTo ")] do
        config <- shouldLoadEnvironmentSessionConfig
        config.parallelWorkersLimit `shouldBe` WorkersAsNumProcessors

      withSessionEnvironment [("LORE_PARALLEL_WORKERS_LIMIT", "1")] do
        config <- shouldLoadEnvironmentSessionConfig
        config.parallelWorkersLimit `shouldBe` ThisWorkersCount 1

      shouldReject "LORE_PARALLEL_WORKERS_LIMIT" "0"
      shouldReject "LORE_PARALLEL_WORKERS_LIMIT" "-1"
      shouldReject "LORE_PARALLEL_WORKERS_LIMIT" "many"

    it "parses log levels and aliases case-insensitively" do
      mapM_
        ( \rawValue ->
            withSessionEnvironment [("LORE_LOG_LEVEL", rawValue)] do
              config <- shouldLoadEnvironmentSessionConfig
              config.projectRoot `shouldBe` defaultSessionConfig.projectRoot
        )
        ["debug", "INFO", "warning", "warn", "ERROR"]

      shouldReject "LORE_LOG_LEVEL" "verbose"

    it "validates empty paths and custom preludes" do
      shouldReject "LORE_PROJECT_ROOT" ""
      shouldReject "LORE_GHC_WORK_DIR" ""
      shouldReject "LORE_CUSTOM_PRELUDE" "   "

      withSessionEnvironment [("LORE_PROJECT_ROOT", "  spaced path  ")] do
        config <- shouldLoadEnvironmentSessionConfig
        config.projectRoot `shouldBe` "  spaced path  "

    it "handles missing, blank, valid, and malformed default test arguments" do
      withSessionEnvironment [] do
        config <- shouldLoadEnvironmentSessionConfig
        config.testSuiteDefaultArguments `shouldBe` []

      withSessionEnvironment [("LORE_DEFAULT_TEST_ARGS", "   \t  ")] do
        config <- shouldLoadEnvironmentSessionConfig
        config.testSuiteDefaultArguments `shouldBe` []

      withSessionEnvironment [("LORE_DEFAULT_TEST_ARGS", "--match \"some test\" --flag")] do
        config <- shouldLoadEnvironmentSessionConfig
        config.testSuiteDefaultArguments `shouldBe` ["--match", "some test", "--flag"]

      shouldReject "LORE_DEFAULT_TEST_ARGS" "--match \"unterminated"

    it "reports the environment variable name and bad value in errors" do
      withSessionEnvironment [("LORE_DEFAULT_TEST_ARGS", "'unterminated")] do
        result <- loadSessionEnvironmentOverrides
        case result of
          Left (InvalidSessionEnvironmentVariable name value expectation) -> do
            name `shouldBe` "LORE_DEFAULT_TEST_ARGS"
            value `shouldBe` "'unterminated"
            T.null expectation `shouldBe` False
          Right _ ->
            expectationFailure "Expected invalid environment variable error"

    it "loads startup YAML before applying environment overrides" do
      withSessionEnvironment [("LORE_PROJECT_ROOT", "/env/project")] do
        result <- loadStartupConfig
        case result of
          Left err ->
            ioError (userError ("Expected valid startup config, got: " <> show err))
          Right startupConfig -> do
            let config = startupSessionConfig startupConfig
            config.projectRoot `shouldBe` "/env/project"

    it "parses native YAML session overrides" do
      overrides <-
        shouldParseYamlSessionOverrides
          "session:\n  project-root: ./project\n  ghc-work-dir: .work\n  custom-prelude: CustomPrelude\n  parallel-workers-limit: 3\n  log-level: warn\n  default-test-args:\n    - --match\n    - some test name\n"
      let config = applySessionConfigOverrides overrides defaultSessionConfig
      config.projectRoot `shouldBe` "./project"
      config.ghcWorkDir `shouldBe` ".work"
      config.customPrelude `shouldBe` Just "CustomPrelude"
      config.parallelWorkersLimit `shouldBe` ThisWorkersCount 3
      config.testSuiteDefaultArguments `shouldBe` ["--match", "some test name"]

    it "rejects invalid YAML worker limits" do
      parseYamlSessionOverrides "session:\n  parallel-workers-limit: 0\n"
        `shouldSatisfy` isLeft
      parseYamlSessionOverrides "session:\n  parallel-workers-limit: -1\n"
        `shouldSatisfy` isLeft
      parseYamlSessionOverrides "session:\n  parallel-workers-limit: many\n"
        `shouldSatisfy` isLeft

shouldLoadEnvironmentSessionConfig :: IO SessionConfig
shouldLoadEnvironmentSessionConfig = do
  result <- loadSessionEnvironmentOverrides
  case result of
    Left err ->
      ioError (userError ("Expected valid session config, got: " <> show err))
    Right overrides ->
      pure (applySessionConfigOverrides overrides defaultSessionConfig)

shouldReject :: String -> String -> Expectation
shouldReject name value = do
  let result =
        parseSessionEnvironmentOverrides
          [(name, Just value)]
  case result of
    Left (InvalidSessionEnvironmentVariable errName errValue _) -> do
      errName `shouldBe` name
      errValue `shouldBe` value
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

shouldParseYamlSessionOverrides :: BS.ByteString -> IO SessionEnvironment.SessionConfigOverrides
shouldParseYamlSessionOverrides rawYaml =
  case parseYamlSessionOverrides rawYaml of
    Left err ->
      ioError (userError ("Expected valid YAML session overrides, got: " <> show err))
    Right overrides ->
      pure overrides

parseYamlSessionOverrides :: BS.ByteString -> Either ConfigError SessionEnvironment.SessionConfigOverrides
parseYamlSessionOverrides rawYaml = do
  value <-
    case Y.decodeEither' rawYaml of
      Left parseError ->
        Left (ConfigFileParseError "lore.yaml" (T.pack (Y.prettyPrintParseException parseError)))
      Right parsedValue ->
        Right parsedValue
  parseSessionConfigOverrides
    LoadedConfigDocument
      { configFilePath = "lore.yaml",
        configFileValue = value
      }

isLeft :: Either err value -> Bool
isLeft = \case
  Left _ -> True
  Right _ -> False
