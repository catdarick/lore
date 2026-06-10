module Lore.Internal.Session.Environment
  ( SessionConfigError,
    SessionConfigOverrides (..),
    defaultSessionConfigOverrides,
    parseSessionConfigOverrides,
    applySessionConfigOverrides,
    loadSessionEnvironmentOverrides,
    parseSessionEnvironmentOverrides,
    sessionEnvironmentVariableNames,
    normalizeSessionConfigPaths,
    normalizePathRelativeTo,
    renderSessionConfigError,
  )
where

import Control.Applicative ((<|>))
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as JT
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Lore.Internal.Config.Document
  ( ConfigError (..),
    LoadedConfigDocument,
    decodeConfigSection,
    renderConfigError,
  )
import Lore.Internal.Ghc.DynFlags (ParallelWorkersCount (..))
import Lore.Internal.Session (SessionConfig (..))
import Lore.Internal.TestSuite.Arguments
  ( parseTestArguments,
    renderTestArgumentsParseError,
  )
import Lore.Logger (LogLevel (..), prettyLoggerHandle)
import System.Environment (lookupEnv)
import System.FilePath (isAbsolute, normalise, takeDirectory, (</>))
import Text.Read (readMaybe)

type SessionConfigError = ConfigError

data SessionConfigOverrides = SessionConfigOverrides
  { projectRootOverride :: Maybe FilePath,
    ghcWorkDirOverride :: Maybe FilePath,
    customPreludeOverride :: Maybe Text,
    parallelWorkersLimitOverride :: Maybe ParallelWorkersCount,
    logLevelOverride :: Maybe LogLevel,
    defaultTestArgumentsOverride :: Maybe [String]
  }
  deriving stock (Eq, Show)

defaultSessionConfigOverrides :: SessionConfigOverrides
defaultSessionConfigOverrides =
  SessionConfigOverrides
    { projectRootOverride = Nothing,
      ghcWorkDirOverride = Nothing,
      customPreludeOverride = Nothing,
      parallelWorkersLimitOverride = Nothing,
      logLevelOverride = Nothing,
      defaultTestArgumentsOverride = Nothing
    }

parseSessionConfigOverrides :: LoadedConfigDocument -> Either ConfigError SessionConfigOverrides
parseSessionConfigOverrides =
  decodeConfigSection "session"

applySessionConfigOverrides :: SessionConfigOverrides -> SessionConfig -> SessionConfig
applySessionConfigOverrides overrides baseConfig =
  baseConfig
    { projectRoot = fromMaybe baseConfig.projectRoot overrides.projectRootOverride,
      ghcWorkDir = fromMaybe baseConfig.ghcWorkDir overrides.ghcWorkDirOverride,
      customPrelude = overrides.customPreludeOverride <|> baseConfig.customPrelude,
      parallelWorkersLimit = fromMaybe baseConfig.parallelWorkersLimit overrides.parallelWorkersLimitOverride,
      loggerHandle = maybe baseConfig.loggerHandle prettyLoggerHandle overrides.logLevelOverride,
      testSuiteDefaultArguments = fromMaybe baseConfig.testSuiteDefaultArguments overrides.defaultTestArgumentsOverride
    }

loadSessionEnvironmentOverrides :: IO (Either ConfigError SessionConfigOverrides)
loadSessionEnvironmentOverrides = do
  environment <- traverse lookupSessionEnvironmentVariable sessionEnvironmentVariableNames
  pure (parseSessionEnvironmentOverrides environment)

parseSessionEnvironmentOverrides :: [(String, Maybe String)] -> Either ConfigError SessionConfigOverrides
parseSessionEnvironmentOverrides environment = do
  projectRootOverride <- parseOptional "LORE_PROJECT_ROOT" parseNonEmptyPath environment
  ghcWorkDirOverride <- parseOptional "LORE_GHC_WORK_DIR" parseNonEmptyPath environment
  customPreludeOverride <- parseOptional "LORE_CUSTOM_PRELUDE" parseCustomPrelude environment
  parallelWorkersLimitOverride <- parseOptional "LORE_PARALLEL_WORKERS_LIMIT" parseParallelWorkersCount environment
  logLevelOverride <- parseOptional "LORE_LOG_LEVEL" parseLogLevel environment
  defaultTestArgumentsOverride <- parseOptional "LORE_DEFAULT_TEST_ARGS" parseDefaultTestArguments environment
  pure
    defaultSessionConfigOverrides
      { projectRootOverride = projectRootOverride,
        ghcWorkDirOverride = ghcWorkDirOverride,
        customPreludeOverride = customPreludeOverride,
        parallelWorkersLimitOverride = parallelWorkersLimitOverride,
        logLevelOverride = logLevelOverride,
        defaultTestArgumentsOverride = defaultTestArgumentsOverride
      }

renderSessionConfigError :: SessionConfigError -> Text
renderSessionConfigError =
  renderConfigError

sessionEnvironmentVariableNames :: [String]
sessionEnvironmentVariableNames =
  [ "LORE_PROJECT_ROOT",
    "LORE_GHC_WORK_DIR",
    "LORE_CUSTOM_PRELUDE",
    "LORE_PARALLEL_WORKERS_LIMIT",
    "LORE_LOG_LEVEL",
    "LORE_DEFAULT_TEST_ARGS"
  ]

lookupSessionEnvironmentVariable :: String -> IO (String, Maybe String)
lookupSessionEnvironmentVariable name = do
  value <- lookupEnv name
  pure (name, value)

parseOptional ::
  String ->
  (String -> Either Text a) ->
  [(String, Maybe String)] ->
  Either ConfigError (Maybe a)
parseOptional name parser environment =
  case lookup name environment of
    Nothing ->
      pure Nothing
    Just Nothing ->
      pure Nothing
    Just (Just rawValue) ->
      case parser rawValue of
        Left expectation ->
          Left
            (InvalidSessionEnvironmentVariable name rawValue expectation)
        Right parsedValue ->
          pure (Just parsedValue)

parseNonEmptyPath :: String -> Either Text FilePath
parseNonEmptyPath rawValue
  | null rawValue = Left "a non-empty path"
  | otherwise = Right rawValue

parseCustomPrelude :: String -> Either Text Text
parseCustomPrelude rawValue
  | T.null normalized = Left "a non-empty module name"
  | otherwise = Right normalized
  where
    normalized = T.strip (T.pack rawValue)

parseParallelWorkersCount :: String -> Either Text ParallelWorkersCount
parseParallelWorkersCount rawValue =
  case normalized of
    "auto" ->
      Right WorkersAsNumProcessors
    _ ->
      case readMaybe normalized of
        Just workersCount
          | workersCount > (0 :: Int) ->
              Right (ThisWorkersCount workersCount)
        _ ->
          Left "\"auto\" or a positive integer"
  where
    normalized = map toLower (T.unpack (T.strip (T.pack rawValue)))

parseLogLevel :: String -> Either Text LogLevel
parseLogLevel rawValue =
  case T.toLower (T.strip (T.pack rawValue)) of
    "debug" -> Right Debug
    "info" -> Right Info
    "warning" -> Right Warning
    "warn" -> Right Warning
    "error" -> Right Error
    _ -> Left "one of debug, info, warning, warn, or error"

parseDefaultTestArguments :: String -> Either Text [String]
parseDefaultTestArguments rawValue
  | T.null (T.strip rawText) = Right []
  | otherwise =
      case parseTestArguments rawText of
        Left parseError ->
          Left ("valid shell-style test arguments (" <> renderTestArgumentsParseError parseError <> ")")
        Right arguments ->
          Right arguments
  where
    rawText = T.pack rawValue

normalizeSessionConfigPaths :: FilePath -> SessionConfig -> SessionConfig
normalizeSessionConfigPaths configPath config =
  let configDir = takeDirectory configPath
      normalizedProjectRoot = normalizePathRelativeTo configDir config.projectRoot
   in config
        { projectRoot = normalizedProjectRoot,
          ghcWorkDir = normalizePathRelativeTo normalizedProjectRoot config.ghcWorkDir,
          configFilePath = configPath
        }

normalizePathRelativeTo :: FilePath -> FilePath -> FilePath
normalizePathRelativeTo base path
  | isAbsolute path = normalise path
  | otherwise = normalise (base </> path)

instance J.FromJSON SessionConfigOverrides where
  parseJSON = J.withObject "SessionConfigOverrides" \obj -> do
    projectRootOverride <- obj J..:? "project-root" >>= traverse parseYamlPath
    ghcWorkDirOverride <- obj J..:? "ghc-work-dir" >>= traverse parseYamlPath
    customPreludeOverride <- fmap T.pack <$> obj J..:? "custom-prelude" >>= traverse parseYamlCustomPrelude
    parallelWorkersLimitOverride <- obj J..:? "parallel-workers-limit" >>= traverse parseYamlParallelWorkersCount
    logLevelOverride <- obj J..:? "log-level" >>= traverse parseYamlLogLevel
    defaultTestArgumentsOverride <- obj J..:? "default-test-args" J..!= Nothing
    pure
      defaultSessionConfigOverrides
        { projectRootOverride = projectRootOverride,
          ghcWorkDirOverride = ghcWorkDirOverride,
          customPreludeOverride = customPreludeOverride,
          parallelWorkersLimitOverride = parallelWorkersLimitOverride,
          logLevelOverride = logLevelOverride,
          defaultTestArgumentsOverride = defaultTestArgumentsOverride
        }

parseYamlPath :: FilePath -> JT.Parser FilePath
parseYamlPath rawValue =
  either (fail . T.unpack) pure (parseNonEmptyPath rawValue)

parseYamlCustomPrelude :: Text -> JT.Parser Text
parseYamlCustomPrelude rawValue =
  either (fail . T.unpack) pure (parseCustomPrelude (T.unpack rawValue))

parseYamlParallelWorkersCount :: J.Value -> JT.Parser ParallelWorkersCount
parseYamlParallelWorkersCount = \case
  J.String rawValue ->
    either (fail . T.unpack) pure (parseParallelWorkersCount (T.unpack rawValue))
  J.Number number ->
    case JT.parseMaybe J.parseJSON (J.Number number) of
      Just workersCount
        | workersCount > (0 :: Int) ->
            pure (ThisWorkersCount workersCount)
      _ ->
        fail (T.unpack parallelWorkersExpectation)
  _ ->
    fail (T.unpack parallelWorkersExpectation)
  where
    parallelWorkersExpectation = "\"auto\" or a positive integer"

parseYamlLogLevel :: Text -> JT.Parser LogLevel
parseYamlLogLevel rawValue =
  either (fail . T.unpack) pure (parseLogLevel (T.unpack rawValue))
