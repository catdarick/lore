module Lore.Internal.Session.Environment
  ( SessionConfigError (..),
    applySessionEnvironmentVariables,
    applySessionEnvironment,
    renderSessionConfigError,
  )
where

import Control.Applicative ((<|>))
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Lore.Internal.Ghc.DynFlags (ParallelWorkersCount (..))
import Lore.Internal.Session (SessionConfig (..))
import Lore.Internal.TestSuite.Arguments
  ( parseTestArguments,
    renderTestArgumentsParseError,
  )
import Lore.Logger (LogLevel (..), prettyLoggerHandle)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

data SessionConfigError
  = InvalidSessionEnvironmentVariable
      { environmentVariableName :: String,
        environmentVariableValue :: String,
        environmentVariableExpectation :: Text
      }
  deriving stock (Eq, Show)

applySessionEnvironment :: SessionConfig -> IO (Either SessionConfigError SessionConfig)
applySessionEnvironment baseConfig = do
  environment <- traverse lookupSessionEnvironmentVariable sessionEnvironmentVariableNames
  pure (applySessionEnvironmentVariables baseConfig environment)

applySessionEnvironmentVariables :: SessionConfig -> [(String, Maybe String)] -> Either SessionConfigError SessionConfig
applySessionEnvironmentVariables baseConfig environment = do
    projectRootOverride <- parseOptional "LORE_PROJECT_ROOT" parseNonEmptyPath environment
    ghcWorkDirOverride <- parseOptional "LORE_GHC_WORK_DIR" parseNonEmptyPath environment
    customPreludeOverride <- parseOptional "LORE_CUSTOM_PRELUDE" parseCustomPrelude environment
    parallelWorkersLimitOverride <- parseOptional "LORE_PARALLEL_WORKERS_LIMIT" parseParallelWorkersCount environment
    logLevelOverride <- parseOptional "LORE_LOG_LEVEL" parseLogLevel environment
    defaultTestArgumentsOverride <- parseOptional "LORE_DEFAULT_TEST_ARGS" parseDefaultTestArguments environment
    pure
      baseConfig
        { projectRoot = fromMaybe baseConfig.projectRoot projectRootOverride,
          ghcWorkDir = fromMaybe baseConfig.ghcWorkDir ghcWorkDirOverride,
          customPrelude = customPreludeOverride <|> baseConfig.customPrelude,
          parallelWorkersLimit = fromMaybe baseConfig.parallelWorkersLimit parallelWorkersLimitOverride,
          loggerHandle = maybe baseConfig.loggerHandle prettyLoggerHandle logLevelOverride,
          testSuiteDefaultArguments = fromMaybe baseConfig.testSuiteDefaultArguments defaultTestArgumentsOverride
        }

renderSessionConfigError :: SessionConfigError -> Text
renderSessionConfigError InvalidSessionEnvironmentVariable {environmentVariableName, environmentVariableValue, environmentVariableExpectation} =
  "Invalid value for "
    <> T.pack environmentVariableName
    <> ": "
    <> T.pack (show environmentVariableValue)
    <> ". Expected "
    <> environmentVariableExpectation
    <> "."

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
  Either SessionConfigError (Maybe a)
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
            InvalidSessionEnvironmentVariable
              { environmentVariableName = name,
                environmentVariableValue = rawValue,
                environmentVariableExpectation = expectation
              }
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
