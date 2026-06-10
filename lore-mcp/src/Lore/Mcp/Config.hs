module Lore.Mcp.Config
  ( McpConfig (..),
    McpConfigOverrides (..),
    McpConfigError (..),
    defaultMcpConfig,
    parseMcpYamlConfig,
    loadMcpEnvironmentOverrides,
    resolveMcpConfig,
    toolEnabled,
    defaultToolEnabledByName,
    toolEnabledEnvVarName,
    renderMcpConfigError,
  )
where

import Control.Monad (forM)
import Data.Aeson ((.:?))
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as JKM
import Data.Char (isAlpha, isAlphaNum, isDigit, isLower, isUpper, toUpper)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Lore.Config (LoadedConfigDocument (..))
import System.Environment (lookupEnv)

data McpConfig = McpConfig
  { definitionKnowledgeCacheEnabled :: Bool,
    feedbackFilePath :: Maybe FilePath,
    toolEnabledOverrides :: Map.Map Text Bool
  }
  deriving stock (Eq, Show)

data McpConfigOverrides = McpConfigOverrides
  { definitionKnowledgeCacheEnabledOverride :: Maybe Bool,
    feedbackFilePathOverride :: Maybe (Maybe FilePath),
    toolEnabledOverridesOverride :: Map.Map Text Bool
  }
  deriving stock (Eq, Show)

data McpConfigError
  = InvalidMcpEnvironmentVariable String String Text
  | InvalidMcpConfig FilePath Text
  | UnknownMcpToolName FilePath Text
  deriving stock (Eq, Show)

defaultMcpConfig :: McpConfig
defaultMcpConfig =
  McpConfig
    { definitionKnowledgeCacheEnabled = False,
      feedbackFilePath = Nothing,
      toolEnabledOverrides = mempty
    }

parseMcpYamlConfig :: Set.Set Text -> LoadedConfigDocument -> Either McpConfigError McpConfigOverrides
parseMcpYamlConfig knownToolNames document =
  case J.fromJSON (mcpSectionValue document.configFileValue) of
    J.Error err ->
      Left (InvalidMcpConfig document.configFilePath (T.pack err))
    J.Success overrides ->
      case filter (`Set.notMember` knownToolNames) (Map.keys overrides.toolEnabledOverridesOverride) of
        unknownToolName : _ ->
          Left (UnknownMcpToolName document.configFilePath unknownToolName)
        [] ->
          Right overrides

loadMcpEnvironmentOverrides :: Set.Set Text -> IO (Either McpConfigError McpConfigOverrides)
loadMcpEnvironmentOverrides toolNames = do
  maybeDefinitionCache <-
    lookupOptionalEnvParsed "LORE_MCP_ENABLE_DEFINITION_KNOWLEDGE_CACHE" parseBool
  maybeFeedbackFile <- lookupEnv "LORE_MCP_FEEDBACK_FILE"
  toolOverrideResults <- forM (Set.toList toolNames) \toolName -> do
    maybeEnabled <- lookupOptionalEnvParsed (toolEnabledEnvVarName toolName) parseBool
    pure (fmap (fmap (toolName,)) maybeEnabled)
  pure $
    McpConfigOverrides
      <$> maybeDefinitionCache
      <*> pure (Just <$> maybeFeedbackFile)
      <*> (Map.fromList . foldMap maybeToList <$> sequence toolOverrideResults)

resolveMcpConfig :: McpConfig -> McpConfigOverrides -> McpConfigOverrides -> McpConfig
resolveMcpConfig defaults yamlOverrides environmentOverrides =
  applyMcpConfigOverrides environmentOverrides (applyMcpConfigOverrides yamlOverrides defaults)

toolEnabled :: McpConfig -> Text -> Bool
toolEnabled config toolName =
  fromMaybe (defaultToolEnabledByName toolName) (Map.lookup toolName config.toolEnabledOverrides)

defaultToolEnabledByName :: Text -> Bool
defaultToolEnabledByName toolName =
  toolName /= "runTestSuite"

toolEnabledEnvVarName :: Text -> String
toolEnabledEnvVarName toolName =
  "LORE_MCP_TOOL_ENABLED_" <> toSnakeUpper (T.unpack toolName)

renderMcpConfigError :: McpConfigError -> Text
renderMcpConfigError = \case
  InvalidMcpEnvironmentVariable mcpEnvironmentVariableName mcpEnvironmentVariableValue mcpEnvironmentVariableExpectation ->
    "Invalid value for "
      <> T.pack mcpEnvironmentVariableName
      <> ": "
      <> T.pack (show mcpEnvironmentVariableValue)
      <> ". Expected "
      <> mcpEnvironmentVariableExpectation
      <> "."
  InvalidMcpConfig path message ->
    "Invalid "
      <> T.pack path
      <> " MCP configuration: "
      <> message
  UnknownMcpToolName path toolName ->
    "Invalid "
      <> T.pack path
      <> " MCP configuration: unknown tool "
      <> quote toolName
      <> "."

applyMcpConfigOverrides :: McpConfigOverrides -> McpConfig -> McpConfig
applyMcpConfigOverrides overrides config =
  config
    { definitionKnowledgeCacheEnabled =
        fromMaybe config.definitionKnowledgeCacheEnabled overrides.definitionKnowledgeCacheEnabledOverride,
      feedbackFilePath =
        fromMaybe config.feedbackFilePath overrides.feedbackFilePathOverride,
      toolEnabledOverrides =
        Map.union overrides.toolEnabledOverridesOverride config.toolEnabledOverrides
    }

lookupOptionalEnvParsed :: String -> (String -> Maybe a) -> IO (Either McpConfigError (Maybe a))
lookupOptionalEnvParsed envName parseValue = do
  maybeRawValue <- lookupEnv envName
  pure $
    case maybeRawValue of
      Nothing ->
        Right Nothing
      Just rawValue ->
        case parseValue rawValue of
          Just value ->
            Right (Just value)
          Nothing ->
            Left (InvalidMcpEnvironmentVariable envName rawValue "one of true, false, 1, 0, yes, no, on, or off")

parseBool :: String -> Maybe Bool
parseBool rawValue =
  case T.toLower (T.strip (T.pack rawValue)) of
    "1" -> Just True
    "true" -> Just True
    "yes" -> Just True
    "on" -> Just True
    "0" -> Just False
    "false" -> Just False
    "no" -> Just False
    "off" -> Just False
    _ -> Nothing

mcpSectionValue :: J.Value -> J.Value
mcpSectionValue = \case
  J.Object obj ->
    fromMaybe (J.Object mempty) (JKM.lookup "mcp" obj)
  _ ->
    J.Object mempty

maybeToList :: Maybe a -> [a]
maybeToList = \case
  Nothing -> []
  Just value -> [value]

toSnakeUpper :: String -> String
toSnakeUpper raw = dropWhile (== '_') (go Nothing raw)
  where
    go _ [] = []
    go previousChar (currentChar : rest)
      | isAlphaNum currentChar =
          let boundary =
                case previousChar of
                  Just prev
                    | isAlphaNum prev ->
                        isWordBoundary prev currentChar
                  _ ->
                    False
              prefix = if boundary then "_" else ""
           in prefix <> [toUpper currentChar] <> go (Just currentChar) rest
      | otherwise =
          case previousChar of
            Just prev
              | isAlphaNum prev ->
                  "_" <> go (Just '_') rest
            _ ->
              go previousChar rest

    isWordBoundary prev current =
      (isLower prev && isUpper current)
        || (isDigit prev && isAlpha current)
        || (isAlpha prev && isDigit current)

quote :: Text -> Text
quote value =
  "\"" <> value <> "\""

instance J.FromJSON McpConfigOverrides where
  parseJSON =
    J.withObject "McpConfigOverrides" \obj ->
      McpConfigOverrides
        <$> obj .:? "enable-definition-knowledge-cache"
        <*> (fmap Just <$> obj .:? "feedback-file")
        <*> obj .:? "tools" J..!= mempty
