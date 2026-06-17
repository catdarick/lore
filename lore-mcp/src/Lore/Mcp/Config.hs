module Lore.Mcp.Config
  ( McpConfig (..),
    McpConfigOverrides (..),
    CustomCommandToolConfig (..),
    CustomCommandToolArgConfig (..),
    CustomCommandToolArgQuoteMode (..),
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
import qualified Data.Aeson.Types as JT
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
    toolEnabledOverrides :: Map.Map Text Bool,
    customCommandTools :: [CustomCommandToolConfig]
  }
  deriving stock (Eq, Show)

data McpConfigOverrides = McpConfigOverrides
  { definitionKnowledgeCacheEnabledOverride :: Maybe Bool,
    feedbackFilePathOverride :: Maybe (Maybe FilePath),
    toolEnabledOverridesOverride :: Map.Map Text Bool,
    customCommandToolsOverride :: [CustomCommandToolConfig]
  }
  deriving stock (Eq, Show)

data CustomCommandToolConfig = CustomCommandToolConfig
  { name :: Text,
    description :: Maybe Text,
    command :: Text,
    args :: [CustomCommandToolArgConfig]
  }
  deriving stock (Eq, Show)

data CustomCommandToolArgConfig = CustomCommandToolArgConfig
  { name :: Text,
    description :: Maybe Text,
    nullable :: Bool,
    escapeQuotes :: Bool,
    quoteMode :: CustomCommandToolArgQuoteMode
  }
  deriving stock (Eq, Show)

data CustomCommandToolArgQuoteMode
  = CustomCommandToolArgQuoteSingle
  | CustomCommandToolArgQuoteDouble
  | CustomCommandToolArgQuoteNone
  deriving stock (Eq, Show)

data McpConfigError
  = InvalidMcpEnvironmentVariable String String Text
  | InvalidMcpConfig FilePath Text
  | DuplicateMcpToolName FilePath Text
  | UnknownMcpToolName FilePath Text
  deriving stock (Eq, Show)

overridableToolNames :: Set.Set Text
overridableToolNames = Set.singleton "runTestSuite"

defaultMcpConfig :: McpConfig
defaultMcpConfig =
  McpConfig
    { definitionKnowledgeCacheEnabled = False,
      feedbackFilePath = Nothing,
      toolEnabledOverrides = mempty,
      customCommandTools = []
    }

parseMcpYamlConfig :: Set.Set Text -> LoadedConfigDocument -> Either McpConfigError McpConfigOverrides
parseMcpYamlConfig knownToolNames document =
  case J.fromJSON (mcpSectionValue document.configFileValue) of
    J.Error err ->
      Left (InvalidMcpConfig document.configFilePath (T.pack err))
    J.Success overrides ->
      case firstDuplicateOrKnown (knownToolNames `Set.difference` overridableToolNames) customToolNames of
        Just duplicateToolName ->
          Left (DuplicateMcpToolName document.configFilePath duplicateToolName)
        Nothing ->
          case filter (`Set.notMember` allToolNames) (Map.keys overrides.toolEnabledOverridesOverride) of
            unknownToolName : _ ->
              Left (UnknownMcpToolName document.configFilePath unknownToolName)
            [] ->
              Right overrides
      where
        customToolNames = map (.name) overrides.customCommandToolsOverride
        allToolNames = knownToolNames <> Set.fromList customToolNames

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
      <*> pure []

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
  DuplicateMcpToolName path toolName ->
    "Invalid "
      <> T.pack path
      <> " MCP configuration: duplicate tool "
      <> quote toolName
      <> "."
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
        Map.union overrides.toolEnabledOverridesOverride config.toolEnabledOverrides,
      customCommandTools =
        config.customCommandTools <> overrides.customCommandToolsOverride
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
        <*> obj .:? "custom-tools" J..!= []

instance J.FromJSON CustomCommandToolConfig where
  parseJSON =
    J.withObject "CustomCommandToolConfig" \obj -> do
      config <-
        CustomCommandToolConfig
          <$> obj J..: "name"
          <*> obj .:? "description"
          <*> obj J..: "command"
          <*> obj J..: "args"
      validateCustomCommandToolConfig config
      pure config

validateCustomCommandToolConfig :: CustomCommandToolConfig -> JT.Parser ()
validateCustomCommandToolConfig config = do
  whenParser (T.null config.name) "custom tool name must not be empty"
  whenParser (T.null config.command) ("custom tool " <> quote config.name <> " command must not be empty")
  case firstDuplicate argNames of
    Just duplicateArg ->
      failText ("custom tool " <> quote config.name <> " declares duplicate arg " <> quote duplicateArg)
    Nothing ->
      pure ()
  case filter (`Set.notMember` declaredArgs) (extractCommandPlaceholders config.command) of
    undeclaredArg : _ ->
      failText ("custom tool " <> quote config.name <> " command references undeclared arg " <> quote undeclaredArg)
    [] ->
      pure ()
  where
    argNames = map (.name) config.args
    declaredArgs = Set.fromList argNames

instance J.FromJSON CustomCommandToolArgConfig where
  parseJSON = \case
    J.String argName ->
      pure
        CustomCommandToolArgConfig
          { name = argName,
            description = Nothing,
            nullable = False,
            escapeQuotes = False,
            quoteMode = CustomCommandToolArgQuoteSingle
          }
    J.Object obj -> do
      config <-
        CustomCommandToolArgConfig
          <$> obj J..: "name"
          <*> obj .:? "description"
          <*> obj .:? "nullable" J..!= False
          <*> obj .:? "escape-quotes" J..!= False
          <*> obj .:? "quote-mode" J..!= CustomCommandToolArgQuoteSingle
      whenParser (T.null config.name) "custom tool arg name must not be empty"
      pure config
    _ ->
      failText "custom tool args must be strings or objects"

instance J.FromJSON CustomCommandToolArgQuoteMode where
  parseJSON =
    J.withText "CustomCommandToolArgQuoteMode" \case
      "single" -> pure CustomCommandToolArgQuoteSingle
      "double" -> pure CustomCommandToolArgQuoteDouble
      "none" -> pure CustomCommandToolArgQuoteNone
      rawValue -> failText ("custom tool arg quote-mode must be one of single, double, or none, got " <> quote rawValue)

whenParser :: Bool -> Text -> JT.Parser ()
whenParser condition message =
  if condition then failText message else pure ()

failText :: Text -> JT.Parser a
failText = fail . T.unpack

firstDuplicate :: (Ord a) => [a] -> Maybe a
firstDuplicate = go Set.empty
  where
    go _ [] = Nothing
    go seen (x : xs)
      | x `Set.member` seen = Just x
      | otherwise = go (Set.insert x seen) xs

firstDuplicateOrKnown :: (Ord a) => Set.Set a -> [a] -> Maybe a
firstDuplicateOrKnown known = go known
  where
    go _ [] = Nothing
    go seen (x : xs)
      | x `Set.member` seen = Just x
      | otherwise = go (Set.insert x seen) xs

extractCommandPlaceholders :: Text -> [Text]
extractCommandPlaceholders text =
  case T.breakOn "@{" text of
    (_, rest)
      | T.null rest -> []
      | otherwise ->
          let afterOpen = T.drop 2 rest
              (argName, afterName) = T.breakOn "}" afterOpen
           in if T.null afterName
                then []
                else argName : extractCommandPlaceholders (T.drop 1 afterName)
