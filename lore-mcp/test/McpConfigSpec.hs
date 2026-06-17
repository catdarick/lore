module McpConfigSpec
  ( spec,
  )
where

import Control.Exception (bracket)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Yaml as Y
import Lore.Config (LoadedConfigDocument (..))
import Lore.Mcp.Config
  ( CustomCommandToolArgQuoteMode (..),
    McpConfig (..),
    McpConfigError (..),
    McpConfigOverrides (..),
    defaultMcpConfig,
    loadMcpEnvironmentOverrides,
    parseMcpYamlConfig,
    resolveMcpConfig,
    toolEnabled,
    toolEnabledEnvVarName,
  )
import qualified Lore.Mcp.Config
import System.Environment (lookupEnv, setEnv, unsetEnv)
import Test.Hspec

spec :: Spec
spec =
  describe "MCP configuration" do
    it "keeps current defaults" do
      defaultMcpConfig.definitionKnowledgeCacheEnabled `shouldBe` False
      defaultMcpConfig.feedbackFilePath `shouldBe` Nothing
      toolEnabled defaultMcpConfig "runTestSuite" `shouldBe` False
      toolEnabled defaultMcpConfig "notifyKnowledgeReset" `shouldBe` True

    it "parses YAML MCP settings" do
      overrides <-
        shouldParseMcpYaml
          "mcp:\n  enable-definition-knowledge-cache: true\n  feedback-file: .lore-work/mcp-feedback.md\n  tools:\n    runTestSuite: true\n    notifyKnowledgeReset: false\n"
      let config = resolveMcpConfig defaultMcpConfig overrides emptyOverrides
      config.definitionKnowledgeCacheEnabled `shouldBe` True
      config.feedbackFilePath `shouldBe` Just ".lore-work/mcp-feedback.md"
      toolEnabled config "runTestSuite" `shouldBe` True
      toolEnabled config "notifyKnowledgeReset" `shouldBe` False

    it "parses custom command tools" do
      overrides <-
        shouldParseMcpYaml
          "mcp:\n  custom-tools:\n    - name: echoArgs\n      description: Echo two arguments\n      command: echo @{first} @{second}\n      args:\n        - first\n        - second\n  tools:\n    echoArgs: true\n"
      let config = resolveMcpConfig defaultMcpConfig overrides emptyOverrides
      map (.name) config.customCommandTools `shouldBe` ["echoArgs"]
      toolEnabled config "echoArgs" `shouldBe` True

    it "parses custom command arg descriptions, nullability, quote escaping, and quote mode" do
      overrides <-
        shouldParseMcpYaml
          "mcp:\n  custom-tools:\n    - name: describeArgs\n      command: echo @{maybeValue}\n      args:\n        - name: maybeValue\n          description: Optional value to print\n          nullable: true\n          escape-quotes: true\n          quote-mode: none\n"
      let config = resolveMcpConfig defaultMcpConfig overrides emptyOverrides
      case config.customCommandTools of
        [tool] ->
          case tool.args of
            [arg] -> do
              arg.name `shouldBe` "maybeValue"
              arg.description `shouldBe` Just "Optional value to print"
              arg.nullable `shouldBe` True
              arg.escapeQuotes `shouldBe` True
              arg.quoteMode `shouldBe` CustomCommandToolArgQuoteNone
            otherArgs ->
              expectationFailure ("expected one custom arg, got: " <> show otherArgs)
        otherTools ->
          expectationFailure ("expected one custom tool, got: " <> show otherTools)

    it "allows a custom command to override runTestSuite" do
      overrides <-
        shouldParseMcpYaml
          "mcp:\n  custom-tools:\n    - name: runTestSuite\n      command: stack test\n      args: []\n"
      map (.name) overrides.customCommandToolsOverride `shouldBe` ["runTestSuite"]

    it "still rejects custom tools that shadow other built-in tools" do
      parseMcpYaml "mcp:\n  custom-tools:\n    - name: notifyKnowledgeReset\n      command: echo nope\n      args: []\n"
        `shouldSatisfy` \case
          Left (DuplicateMcpToolName "lore.yaml" "notifyKnowledgeReset") -> True
          _ -> False

    it "rejects duplicate runTestSuite overrides" do
      parseMcpYaml "mcp:\n  custom-tools:\n    - name: runTestSuite\n      command: echo one\n      args: []\n    - name: runTestSuite\n      command: echo two\n      args: []\n"
        `shouldSatisfy` \case
          Left (DuplicateMcpToolName "lore.yaml" "runTestSuite") -> True
          _ -> False

    it "rejects duplicate custom tool names" do
      parseMcpYaml "mcp:\n  custom-tools:\n    - name: same\n      command: echo one\n      args: []\n    - name: same\n      command: echo two\n      args: []\n"
        `shouldSatisfy` \case
          Left (DuplicateMcpToolName "lore.yaml" "same") -> True
          _ -> False

    it "rejects command placeholders not declared as args" do
      parseMcpYaml "mcp:\n  custom-tools:\n    - name: badPlaceholder\n      command: echo @{missing}\n      args: []\n"
        `shouldSatisfy` \case
          Left (InvalidMcpConfig "lore.yaml" message) -> "undeclared arg \"missing\"" `T.isInfixOf` message
          _ -> False

    it "lets environment variables override YAML" do
      yamlOverrides <-
        shouldParseMcpYaml
          "mcp:\n  enable-definition-knowledge-cache: true\n  feedback-file: yaml-feedback.md\n  tools:\n    runTestSuite: true\n"
      withMcpEnvironment
        [ ("LORE_MCP_ENABLE_DEFINITION_KNOWLEDGE_CACHE", "false"),
          ("LORE_MCP_FEEDBACK_FILE", "env-feedback.md"),
          (toolEnabledEnvVarName "runTestSuite", "false")
        ]
        do
          envOverrides <- shouldLoadMcpEnvironmentOverrides
          let config = resolveMcpConfig defaultMcpConfig yamlOverrides envOverrides
          config.definitionKnowledgeCacheEnabled `shouldBe` False
          config.feedbackFilePath `shouldBe` Just "env-feedback.md"
          toolEnabled config "runTestSuite" `shouldBe` False

    it "rejects unknown YAML tool names" do
      parseMcpYaml "mcp:\n  tools:\n    typoTool: true\n"
        `shouldSatisfy` \case
          Left (UnknownMcpToolName "lore.yaml" "typoTool") -> True
          _ -> False

    it "reports invalid environment booleans" do
      withMcpEnvironment [("LORE_MCP_ENABLE_DEFINITION_KNOWLEDGE_CACHE", "maybe")] do
        result <- loadMcpEnvironmentOverrides knownToolNames
        result
          `shouldSatisfy` \case
            Left (InvalidMcpEnvironmentVariable "LORE_MCP_ENABLE_DEFINITION_KNOWLEDGE_CACHE" "maybe" expectation) ->
              not (T.null expectation)
            _ -> False

emptyOverrides :: Lore.Mcp.Config.McpConfigOverrides
emptyOverrides =
  Lore.Mcp.Config.McpConfigOverrides
    { definitionKnowledgeCacheEnabledOverride = Nothing,
      feedbackFilePathOverride = Nothing,
      toolEnabledOverridesOverride = Map.empty,
      customCommandToolsOverride = []
    }

shouldLoadMcpEnvironmentOverrides :: IO Lore.Mcp.Config.McpConfigOverrides
shouldLoadMcpEnvironmentOverrides = do
  result <- loadMcpEnvironmentOverrides knownToolNames
  case result of
    Left err ->
      ioError (userError ("Expected valid MCP environment overrides, got: " <> show err))
    Right overrides ->
      pure overrides

shouldParseMcpYaml :: BS.ByteString -> IO Lore.Mcp.Config.McpConfigOverrides
shouldParseMcpYaml rawYaml =
  case parseMcpYaml rawYaml of
    Left err ->
      ioError (userError ("Expected valid MCP YAML overrides, got: " <> show err))
    Right overrides ->
      pure overrides

parseMcpYaml :: BS.ByteString -> Either McpConfigError Lore.Mcp.Config.McpConfigOverrides
parseMcpYaml rawYaml = do
  value <-
    case Y.decodeEither' rawYaml of
      Left parseError ->
        Left (InvalidMcpConfig "lore.yaml" (T.pack (Y.prettyPrintParseException parseError)))
      Right parsedValue ->
        Right parsedValue
  parseMcpYamlConfig
    knownToolNames
    LoadedConfigDocument
      { configFilePath = "lore.yaml",
        configFileValue = value
      }

knownToolNames :: Set.Set T.Text
knownToolNames =
  Set.fromList ["runTestSuite", "notifyKnowledgeReset"]

withMcpEnvironment :: [(String, String)] -> IO a -> IO a
withMcpEnvironment overrides action =
  bracket
    saveEnvironment
    restoreEnvironment
    (const (setOverridesAndRun action))
  where
    names =
      [ "LORE_MCP_ENABLE_DEFINITION_KNOWLEDGE_CACHE",
        "LORE_MCP_FEEDBACK_FILE",
        toolEnabledEnvVarName "runTestSuite",
        toolEnabledEnvVarName "notifyKnowledgeReset"
      ]

    setOverridesAndRun bracketedAction = do
      mapM_ unsetEnv names
      mapM_ (uncurry setEnv) overrides
      bracketedAction

    saveEnvironment =
      traverse (\name -> (name,) <$> lookupEnv name) names

    restoreEnvironment savedValues =
      mapM_ restoreEnvVar savedValues

restoreEnvVar :: (String, Maybe String) -> IO ()
restoreEnvVar (name, maybeValue) =
  case maybeValue of
    Nothing -> unsetEnv name
    Just value -> setEnv name value
