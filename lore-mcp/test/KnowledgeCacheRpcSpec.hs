module KnowledgeCacheRpcSpec
  ( spec,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON)
import qualified Data.Aeson as J
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Lore.JsonRpc.Server (JsonRpcError (..), JsonRpcResponse (..))
import Lore.Mcp.KnowledgeCacheRpc (knowledgeCacheRequestHandlers)
import Lore.Mcp.Monad (LoreMcpMonad, getSentDefinitionHashes)
import Lore.Mcp.Protocol.Request (McpRequest (..))
import Lore.Mcp.Protocol.Server
  ( CustomRequestHandler (..),
    McpServer (..),
    handleMcpRequest,
    initialMcpServerState,
  )
import Lore.Mcp.Tools.GetDefinitions.Cached (cachedGetDefinitionsTool)
import Lore.Mcp.Tools.NotifyKnowledgeReset (notifyKnowledgeResetTool)
import Lore.Tools.Render.Markdown (renderLoreDocMarkdown)
import McpTestSupport (callToolWithArgs, callToolWithoutArgs, fixtureLoreMcpWithCache, loadFixtureHomeModules)
import Test.Hspec

newtype GetCachedDefinitionsResult = GetCachedDefinitionsResult
  { hashes :: [Text]
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON GetCachedDefinitionsResult

newtype SetCachedDefinitionsResult = SetCachedDefinitionsResult
  { cachedDefinitionCount :: Int
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON SetCachedDefinitionsResult

spec :: Spec
spec =
  describe "knowledge-cache custom JSON-RPC handlers" do
    it "returns an empty hash list when cache is empty" do
      result <-
        fixtureLoreMcpWithCache True do
          getCachedDefinitions Nothing

      result `shouldBe` Right (GetCachedDefinitionsResult [])

    it "accepts omitted, null, and empty-object params for get" do
      (omitted, nullParams, emptyObject) <-
        fixtureLoreMcpWithCache True do
          omitted <- getCachedDefinitions Nothing
          nullParams <- getCachedDefinitions (Just J.Null)
          emptyObject <- getCachedDefinitions (Just (J.object []))
          pure (omitted, nullParams, emptyObject)

      omitted `shouldBe` Right (GetCachedDefinitionsResult [])
      nullParams `shouldBe` Right (GetCachedDefinitionsResult [])
      emptyObject `shouldBe` Right (GetCachedDefinitionsResult [])

    it "rejects non-empty object params for get with -32602" do
      result <-
        fixtureLoreMcpWithCache True do
          runKnowledgeRequest "lore/knowledge/getCachedDefinitions" (Just (J.object ["unexpected" J..= True]))

      result `shouldSatisfy` isInvalidParams

    it "returns populated hashes in deterministic sorted order" do
      result <-
        fixtureLoreMcpWithCache True do
          _ <- setCachedDefinitions ["b", "a"]
          getCachedDefinitions Nothing

      result `shouldBe` Right (GetCachedDefinitionsResult ["a", "b"])

    it "setter replaces the complete cache" do
      (setResult, readBack) <-
        fixtureLoreMcpWithCache True do
          _ <- setCachedDefinitions ["a", "b"]
          setResult <- setCachedDefinitions ["c"]
          readBack <- getCachedDefinitions Nothing
          pure (setResult, readBack)

      setResult `shouldBe` Right (SetCachedDefinitionsResult 1)
      readBack `shouldBe` Right (GetCachedDefinitionsResult ["c"])

    it "empty list clears the cache" do
      readBack <-
        fixtureLoreMcpWithCache True do
          _ <- setCachedDefinitions ["a", "b"]
          _ <- setCachedDefinitions []
          getCachedDefinitions Nothing

      readBack `shouldBe` Right (GetCachedDefinitionsResult [])

    it "deduplicates duplicate strings using Set.fromList" do
      (setResult, readBack) <-
        fixtureLoreMcpWithCache True do
          setResult <- setCachedDefinitions ["dup", "dup", "x"]
          readBack <- getCachedDefinitions Nothing
          pure (setResult, readBack)

      setResult `shouldBe` Right (SetCachedDefinitionsResult 2)
      readBack `shouldBe` Right (GetCachedDefinitionsResult ["dup", "x"])

    it "accepts arbitrary strings without validation" do
      readBack <-
        fixtureLoreMcpWithCache True do
          _ <- setCachedDefinitions ["", "NOT_A_HASH", "MiXeD", "ß"]
          getCachedDefinitions Nothing

      readBack `shouldBe` Right (GetCachedDefinitionsResult ["", "MiXeD", "NOT_A_HASH", "ß"])

    it "returns -32602 when hashes field is missing" do
      result <-
        fixtureLoreMcpWithCache True do
          runKnowledgeRequest "lore/knowledge/setCachedDefinitions" (Just (J.object []))

      result `shouldSatisfy` isInvalidParams

    it "returns -32602 when hashes is not an array" do
      result <-
        fixtureLoreMcpWithCache True do
          runKnowledgeRequest "lore/knowledge/setCachedDefinitions" (Just (J.object ["hashes" J..= (1 :: Int)]))

      result `shouldSatisfy` isInvalidParams

    it "keeps cache unchanged when setter params fail JSON decoding" do
      (result, hashesAfterFailure) <-
        fixtureLoreMcpWithCache True do
          _ <- setCachedDefinitions ["keep"]
          result <- runKnowledgeRequest "lore/knowledge/setCachedDefinitions" (Just (J.object ["hashes" J..= (1 :: Int)]))
          hashesAfterFailure <- getSentDefinitionHashes
          pure (result, hashesAfterFailure)

      result `shouldSatisfy` isInvalidParams
      hashesAfterFailure `shouldBe` Set.fromList ["keep"]

    it "returns -32601 for both methods when knowledge caching is disabled" do
      (getResponse, setResponse) <-
        fixtureLoreMcpWithCache False do
          state <- liftIO initialMcpServerState
          let server =
                McpServer
                  { name = "test",
                    initialize = pure (),
                    tools = [],
                    customRequestHandlers = Map.empty,
                    renderer = renderLoreDocMarkdown
                  }
          _ <- handleMcpRequest state server Initialize
          getResponse <- handleMcpRequest state server (OtherRequest "lore/knowledge/getCachedDefinitions" Nothing)
          setResponse <- handleMcpRequest state server (OtherRequest "lore/knowledge/setCachedDefinitions" (Just (J.object ["hashes" J..= ([] :: [Text])])))
          pure (getResponse, setResponse)

      getResponse `shouldBe` methodNotFoundResponse "lore/knowledge/getCachedDefinitions"
      setResponse `shouldBe` methodNotFoundResponse "lore/knowledge/setCachedDefinitions"

    it "notifyKnowledgeReset clears state previously installed through setter" do
      readBack <-
        fixtureLoreMcpWithCache True do
          _ <- setCachedDefinitions ["a", "b"]
          _ <- callToolWithoutArgs notifyKnowledgeResetTool
          getCachedDefinitions Nothing

      readBack `shouldBe` Right (GetCachedDefinitionsResult [])

    it "supports fork restoration by replacing cache state snapshots" do
      (fromABA, fromABB, fromABC, fromACA, fromACC, fromACB) <-
        fixtureLoreMcpWithCache True do
          loadFixtureHomeModules

          _ <- callToolWithArgs (cachedGetDefinitionsTool True) (getDefinitionArgs ["lookupOrZero"])
          stateAfterA <- requireRight =<< getCachedDefinitions Nothing

          _ <- callToolWithArgs (cachedGetDefinitionsTool True) (getDefinitionArgs ["lookupOrOne"])
          stateAfterAB <- requireRight =<< getCachedDefinitions Nothing

          _ <- requireRight =<< setCachedDefinitions stateAfterA.hashes
          _ <- callToolWithArgs (cachedGetDefinitionsTool True) (getDefinitionArgs ["lookupWithWhere"])
          stateAfterAC <- requireRight =<< getCachedDefinitions Nothing

          _ <- requireRight =<< setCachedDefinitions stateAfterAB.hashes
          fromABA <- callToolWithArgs (cachedGetDefinitionsTool True) (getDefinitionArgs ["lookupOrZero"])
          fromABB <- callToolWithArgs (cachedGetDefinitionsTool True) (getDefinitionArgs ["lookupOrOne"])
          fromABC <- callToolWithArgs (cachedGetDefinitionsTool True) (getDefinitionArgs ["lookupWithWhere"])

          _ <- requireRight =<< setCachedDefinitions stateAfterAC.hashes
          fromACA <- callToolWithArgs (cachedGetDefinitionsTool True) (getDefinitionArgs ["lookupOrZero"])
          fromACC <- callToolWithArgs (cachedGetDefinitionsTool True) (getDefinitionArgs ["lookupWithWhere"])
          fromACB <- callToolWithArgs (cachedGetDefinitionsTool True) (getDefinitionArgs ["lookupOrOne"])

          pure (fromABA, fromABB, fromABC, fromACA, fromACC, fromACB)

      fromABA `shouldContainText` "Demo: lookupOrZero"
      fromABB `shouldContainText` "Demo: lookupOrOne"
      fromABC `shouldContainText` "lookupWithWhere :: [(String, Int)] -> String -> Int"

      fromACA `shouldContainText` "Demo: lookupOrZero"
      fromACC `shouldContainText` "Demo: lookupWithWhere"
      fromACB `shouldContainText` "lookupOrOne :: [(String, Int)] -> String -> Int"

getCachedDefinitions :: Maybe J.Value -> LoreMcpMonad (Either JsonRpcError GetCachedDefinitionsResult)
getCachedDefinitions params = do
  response <- runKnowledgeRequest "lore/knowledge/getCachedDefinitions" params
  pure (response >>= decodeResponseValue)

setCachedDefinitions :: [Text] -> LoreMcpMonad (Either JsonRpcError SetCachedDefinitionsResult)
setCachedDefinitions hashes = do
  response <- runKnowledgeRequest "lore/knowledge/setCachedDefinitions" (Just (J.object ["hashes" J..= hashes]))
  pure (response >>= decodeResponseValue)

runKnowledgeRequest :: Text -> Maybe J.Value -> LoreMcpMonad (Either JsonRpcError J.Value)
runKnowledgeRequest method params =
  case Map.lookup method (knowledgeCacheRequestHandlers :: Map.Map Text (CustomRequestHandler LoreMcpMonad)) of
    Nothing -> error ("missing knowledge-cache handler for method: " <> show method)
    Just handler ->
      runCustomRequestHandler handler params

decodeResponseValue :: (FromJSON a) => J.Value -> Either JsonRpcError a
decodeResponseValue value =
  case J.fromJSON value of
    J.Error err ->
      Left
        JsonRpcError
          { jsonRpcErrorCode = -32603,
            jsonRpcErrorMessage = "internal error: invalid handler response payload: " <> T.pack err
          }
    J.Success decoded ->
      Right decoded

isInvalidParams :: Either JsonRpcError a -> Bool
isInvalidParams (Left JsonRpcError {jsonRpcErrorCode}) =
  jsonRpcErrorCode == -32602
isInvalidParams (Right _) =
  False

requireRight :: (Show e) => Either e a -> LoreMcpMonad a
requireRight (Left err) =
  error ("expected successful response, got: " <> show err)
requireRight (Right result) =
  pure result

getDefinitionArgs :: [Text] -> J.Value
getDefinitionArgs symbols =
  J.object
    [ "symbols" J..= symbols,
      "expansion" J..= ("None" :: Text)
    ]

shouldContainText :: Text -> Text -> Expectation
shouldContainText haystack needle =
  haystack `shouldSatisfy` T.isInfixOf needle

methodNotFoundResponse :: Text -> JsonRpcResponse
methodNotFoundResponse method =
  JsonRpcErrorResponse
    JsonRpcError
      { jsonRpcErrorCode = -32601,
        jsonRpcErrorMessage = "method not found: " <> method
      }
