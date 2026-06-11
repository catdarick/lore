module StructuredToolRpcSpec
  ( spec,
  )
where

import Control.Exception (throwIO)
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Lore.JsonRpc.Server (JsonRpcError (..), JsonRpcResponse (..))
import Lore.Mcp.Internal.Annotated (Field, FieldType (..))
import Lore.Mcp.Internal.Tool (SomeTool (..), ToolWithArgs (..), ToolWithoutArgs (..))
import Lore.Mcp.Protocol.Request (McpRequest (..), McpRequest'Tools (..))
import Lore.Mcp.Protocol.Server
  ( McpServer (..),
    handleMcpRequest,
    initialMcpServerState,
  )
import Lore.Mcp.StructuredToolRpc (structuredToolRequestHandlers)
import Lore.Tools.Render.Doc (LoreDoc, paragraph)
import Lore.Tools.Render.Markdown (renderLoreDocMarkdown)
import Test.Hspec

data EchoArgs (fieldType :: FieldType) = EchoArgs
  { value :: Field fieldType Int
  }
  deriving stock (Generic)

instance J.FromJSON (EchoArgs 'ValueType)

instance ToSchema (EchoArgs 'MetadataType)

spec :: Spec
spec =
  describe "lore/tools/callStructured" do
    it "returns identical content and null structuredContent for tools without structured projection" do
      invocationCount <- newIORef (0 :: Int)
      let server =
            testServer
              [ SomeToolWithoutArgs
                  ToolWithoutArgs
                    { name = "plain",
                      description = Nothing,
                      handler = do
                        modifyIORef' invocationCount (+ 1)
                        pure (paragraph "plain-result")
                    }
              ]

      responses <-
        runRequests
          server
          [ Initialize,
            Tools (ToolsCall "plain" Nothing),
            structuredToolsCall "plain" Nothing
          ]

      invocationTotal <- readIORef invocationCount
      invocationTotal `shouldBe` 2

      publicContent <- extractContentText (responses !! 1)
      privateContent <- extractContentText (responses !! 2)
      privateStructured <- extractStructuredContent (responses !! 2)

      publicContent `shouldBe` privateContent
      privateStructured `shouldBe` Just J.Null

    it "returns non-null structuredContent when the tool declares a structured projection" do
      let server =
            testServer
              [ SomeToolWithArgsStructured
                  ToolWithArgs
                    { name = "echo",
                      description = Nothing,
                      handler = \EchoArgs {value} ->
                        pure (paragraph ("value=" <> T.pack (show value)))
                    }
                  (\EchoArgs {value} _ -> J.object ["value" J..= value])
              ]

      responses <-
        runRequests
          server
          [ Initialize,
            structuredToolsCall "echo" (Just (J.object ["value" J..= (7 :: Int)]))
          ]

      structuredContent <- extractStructuredContent (responses !! 1)
      structuredContent `shouldBe` Just (J.object ["value" J..= (7 :: Int)])

    it "executes a tool handler exactly once per request" do
      invocationCount <- newIORef (0 :: Int)
      let server =
            testServer
              [ SomeToolWithoutArgs
                  ToolWithoutArgs
                    { name = "count",
                      description = Nothing,
                      handler = do
                        modifyIORef' invocationCount (+ 1)
                        pure (paragraph "ok")
                    }
              ]

      responses <-
        runRequests
          server
          [ Initialize,
            structuredToolsCall "count" Nothing
          ]

      responses !! 1 `shouldSatisfy` isJsonRpcResult
      invocationTotal <- readIORef invocationCount
      invocationTotal `shouldBe` 1

    it "returns unknown-tool errors identically for public and private calls" do
      let server = testServer []
      responses <-
        runRequests
          server
          [ Initialize,
            Tools (ToolsCall "missing" Nothing),
            structuredToolsCall "missing" Nothing
          ]

      responses !! 1 `shouldBe` responses !! 2

    it "returns missing-argument errors identically for public and private calls" do
      let server = testServer [echoTool]
      responses <-
        runRequests
          server
          [ Initialize,
            Tools (ToolsCall "echo" Nothing),
            structuredToolsCall "echo" Nothing
          ]

      responses !! 1 `shouldBe` responses !! 2

    it "returns malformed-argument errors identically for public and private calls" do
      let server = testServer [echoTool]
          malformedArgs = Just (J.object ["value" J..= ("oops" :: String)])
      responses <-
        runRequests
          server
          [ Initialize,
            Tools (ToolsCall "echo" malformedArgs),
            structuredToolsCall "echo" malformedArgs
          ]

      responses !! 1 `shouldBe` responses !! 2

    it "returns thrown-handler exceptions identically for public and private calls" do
      let server =
            testServer
              [ SomeToolWithoutArgs
                  ToolWithoutArgs
                    { name = "boom",
                      description = Nothing,
                      handler = (throwIO (userError "boom") :: IO LoreDoc)
                    }
              ]
      responses <-
        runRequests
          server
          [ Initialize,
            Tools (ToolsCall "boom" Nothing),
            structuredToolsCall "boom" Nothing
          ]

      responses !! 1 `shouldBe` responses !! 2

    it "requires initialization before private structured requests" do
      let server = testServer [echoTool]
      responses <- runRequests server [structuredToolsCall "echo" (Just (J.object ["value" J..= (1 :: Int)]))]

      responses !! 0
        `shouldBe` JsonRpcErrorResponse
          JsonRpcError
            { jsonRpcErrorCode = -32002,
              jsonRpcErrorMessage = "server not initialized"
            }

    it "keeps the private structured method out of tools/list" do
      let server = testServer [echoTool]
      responses <- runRequests server [Initialize, Tools ToolsList]

      toolNames <- extractToolNames (responses !! 1)
      toolNames `shouldBe` ["echo"]

echoTool :: SomeTool IO
echoTool =
  SomeToolWithArgs
    ToolWithArgs
      { name = "echo",
        description = Nothing,
        handler = \EchoArgs {value} ->
          pure (paragraph ("value=" <> T.pack (show value)))
      }

structuredToolsCall :: Text -> Maybe J.Value -> McpRequest
structuredToolsCall toolName maybeArgs =
  OtherRequest
    "lore/tools/callStructured"
    (Just callParams)
  where
    callParams =
      case maybeArgs of
        Nothing ->
          J.object ["name" J..= toolName]
        Just args ->
          J.object
            [ "name" J..= toolName,
              "arguments" J..= args
            ]

runRequests :: McpServer IO -> [McpRequest] -> IO [JsonRpcResponse]
runRequests server requests = do
  state <- initialMcpServerState
  mapM (handleMcpRequest state server) requests

testServer :: [SomeTool IO] -> McpServer IO
testServer tools =
  McpServer
    { name = "test",
      initialize = pure (),
      tools,
      customRequestHandlers = structuredToolRequestHandlers tools renderLoreDocMarkdown,
      renderer = renderLoreDocMarkdown
    }

extractContentText :: JsonRpcResponse -> IO Text
extractContentText response =
  case response of
    JsonRpcResult (J.Object obj) ->
      case KM.lookup "content" obj of
        Just (J.Array contentItems)
          | Just (J.Object firstItem) <- contentItems V.!? 0,
            Just (J.String textValue) <- KM.lookup "text" firstItem ->
              pure textValue
        _ ->
          expectationFailure ("unexpected tools/call payload: " <> show response) >> pure ""
    _ ->
      expectationFailure ("expected JsonRpcResult, got: " <> show response) >> pure ""

extractStructuredContent :: JsonRpcResponse -> IO (Maybe J.Value)
extractStructuredContent response =
  case response of
    JsonRpcResult (J.Object obj) ->
      pure (KM.lookup "structuredContent" obj)
    _ ->
      expectationFailure ("expected JsonRpcResult, got: " <> show response) >> pure Nothing

extractToolNames :: JsonRpcResponse -> IO [Text]
extractToolNames response =
  case response of
    JsonRpcResult (J.Object obj) ->
      case KM.lookup "tools" obj of
        Just (J.Array toolsArray) ->
          pure
            [ name
            | J.Object toolObj <- V.toList toolsArray,
              Just (J.String name) <- [KM.lookup "name" toolObj]
            ]
        _ ->
          expectationFailure ("expected tools array, got: " <> show response) >> pure []
    _ ->
      expectationFailure ("expected JsonRpcResult, got: " <> show response) >> pure []

isJsonRpcResult :: JsonRpcResponse -> Bool
isJsonRpcResult = \case
  JsonRpcResult _ -> True
  JsonRpcErrorResponse _ -> False
  JsonRpcNoResponse -> False
