module ProtocolServerSpec
  ( spec,
  )
where

import Control.Exception (throwIO)
import Data.Aeson (object, (.=))
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Lore.JsonRpc.Server (JsonRpcError (..), JsonRpcResponse (..))
import Lore.Mcp.Config (CustomCommandToolArgConfig (..), CustomCommandToolArgQuoteMode (..), CustomCommandToolConfig (..))
import Lore.Mcp.Protocol.Request (McpRequest (..), McpRequest'Tools (..))
import Lore.Mcp.Protocol.Server
  ( CustomRequestHandler (..),
    ExecutedToolResult (..),
    McpServer (..),
    executeToolCall,
    handleMcpRequest,
    initialMcpServerState,
  )
import Lore.Mcp.Tools.CustomCommand (customCommandTool)
import Lore.Tools.Render.Markdown (renderLoreDocMarkdown)
import Test.Hspec

spec :: Spec
spec =
  describe "custom JSON-RPC request dispatch" do
    it "calls a registered custom handler" do
      calledRef <- newIORef False
      let server =
            testServer
              ( Map.fromList
                  [ ( "custom/ping",
                      CustomRequestHandler $ \_ -> do
                        writeIORef calledRef True
                        pure (Right (object ["ok" .= True]))
                    )
                  ]
              )
      responses <- runRequests server [Initialize, OtherRequest "custom/ping" Nothing]

      called <- readIORef calledRef
      called `shouldBe` True
      responses !! 1 `shouldBe` JsonRpcResult (object ["ok" .= True])

    it "passes original params to the custom handler unchanged" do
      paramsRef <- newIORef Nothing
      let inputParams = Just (J.Array (V.fromList [J.Number 1, J.String "two"]))
          server =
            testServer
              ( Map.fromList
                  [ ( "custom/echoParams",
                      CustomRequestHandler $ \params -> do
                        writeIORef paramsRef (Just params)
                        pure (Right J.Null)
                    )
                  ]
              )
      _ <- runRequests server [Initialize, OtherRequest "custom/echoParams" inputParams]

      capturedParams <- readIORef paramsRef
      capturedParams `shouldBe` Just inputParams

    it "returns successful custom handler output as JsonRpcResult" do
      let server =
            testServer
              ( Map.fromList
                  [ ( "custom/result",
                      CustomRequestHandler $ \_ ->
                        pure (Right (J.String "done"))
                    )
                  ]
              )
      responses <- runRequests server [Initialize, OtherRequest "custom/result" Nothing]

      responses !! 1 `shouldBe` JsonRpcResult (J.String "done")

    it "converts handler-reported errors into JsonRpcErrorResponse" do
      let reportedError =
            JsonRpcError
              { jsonRpcErrorCode = -32602,
                jsonRpcErrorMessage = "bad input"
              }
          server =
            testServer
              ( Map.fromList
                  [ ( "custom/fail",
                      CustomRequestHandler $ \_ ->
                        pure (Left reportedError)
                    )
                  ]
              )
      responses <- runRequests server [Initialize, OtherRequest "custom/fail" Nothing]

      responses !! 1 `shouldBe` JsonRpcErrorResponse reportedError

    it "returns method not found for unknown custom methods" do
      let server = testServer Map.empty
      responses <- runRequests server [Initialize, OtherRequest "custom/missing" Nothing]

      responses !! 1
        `shouldBe` JsonRpcErrorResponse
          JsonRpcError
            { jsonRpcErrorCode = -32601,
              jsonRpcErrorMessage = "method not found: custom/missing"
            }

    it "requires initialization for custom requests" do
      let server = testServer Map.empty
      responses <- runRequests server [OtherRequest "custom/method" Nothing]

      responses !! 0
        `shouldBe` JsonRpcErrorResponse
          JsonRpcError
            { jsonRpcErrorCode = -32002,
              jsonRpcErrorMessage = "server not initialized"
            }

    it "returns internal error when a custom handler throws unexpectedly" do
      let server =
            testServer
              ( Map.fromList
                  [ ( "custom/boom",
                      CustomRequestHandler $ \_ -> do
                        throwIO (userError "boom")
                    )
                  ]
              )
      responses <- runRequests server [Initialize, OtherRequest "custom/boom" Nothing]

      case responses !! 1 of
        JsonRpcErrorResponse JsonRpcError {jsonRpcErrorCode} ->
          jsonRpcErrorCode `shouldBe` -32603
        otherResponse ->
          expectationFailure ("expected JsonRpcErrorResponse, got: " <> show otherResponse)

    it "keeps custom methods out of tools/list" do
      let server =
            testServer
              ( Map.fromList
                  [ ( "lore/knowledge/getCachedDefinitions",
                      CustomRequestHandler $ \_ -> pure (Right J.Null)
                    )
                  ]
              )
      responses <- runRequests server [Initialize, Tools ToolsList]

      case responses !! 1 of
        JsonRpcResult (J.Object obj) ->
          KM.lookup "tools" obj `shouldBe` Just (J.Array V.empty)
        otherResponse ->
          expectationFailure ("expected JsonRpcResult object, got: " <> show otherResponse)

    it "executes custom command tools with shell-quoted arguments" do
      result <-
        executeToolCall
          [ customCommandTool
              CustomCommandToolConfig
                { name = "echoLiteral",
                  description = Nothing,
                  command = "printf '%s' @{value}",
                  args = [stringArg "value"]
                }
          ]
          renderLoreDocMarkdown
          "echoLiteral"
          (Just (object ["value" .= ("hello; exit 7" :: Text)]))

      case result of
        Right ExecutedToolResult {executedToolContent} -> do
          executedToolContent `shouldSatisfy` T.isInfixOf "exit: 0"
          executedToolContent `shouldSatisfy` T.isInfixOf "hello; exit 7"
        Left err ->
          expectationFailure ("expected successful custom command, got: " <> show err)

    it "passes nullable custom command arguments as empty strings" do
      result <-
        executeToolCall
          [ customCommandTool
              CustomCommandToolConfig
                { name = "nullableEcho",
                  description = Nothing,
                  command = "printf '<%s>' @{value}",
                  args = [nullableArg "value"]
                }
          ]
          renderLoreDocMarkdown
          "nullableEcho"
          (Just (object ["value" .= J.Null]))

      case result of
        Right ExecutedToolResult {executedToolContent} -> do
          executedToolContent `shouldSatisfy` T.isInfixOf "exit: 0"
          executedToolContent `shouldSatisfy` T.isInfixOf "<>"
        Left err ->
          expectationFailure ("expected successful custom command, got: " <> show err)

    it "escapes double quotes for custom command args that request quote escaping" do
      result <-
        executeToolCall
          [ customCommandTool
              CustomCommandToolConfig
                { name = "quoteEscape",
                  description = Nothing,
                  command = "printf '%s' @{value}",
                  args = [(stringArg "value") {escapeQuotes = True}]
                }
          ]
          renderLoreDocMarkdown
          "quoteEscape"
          (Just (object ["value" .= ("say \"hello\"" :: Text)]))

      case result of
        Right ExecutedToolResult {executedToolContent} -> do
          executedToolContent `shouldSatisfy` T.isInfixOf "exit: 0"
          executedToolContent `shouldSatisfy` T.isInfixOf "say \\\"hello\\\""
        Left err ->
          expectationFailure ("expected successful custom command, got: " <> show err)

    it "passes unquoted custom command args directly when quote mode is none" do
      result <-
        executeToolCall
          [ customCommandTool
              CustomCommandToolConfig
                { name = "directArgs",
                  description = Nothing,
                  command = "printf '%s' @{extraArgs}",
                  args = [(stringArg "extraArgs") {quoteMode = CustomCommandToolArgQuoteNone}]
                }
          ]
          renderLoreDocMarkdown
          "directArgs"
          (Just (object ["extraArgs" .= ("first second" :: Text)]))

      case result of
        Right ExecutedToolResult {executedToolContent} -> do
          executedToolContent `shouldSatisfy` T.isInfixOf "exit: 0"
          executedToolContent `shouldSatisfy` T.isInfixOf "first"
          executedToolContent `shouldNotSatisfy` T.isInfixOf "first second"
        Left err ->
          expectationFailure ("expected successful custom command, got: " <> show err)

runRequests :: McpServer IO -> [McpRequest] -> IO [JsonRpcResponse]
runRequests server requests = do
  state <- initialMcpServerState
  mapM (handleMcpRequest state server) requests

testServer :: Map.Map Text (CustomRequestHandler IO) -> McpServer IO
testServer customRequestHandlers =
  McpServer
    { name = "test",
      initialize = pure (),
      tools = [],
      customRequestHandlers,
      renderer = renderLoreDocMarkdown
    }

stringArg :: Text -> CustomCommandToolArgConfig
stringArg argName =
  CustomCommandToolArgConfig
    { name = argName,
      description = Nothing,
      nullable = False,
      escapeQuotes = False,
      quoteMode = CustomCommandToolArgQuoteSingle
    }

nullableArg :: Text -> CustomCommandToolArgConfig
nullableArg argName =
  CustomCommandToolArgConfig
    { name = argName,
      description = Nothing,
      nullable = True,
      escapeQuotes = False,
      quoteMode = CustomCommandToolArgQuoteSingle
    }
