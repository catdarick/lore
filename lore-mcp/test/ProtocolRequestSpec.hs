module ProtocolRequestSpec
  ( spec,
  )
where

import Data.Aeson (Value, object, (.=))
import Data.Text (Text)
import Lore.JsonRpc.Server (JsonRpcRequest (..))
import Lore.Mcp.Protocol.Request
  ( McpRequest (..),
    McpRequest'Notification (..),
    McpRequest'Tools (..),
    parseMcpRequest,
  )
import Test.Hspec

spec :: Spec
spec = do
  describe "parseMcpRequest" do
    it "parses initialize requests without params" do
      parseMcpRequest (request "initialize" Nothing)
        `shouldBe` Right Initialize

    it "parses tools/list requests" do
      parseMcpRequest (request "tools/list" Nothing)
        `shouldBe` Right (Tools ToolsList)

    it "parses tools/call requests with optional arguments" do
      parseMcpRequest (request "tools/call" (Just (object ["name" .= ("reloadHomeModules" :: String)])))
        `shouldBe` Right (Tools (ToolsCall "reloadHomeModules" Nothing))

      parseMcpRequest
        ( request
            "tools/call"
            ( Just
                ( object
                    [ "name" .= ("lookupSymbolInfo" :: String),
                      "arguments" .= object ["symbol" .= ("lookupOrZero" :: String)]
                    ]
                )
            )
        )
        `shouldBe` Right (Tools (ToolsCall "lookupSymbolInfo" (Just (object ["symbol" .= ("lookupOrZero" :: String)]))))

    it "rejects tools/call requests with missing or invalid params" do
      parseMcpRequest (request "tools/call" Nothing)
        `shouldBe` Left "method tools/call requires params"

      parseMcpRequest (request "tools/call" (Just (object ["arguments" .= object []])))
        `shouldBe` Left "tools/call.params.name is missing"

      parseMcpRequest (request "tools/call" (Just (object ["name" .= (123 :: Int)])))
        `shouldBe` Left "tools/call.params.name must be a string"

    it "parses initialized and other notifications" do
      parseMcpRequest (request "notifications/initialized" Nothing)
        `shouldBe` Right (Notification Initialized)

      parseMcpRequest (request "notifications/cancelled" Nothing)
        `shouldBe` Right (Notification (OtherNotification "notifications/cancelled"))

    it "routes unknown methods to OtherRequest" do
      parseMcpRequest (request "custom/method" (Just (object [])))
        `shouldBe` Right (OtherRequest "custom/method")

request :: Text -> Maybe Value -> JsonRpcRequest
request method params =
  JsonRpcRequest
    { jsonRpcMethod = method,
      jsonRpcParams = params,
      jsonRpcExpectsResponse = True
    }
