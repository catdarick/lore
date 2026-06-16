module VersionSpec
  ( spec,
  )
where

import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import GHC.Version (cProjectVersion)
import Lore.JsonRpc.Server (JsonRpcResponse (..))
import Lore.Mcp.Protocol.Request (McpRequest (..))
import Lore.Mcp.Protocol.Server (McpServer (..), handleMcpRequest, initialMcpServerState)
import Lore.Mcp.Version (ghcVersionText, loreVersionText, targetText, versionJson)
import Lore.Tools.Render.Markdown (renderLoreDocMarkdown)
import Test.Hspec

spec :: Spec
spec =
  describe "lore-mcp version metadata" do
    it "versionJson is valid JSON and reports generated package/build metadata" do
      case J.decode (J.encode versionJson) of
        Nothing -> expectationFailure "versionJson did not encode as JSON"
        Just (J.Object obj) -> do
          KM.lookup "ghcVersion" obj `shouldBe` Just (J.String ghcVersionText)
          ghcVersionText `shouldBe` T.pack cProjectVersion
          KM.lookup "loreVersion" obj `shouldBe` Just (J.String loreVersionText)
          KM.lookup "target" obj `shouldBe` Just (J.String targetText)
        Just other -> expectationFailure ("expected object, got " <> BL8.unpack (J.encode other))

    it "MCP initialize uses generated package version metadata" do
      state <- initialMcpServerState
      let server = McpServer {name = "test", initialize = pure (), tools = [], customRequestHandlers = Map.empty, renderer = renderLoreDocMarkdown}
      response <- handleMcpRequest state server Initialize
      case response of
        JsonRpcResult (J.Object obj) ->
          case KM.lookup "serverInfo" obj of
            Just (J.Object serverInfo) -> KM.lookup "version" serverInfo `shouldBe` Just (J.String loreVersionText)
            _ -> expectationFailure "missing serverInfo"
        other -> expectationFailure ("expected initialize result, got " <> show other)
