module ToolBlockedSpec
  ( spec,
  )
where

import Lore.Mcp.Internal.LoreDoc (ToLoreDoc (toLoreDoc))
import Lore.Mcp.Internal.LoreDoc.Markdown (renderLoreDocMarkdown)
import Lore.Mcp.Tools.Shared (ToolBlocked (..))
import Test.Hspec

spec :: Spec
spec =
  describe "ToolBlocked rendering" do
    it "renders shared interpreter blocked message" do
      renderLoreDocMarkdown (toLoreDoc InterpreterContextNotReady)
        `shouldBe` "Interpreter context is not ready. Run reloadHomeModules again."
