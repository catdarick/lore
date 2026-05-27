module ToolBlockedSpec
  ( spec,
  )
where

import Lore.Tools.Render.Doc (ToLoreDoc (toLoreDoc))
import Lore.Tools.Render.Markdown (renderLoreDocMarkdown)
import Lore.Tools.Result (ToolBlocked (..))
import Test.Hspec

spec :: Spec
spec =
  describe "ToolBlocked rendering" do
    it "renders shared interpreter blocked message" do
      renderLoreDocMarkdown (toLoreDoc InterpreterContextNotReady)
        `shouldBe` "Interpreter context is not ready. Run reloadHomeModules again."
