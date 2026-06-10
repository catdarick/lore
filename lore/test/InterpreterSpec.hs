module InterpreterSpec
  ( spec,
  )
where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified GHC
import qualified GHC.Utils.Outputable as Outputable
import Lore.Diagnostics (Diagnostic (..))
import Lore.Interpreter (executeStatement, getTypeOfExpression)
import Lore.Session (SessionConfig (..), defaultSessionConfig)
import System.FilePath ((</>))
import Test.Hspec
import TestSupport (fixtureLore, fixtureLoreAt, fixtureLoreAtWithConfig, withFixtureCopy, withFixtureSpec)

spec :: Spec
spec =
  withFixtureSpec do
    describe "interpreter" do
      it "executes statements against project modules loaded as default imports" \fixture -> do
        result <-
          fixtureLore fixture do
            executeStatement "lookupOrZero [(\"left\", 3)] \"left\""

        result `shouldBe` Right "3"

      it "uses symbols from multiple project modules without explicit imports" \fixture -> do
        result <-
          fixtureLore fixture do
            executeStatement "(crossModuleSeed, supportStep 4)"

        result `shouldBe` Right "(5,9)"

      it "returns the inferred type of an expression in the default project context" \fixture -> do
        result <-
          fixtureLore fixture do
            getTypeOfExpression "lookupOrZero [(\"left\", 3)]"

        renderType result `shouldBe` "String -> Int"

      it "returns diagnostics instead of throwing for parse failures" \fixture -> do
        result <-
          fixtureLore fixture do
            executeStatement "map (+1 [1, 2 :: Int]"

        case result of
          Left diagnostics -> do
            diagnostics `shouldSatisfy` (not . null)
            any (\diagnostic -> "parse error" `T.isInfixOf` diagnostic.diagnosticMessage) diagnostics `shouldBe` True
          Right rendered ->
            expectationFailure ("Expected parse failure, got: " <> show rendered)

      it "executes IO expressions instead of wrapping them in show" \fixture -> do
        result <-
          fixtureLore fixture do
            executeStatement "pure (3 :: Int)"

        result `shouldBe` Right "3"

      it "captures stdout produced by IO expressions" \fixture -> do
        result <-
          fixtureLore fixture do
            executeStatement "putStrLn \"123\\n345\""

        result `shouldBe` Right "123\n345"

      it "returns combined output for IO expressions that also produce a final result" \fixture -> do
        result <-
          fixtureLore fixture do
            executeStatement "print \"side\" >> pure (3 :: Int)"

        result `shouldBe` Right "\"side\"\n3"

      it "keeps successfully loaded modules in context even when another module fails to compile" \fixture -> do
        withFixtureCopy fixture \fixtureRoot -> do
          TIO.writeFile
            (fixtureRoot </> "src" </> "Broken.hs")
            "module Broken where\n\nbrokenValue = doesNotExist\n"

          result <-
            fixtureLoreAt fixture fixtureRoot do
              executeStatement "lookupOrZero [(\"left\", 7)] \"left\""

          result `shouldBe` Right "7"

      it "can use a custom Prelude import module" \fixture -> do
        withFixtureCopy fixture \fixtureRoot -> do
          TIO.writeFile
            (fixtureRoot </> "src" </> "CustomPrelude.hs")
            "module CustomPrelude (module Prelude, nub) where\n\nimport Prelude\nimport Data.List (nub)\n"

          let sessionConfig = sessionConfigWithCustomPrelude (Just "CustomPrelude")

          result <-
            fixtureLoreAtWithConfig fixture sessionConfig fixtureRoot do
              executeStatement "nub ['a', 'a', 'b']"

          result `shouldBe` Right "\"ab\""

          ty <-
            fixtureLoreAtWithConfig fixture sessionConfig fixtureRoot do
              getTypeOfExpression "nub ['a', 'a', 'b']"

          renderType ty `shouldBe` "[Char]"

sessionConfigWithCustomPrelude :: Maybe T.Text -> SessionConfig
sessionConfigWithCustomPrelude customPrelude =
  defaultSessionConfig
    { customPrelude = customPrelude
    }

renderType :: GHC.Type -> String
renderType =
  Outputable.showSDocUnsafe . Outputable.ppr
