module InterpreterSpec
  ( spec,
  )
where

import qualified Data.Text.IO as TIO
import qualified GHC
import qualified GHC.Utils.Outputable as Outputable
import Lore.Interpreter (getTypeOfExpression, interpretExpression)
import Lore.Session (PreludeImportRule (..), defaultSessionConfig)
import qualified Lore.Session as Session
import System.FilePath ((</>))
import Test.Hspec
import TestSupport (fixtureLore, fixtureLoreAt, fixtureLoreAtWithConfig, withFixtureCopy)

spec :: Spec
spec =
  describe "interpreter" do
    it "evaluates expressions against project modules loaded as default imports" do
      result <-
        fixtureLore do
          interpretExpression "lookupOrZero [(\"left\", 3)] \"left\""

      result `shouldBe` "3"

    it "uses symbols from multiple project modules without explicit imports" do
      result <-
        fixtureLore do
          interpretExpression "(crossModuleSeed, supportStep 4)"

      result `shouldBe` "(5,9)"

    it "returns the inferred type of an expression in the default project context" do
      result <-
        fixtureLore do
          getTypeOfExpression "lookupOrZero [(\"left\", 3)]"

      renderType result `shouldBe` "String -> Int"

    it "keeps successfully loaded modules in context even when another module fails to compile" do
      withFixtureCopy \fixtureRoot -> do
        TIO.writeFile
          (fixtureRoot </> "src" </> "Broken.hs")
          "module Broken where\n\nbrokenValue = doesNotExist\n"

        result <-
          fixtureLoreAt fixtureRoot do
            interpretExpression "lookupOrZero [(\"left\", 7)] \"left\""

        result `shouldBe` "7"

    it "can disable implicit Prelude imports" do
      withFixtureCopy \fixtureRoot -> do
        let sessionConfig = sessionConfigWithPreludeRule NoPrelude

        ( fixtureLoreAtWithConfig sessionConfig fixtureRoot do
            interpretExpression "map (+1) [1, 2 :: Int]"
          )
          `shouldThrow` anyException

    it "can use a custom Prelude import module" do
      withFixtureCopy \fixtureRoot -> do
        TIO.writeFile
          (fixtureRoot </> "src" </> "CustomPrelude.hs")
          "module CustomPrelude (module Prelude, nub) where\n\nimport Prelude\nimport Data.List (nub)\n"

        let sessionConfig = sessionConfigWithPreludeRule (ImportCustomPrelude "CustomPrelude")

        result <-
          fixtureLoreAtWithConfig sessionConfig fixtureRoot do
            interpretExpression "nub ['a', 'a', 'b']"

        result `shouldBe` "\"ab\""

        ty <-
          fixtureLoreAtWithConfig sessionConfig fixtureRoot do
            getTypeOfExpression "nub ['a', 'a', 'b']"

        renderType ty `shouldBe` "[Char]"

sessionConfigWithPreludeRule :: PreludeImportRule -> Session.SessionConfig
sessionConfigWithPreludeRule interpreterPreludeImportRule =
  Session.SessionConfig
    { Session.projectRoot = projectRoot,
      Session.ghcWorkDir = ghcWorkDir,
      Session.loggerHandle = loggerHandle,
      Session.interpreterPreludeImportRule = interpreterPreludeImportRule,
      Session.parallelWorkersLimit = parallelWorkersLimit
    }
  where
    Session.SessionConfig
      { Session.projectRoot,
        Session.ghcWorkDir,
        Session.loggerHandle,
        Session.parallelWorkersLimit
      } = defaultSessionConfig

renderType :: GHC.Type -> String
renderType =
  Outputable.showSDocUnsafe . Outputable.ppr
