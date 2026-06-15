module TestSupportSpec
  ( spec,
  )
where

import Control.Exception (bracket)
import Control.Monad (forM_, void, when)
import qualified Data.List as List
import Data.Text (pack)
import qualified GHC
import qualified GHC.Data.FastString as FastString
import qualified GHC.Plugins as Plugins
import qualified Lore
import Lore.Definition (resolveDefinitionSourceNamed)
import Lore.Definition.RenderSlice (definitionSourceToRenderSlice)
import Lore.HomeModules (defaultLoadHomeModulesOptions, loadHomeModules)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import Test.Hspec
import TestSupport
  ( FixtureBuildProvider (..),
    findSymbols,
    fixtureLore,
    fixtureProjectRoot,
    fixtureSourceRoot,
    selectFixtureBuildProvider,
    withFixtureContext,
    withFixtureCopy,
  )

spec :: Spec
spec = do
  describe "selectFixtureBuildProvider" do
    it "honors an explicit Stack override" do
      selectFixtureBuildProvider (Just "stack") Nothing `shouldBe` Right FixtureProviderStack

    it "honors an explicit Cabal override" do
      selectFixtureBuildProvider (Just "cabal") (Just "/usr/bin/stack") `shouldBe` Right FixtureProviderCabal

    it "rejects unsupported overrides" do
      selectFixtureBuildProvider (Just "nix") Nothing `shouldSatisfy` isLeft

    it "detects Stack from STACK_EXE" do
      selectFixtureBuildProvider Nothing (Just "/usr/bin/stack") `shouldBe` Right FixtureProviderStack

    it "uses Cabal when Stack is not running the tests" do
      selectFixtureBuildProvider Nothing Nothing `shouldBe` Right FixtureProviderCabal

  describe "withFixtureContext" do
    it "does not write provider files or generated state into the source fixture" do
      withFixtureProvider "stack" do
        sourceRoot <- fixtureSourceRoot <$> withFixtureContext pure
        assertSourceFixtureNeutral sourceRoot
        withFixtureContext \_ ->
          assertSourceFixtureNeutral sourceRoot
        assertSourceFixtureNeutral sourceRoot

    it "keeps the temporary root alive for the callback lifetime and removes it afterward" do
      fixtureRoot <-
        withFixtureProvider "stack" $
          withFixtureContext \fixture -> do
            let root = fixtureProjectRoot fixture
            doesDirectoryExist root `shouldReturn` True
            doesFileExist (root </> "src" </> "Demo.hs") `shouldReturn` True
            pure root
      doesDirectoryExist fixtureRoot `shouldReturn` False

    it "materializes only Stack provider files for a Stack fixture" do
      withFixtureProvider "stack" $
        withFixtureContext \fixture -> do
          doesFileExist (fixtureProjectRoot fixture </> "stack.yaml") `shouldReturn` True
          doesFileExist (fixtureProjectRoot fixture </> "cabal.project") `shouldReturn` False

    it "materializes only Cabal provider files for a Cabal fixture" do
      withFixtureProvider "cabal" $
        withFixtureContext \fixture -> do
          doesFileExist (fixtureProjectRoot fixture </> "cabal.project") `shouldReturn` True
          doesFileExist (fixtureProjectRoot fixture </> "stack.yaml") `shouldReturn` False

    it "keeps path-backed definition spans readable throughout the example" do
      withFixtureProvider "stack" $
        withFixtureContext \fixture -> do
          slice <-
            fixtureLore fixture do
              _ <- loadHomeModules defaultLoadHomeModulesOptions
              symbols <- findSymbols (pack "lookupOrZero")
              targetName <-
                maybe (error "lookupOrZero not found") (pure . (.name)) $
                  List.find ((== "lookupOrZero") . Plugins.getOccString . (.name)) symbols
              source <- maybe (error "lookupOrZero definition not found") pure =<< resolveDefinitionSourceNamed targetName
              pure (definitionSourceToRenderSlice source)
          case slice.declarationSpans of
            [declaration] -> do
              spanPath <- singleRealSpanPath declaration.declarationSpan
              void (readFile spanPath)
            spans ->
              expectationFailure ("Expected one declaration span, got " <> show (length spans))

    it "cleans up nested mutable fixture copies after their callback" do
      nestedRoot <-
        withFixtureProvider "stack" $
          withFixtureContext \fixture ->
            withFixtureCopy fixture \fixtureRoot -> do
              doesDirectoryExist fixtureRoot `shouldReturn` True
              doesFileExist (fixtureRoot </> "src" </> "Demo.hs") `shouldReturn` True
              pure fixtureRoot
      doesDirectoryExist nestedRoot `shouldReturn` False

withFixtureProvider :: String -> IO a -> IO a
withFixtureProvider provider =
  bracket setProvider restoreProvider . const
  where
    setProvider = do
      previous <- lookupEnv "LORE_FIXTURE_PROVIDER"
      setEnv "LORE_FIXTURE_PROVIDER" provider
      pure previous

    restoreProvider previous =
      maybe (unsetEnv "LORE_FIXTURE_PROVIDER") (setEnv "LORE_FIXTURE_PROVIDER") previous

assertSourceFixtureNeutral :: FilePath -> Expectation
assertSourceFixtureNeutral sourceRoot = do
  entries <- listDirectory sourceRoot
  forM_ generatedEntries \entry ->
    when (entry `elem` entries) $
      expectationFailure ("source fixture contains generated entry: " <> entry)
  where
    generatedEntries =
      [ ".lore-work-test",
        ".stack-work",
        "dist-newstyle",
        "stack.yaml",
        "stack.yaml.lock",
        "cabal.project",
        "cabal.project.local",
        "cabal.project.freeze",
        ".hspec-failures"
      ]

singleRealSpanPath :: GHC.SrcSpan -> IO FilePath
singleRealSpanPath srcSpan =
  case srcSpan of
    GHC.RealSrcSpan realSpan _ ->
      pure (FastString.unpackFS (GHC.srcSpanFile realSpan))
    GHC.UnhelpfulSpan _ ->
      expectationFailure "expected a real source span" >> pure ""

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False
