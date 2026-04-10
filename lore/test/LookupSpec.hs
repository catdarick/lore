module LookupSpec
  ( spec,
  )
where

import Data.List (isInfixOf)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified GHC
import qualified GHC.Plugins as Plugins
import qualified GHC.Utils.Outputable as Outputable
import Lore
  ( LookupInstancesQuery (..),
    LookupInstancesResult (..),
    MatchingInstance (..),
    Symbol (..),
    SymbolCategory (..),
    SymbolInfo (..),
    SymbolVisibility (..),
    defaultLoadTargetsOptions,
    findSymbols,
    loadTargets,
    lookupIntersectingInstances,
    lookupIntersectingRootInstances,
    lookupRootSymbolInfo,
  )
import System.FilePath ((</>))
import Test.Hspec
import TestSupport (fixtureLore, fixtureLoreAt, withFixtureCopy)

spec :: Spec
spec =
  do
    describe "lookupRootSymbolInfo" do
      it "deduplicates root-resolved results when a type and constructor share a name" do
        result <-
          fixtureLore do
            _ <- loadTargets defaultLoadTargetsOptions
            lookupRootSymbolInfo "Indexed"

        length result `shouldBe` 1
        fmap (Outputable.showSDocUnsafe . Outputable.ppr . symbolName) result
          `shouldSatisfy` any (isInfixOf "Indexed")

      it "classifies non-value symbols by declaration kind" do
        indexedInfo <-
          fixtureLore do
            _ <- loadTargets defaultLoadTargetsOptions
            lookupRootSymbolInfo "Indexed"

        nameSetInfo <-
          fixtureLore do
            _ <- loadTargets defaultLoadTargetsOptions
            lookupRootSymbolInfo "NameSet"

        hasIndexInfo <-
          fixtureLore do
            _ <- loadTargets defaultLoadTargetsOptions
            lookupRootSymbolInfo "HasIndex"

        elemInfo <-
          fixtureLore do
            _ <- loadTargets defaultLoadTargetsOptions
            lookupRootSymbolInfo "Elem"

        bucketInfo <-
          fixtureLore do
            _ <- loadTargets defaultLoadTargetsOptions
            lookupRootSymbolInfo "Bucket"

        demoCategories indexedInfo `shouldBe` [SymbolData]
        demoCategories nameSetInfo `shouldBe` [SymbolTypeAlias]
        demoCategories hasIndexInfo `shouldBe` [SymbolClass]
        demoCategories elemInfo `shouldBe` [SymbolTypeFamily]
        demoCategories bucketInfo `shouldBe` [SymbolDataFamily]

      it "filters symbol matches by definition module hint" do
        result <-
          fixtureLore do
            _ <- loadTargets defaultLoadTargetsOptions
            lookupRootSymbolInfo "Demo.Support.supportSeed"

        length result `shouldBe` 1
        fmap (GHC.moduleNameString . GHC.moduleName . definedIn) result
          `shouldBe` ["Demo.Support"]

      it "filters symbol matches by export module hint" do
        result <-
          fixtureLore do
            _ <- loadTargets defaultLoadTargetsOptions
            lookupRootSymbolInfo "Prelude.map"

        result `shouldSatisfy` (not . null)
        result
          `shouldSatisfy` all (\symbolInfo -> elem "Prelude" (map (GHC.moduleNameString . GHC.moduleName) symbolInfo.exportedFrom))

      it "supports module-qualified dotted operators" do
        result <-
          fixtureLore do
            _ <- loadTargets defaultLoadTargetsOptions
            lookupRootSymbolInfo "Demo.Support..+."

        length result `shouldBe` 1
        fmap (Outputable.showSDocUnsafe . Outputable.ppr . symbolName) result
          `shouldSatisfy` any (isInfixOf ".+.")

      it "supports parenthesized operator queries" do
        result <-
          fixtureLore do
            _ <- loadTargets defaultLoadTargetsOptions
            lookupRootSymbolInfo "(.+.)"

        length result `shouldBe` 1
        fmap (Outputable.showSDocUnsafe . Outputable.ppr . symbolName) result
          `shouldSatisfy` any (isInfixOf ".+.")

      it "supports module-qualified parenthesized operator queries" do
        result <-
          fixtureLore do
            _ <- loadTargets defaultLoadTargetsOptions
            lookupRootSymbolInfo "Demo.Support.(.+.)"

        length result `shouldBe` 1
        fmap (Outputable.showSDocUnsafe . Outputable.ppr . symbolName) result
          `shouldSatisfy` any (isInfixOf ".+.")

    describe "findSymbols" do
      it "supports module-qualified hints before filtering candidates" do
        result <-
          fixtureLore do
            _ <- loadTargets defaultLoadTargetsOptions
            findSymbols "Demo.Support.supportSeed"

        length result `shouldBe` 1
        fmap (\exportedSymbol -> maybe "" (GHC.moduleNameString . GHC.moduleName) (Plugins.nameModule_maybe exportedSymbol.name)) result
          `shouldBe` ["Demo.Support"]

      it "includes non-exported top-level symbols from home modules" do
        result <-
          fixtureLore do
            _ <- loadTargets defaultLoadTargetsOptions
            findSymbols "supportValues"

        length result `shouldBe` 1
        all (== Symbol'Unexported) (fmap visibility result) `shouldBe` True
        fmap (\symbol -> maybe "" (GHC.moduleNameString . GHC.moduleName) (Plugins.nameModule_maybe symbol.name)) result
          `shouldBe` ["Demo.Support"]

      it "supports module-qualified dotted operators" do
        result <-
          fixtureLore do
            _ <- loadTargets defaultLoadTargetsOptions
            findSymbols "Demo.Support..+."

        length result `shouldBe` 1
        fmap (Outputable.showSDocUnsafe . Outputable.ppr . name) result
          `shouldSatisfy` any (isInfixOf ".+.")

      it "supports parenthesized operator queries" do
        result <-
          fixtureLore do
            _ <- loadTargets defaultLoadTargetsOptions
            findSymbols "(.+.)"

        length result `shouldBe` 1
        fmap (Outputable.showSDocUnsafe . Outputable.ppr . name) result
          `shouldSatisfy` any (isInfixOf ".+.")

      it "supports module-qualified parenthesized operator queries" do
        result <-
          fixtureLore do
            _ <- loadTargets defaultLoadTargetsOptions
            findSymbols "Demo.Support.(.+.)"

        length result `shouldBe` 1
        fmap (Outputable.showSDocUnsafe . Outputable.ppr . name) result
          `shouldSatisfy` any (isInfixOf ".+.")

    describe "lookupIntersectingInstances" do
      it "intersects class instances across multiple symbol queries" do
        withFixtureInstances \fixtureRoot -> do
          result <-
            fixtureLoreAt fixtureRoot do
              _ <- loadTargets defaultLoadTargetsOptions
              lookupIntersectingInstances ["HasIndex", "Indexed"]

          lookupInstancesQueryMatchCounts result `shouldSatisfy` all (> 0)
          renderMatchingInstances result `shouldSatisfy` matchesRenderedInstance "HasIndex (Indexed Int)"

      it "supports root-resolved symbol queries before intersecting instances" do
        withFixtureInstances \fixtureRoot -> do
          result <-
            fixtureLoreAt fixtureRoot do
              _ <- loadTargets defaultLoadTargetsOptions
              lookupIntersectingRootInstances ["indexedValues", "HasIndex"]

          lookupInstancesQueryMatchCounts result `shouldSatisfy` all (> 0)
          renderMatchingInstances result `shouldSatisfy` matchesRenderedInstance "HasIndex (Indexed Int)"

      it "supports module-qualified symbol queries" do
        withFixtureInstances \fixtureRoot -> do
          result <-
            fixtureLoreAt fixtureRoot do
              _ <- loadTargets defaultLoadTargetsOptions
              lookupIntersectingRootInstances ["Demo.Indexed", "HasIndex"]

          lookupInstancesQueryMatchCounts result `shouldSatisfy` all (> 0)
          renderMatchingInstances result `shouldSatisfy` matchesRenderedInstance "HasIndex (Indexed Int)"

      it "intersects family instances across multiple symbol queries" do
        withFixtureInstances \fixtureRoot -> do
          result <-
            fixtureLoreAt fixtureRoot do
              _ <- loadTargets defaultLoadTargetsOptions
              lookupIntersectingInstances ["Elem", "Indexed"]

          lookupInstancesQueryMatchCounts result `shouldSatisfy` all (> 0)
          renderMatchingInstances result `shouldSatisfy` matchesRenderedInstance "Elem (Indexed a) = a"

withFixtureInstances :: (FilePath -> IO a) -> IO a
withFixtureInstances action =
  withFixtureCopy \fixtureRoot -> do
    enableFlexibleInstances fixtureRoot
    appendDemoInstances fixtureRoot
    action fixtureRoot

appendDemoInstances :: FilePath -> IO ()
appendDemoInstances fixtureRoot = do
  let demoFile = fixtureRoot </> "src" </> "Demo.hs"
  TIO.appendFile demoFile instanceDefinitions

enableFlexibleInstances :: FilePath -> IO ()
enableFlexibleInstances fixtureRoot = do
  let packageFile = fixtureRoot </> "package.yaml"
  packageSource <- TIO.readFile packageFile
  TIO.writeFile
    packageFile
    (T.replace "- KindSignatures\n" "- KindSignatures\n- FlexibleInstances\n" packageSource)

instanceDefinitions :: T.Text
instanceDefinitions =
  T.unlines
    [ "",
      "type instance Elem (Indexed a) = a",
      "",
      "data instance Bucket Int = IntBucket Int",
      "",
      "instance HasIndex (Indexed Int) where",
      "  toIndex _ = Map.empty",
      "",
      "instance HasIndex Support.SupportRecord where",
      "  toIndex _ = Map.empty"
    ]

lookupInstancesQueryMatchCounts :: LookupInstancesResult -> [Int]
lookupInstancesQueryMatchCounts =
  map (length . lookupInstancesQueryMatches) . lookupInstancesQueries

renderMatchingInstances :: LookupInstancesResult -> [String]
renderMatchingInstances =
  map renderMatchingInstance . lookupInstancesResults

renderMatchingInstance :: MatchingInstance -> String
renderMatchingInstance = \case
  MatchingClassInstance _ matchingClassInstance ->
    Outputable.showSDocUnsafe (Outputable.ppr matchingClassInstance)
  MatchingFamilyInstance _ matchingFamilyInstance ->
    Outputable.showSDocUnsafe (Outputable.ppr matchingFamilyInstance)

matchesRenderedInstance :: String -> [String] -> Bool
matchesRenderedInstance expected = \case
  [rendered] -> expected `isInfixOf` rendered
  _ -> False

demoCategories :: [SymbolInfo] -> [SymbolCategory]
demoCategories =
  map symbolCategory
    . filter ((== "Demo") . GHC.moduleNameString . GHC.moduleName . definedIn)
