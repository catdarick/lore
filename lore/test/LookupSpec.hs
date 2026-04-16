module LookupSpec
  ( spec,
  )
where

import Control.Monad (forM)
import Data.List (foldl', isInfixOf)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified GHC
import qualified GHC.Plugins as Plugins
import qualified GHC.Utils.Outputable as Outputable
import Lore
  ( ExportedSymbolNode (..),
    Instances (..),
    MonadLore,
    Symbol (..),
    SymbolCategory (..),
    SymbolInfo (..),
    SymbolVisibility (..),
    classifySymbolCategory,
    defaultLoadTargetsOptions,
    listAssociatedInstances,
    loadTargets,
  )
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Test.Hspec
import TestSupport
  ( filterExportedSymbolNodesByTypeHint,
    findRootSymbols,
    findSymbols,
    fixtureLore,
    fixtureLoreAt,
    listExportedSymbolsByModule,
    lookupRootSymbolInfo,
    withFixtureCopy,
  )

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

        null result `shouldBe` False
        all (symbolExportedFromModule "Prelude") result `shouldBe` True

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

      it "survives two consecutive reloads before lookupRootSymbolInfo" do
        result <-
          fixtureLore do
            _ <- loadTargets defaultLoadTargetsOptions
            _ <- loadTargets defaultLoadTargetsOptions
            lookupRootSymbolInfo "Demo.Support.supportSeed"

        length result `shouldBe` 1
        fmap (GHC.moduleNameString . GHC.moduleName . definedIn) result
          `shouldBe` ["Demo.Support"]

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
        all (== Symbol'Unexported) (fmap (.visibility) result) `shouldBe` True
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

      it "survives two consecutive reloads before findSymbols" do
        result <-
          fixtureLore do
            _ <- loadTargets defaultLoadTargetsOptions
            _ <- loadTargets defaultLoadTargetsOptions
            findSymbols "supportValues"

        length result `shouldBe` 1
        all (== Symbol'Unexported) (fmap (.visibility) result) `shouldBe` True
        fmap (\symbol -> maybe "" (GHC.moduleNameString . GHC.moduleName) (Plugins.nameModule_maybe symbol.name)) result
          `shouldBe` ["Demo.Support"]

    describe "listExportedSymbolsByModule" do
      it "lists exported symbols for the requested module and excludes unexported ones" do
        result <-
          fixtureLore do
            _ <- loadTargets defaultLoadTargetsOptions
            listExportedSymbolsByModule "Demo.Support" Nothing

        let occNames = exportedNodeOccNames result
        occNames `shouldSatisfy` elem "supportSeed"
        occNames `shouldSatisfy` elem "supportStep"
        occNames `shouldSatisfy` elem "mkSupportRecord"
        occNames `shouldSatisfy` elem ".+."
        occNames `shouldSatisfy` not . elem "supportValues"

      it "returns an empty list when the module is not visible" do
        result <-
          fixtureLore do
            _ <- loadTargets defaultLoadTargetsOptions
            listExportedSymbolsByModule "No.Such.Module" Nothing

        null result `shouldBe` True

      it "filters by direct surface type mentions instead of transitive metadata" do
        withFixtureCopy \fixtureRoot -> do
          let moduleDir = fixtureRoot </> "src" </> "TestHint"
              moduleFile = moduleDir </> "Filter.hs"
          createDirectoryIfMissing True moduleDir
          TIO.writeFile moduleFile directTypeHintFixtureModuleSource

          result <-
            fixtureLoreAt fixtureRoot do
              _ <- loadTargets defaultLoadTargetsOptions
              exports <- listExportedSymbolsByModule "TestHint.Filter" Nothing
              pure (filterExportedSymbolNodesByTypeHint "String" exports)

          let occNames = exportedNodeOccNames result
          occNames `shouldSatisfy` elem "directStringConsumer"
          occNames `shouldSatisfy` not . elem "WrapSomeException"

    describe "lookupIntersectingInstances" do
      it "intersects class instances across multiple symbol queries" do
        withFixtureInstances \fixtureRoot -> do
          (queryMatchCounts, renderedInstances) <-
            fixtureLoreAt fixtureRoot do
              _ <- loadTargets defaultLoadTargetsOptions
              lookupIntersectingInstancesForQueries False ["HasIndex", "Indexed"]

          queryMatchCounts `shouldSatisfy` all (> 0)
          renderedInstances `shouldSatisfy` matchesRenderedInstance "HasIndex (Indexed Int)"

      it "supports root-resolved symbol queries before intersecting instances" do
        withFixtureInstances \fixtureRoot -> do
          (queryMatchCounts, renderedInstances) <-
            fixtureLoreAt fixtureRoot do
              _ <- loadTargets defaultLoadTargetsOptions
              lookupIntersectingInstancesForQueries True ["indexedValues", "HasIndex"]

          queryMatchCounts `shouldSatisfy` all (> 0)
          renderedInstances `shouldSatisfy` matchesRenderedInstance "HasIndex (Indexed Int)"

      it "supports module-qualified symbol queries" do
        withFixtureInstances \fixtureRoot -> do
          (queryMatchCounts, renderedInstances) <-
            fixtureLoreAt fixtureRoot do
              _ <- loadTargets defaultLoadTargetsOptions
              lookupIntersectingInstancesForQueries True ["Demo.Indexed", "HasIndex"]

          queryMatchCounts `shouldSatisfy` all (> 0)
          renderedInstances `shouldSatisfy` matchesRenderedInstance "HasIndex (Indexed Int)"

      it "intersects family instances across multiple symbol queries" do
        withFixtureInstances \fixtureRoot -> do
          (queryMatchCounts, renderedInstances) <-
            fixtureLoreAt fixtureRoot do
              _ <- loadTargets defaultLoadTargetsOptions
              lookupIntersectingInstancesForQueries False ["Elem", "Indexed"]

          queryMatchCounts `shouldSatisfy` all (> 0)
          renderedInstances `shouldSatisfy` matchesRenderedInstance "Elem (Indexed a) = a"

exportedNodeOccNames :: [ExportedSymbolNode] -> [String]
exportedNodeOccNames nodes =
  map (Plugins.getOccString . (.nodeName)) nodes
    <> concatMap (map (Plugins.getOccString . (.nodeName)) . (.nodeChildren)) nodes

directTypeHintFixtureModuleSource :: T.Text
directTypeHintFixtureModuleSource =
  T.unlines
    [ "module TestHint.Filter",
      "  ( directStringConsumer,",
      "    WrapSomeException(..)",
      "  )",
      "where",
      "",
      "import Control.Exception (SomeException)",
      "",
      "directStringConsumer :: String -> Int",
      "directStringConsumer = length",
      "",
      "data WrapSomeException = WrapSomeException SomeException"
    ]

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

lookupIntersectingInstancesForQueries :: (MonadLore m) => Bool -> [T.Text] -> m ([Int], [String])
lookupIntersectingInstancesForQueries resolveRoots queries = do
  queryMatches <- forM queries resolveQueryMatches
  queryInstances <- mapM listUnionInstancesForSymbols queryMatches
  let queryMatchCounts = map length queryMatches
      intersectedInstances = intersectAllInstances queryInstances
  pure (queryMatchCounts, renderInstances intersectedInstances)
  where
    resolveQueryMatches query =
      if resolveRoots
        then findRootSymbols query
        else findSymbols query

listUnionInstancesForSymbols :: (MonadLore m) => [Symbol] -> m Instances
listUnionInstancesForSymbols symbols = do
  instancesPerSymbol <- mapM (listAssociatedInstances . (.name)) symbols
  pure (foldl' unionInstances (Instances [] []) instancesPerSymbol)

unionInstances :: Instances -> Instances -> Instances
unionInstances left right =
  Instances
    { classInstances = deduplicateClassInstances (left.classInstances <> right.classInstances),
      familyInstances = deduplicateFamilyInstances (left.familyInstances <> right.familyInstances)
    }
  where
    deduplicateClassInstances = Map.elems . Map.fromList . map (\instance_ -> (GHC.getName instance_, instance_))
    deduplicateFamilyInstances = Map.elems . Map.fromList . map (\instance_ -> (GHC.getName instance_, instance_))

intersectAllInstances :: [Instances] -> Instances
intersectAllInstances = \case
  [] -> Instances [] []
  firstInstances : restInstances ->
    foldl' intersectInstances firstInstances restInstances

intersectInstances :: Instances -> Instances -> Instances
intersectInstances left right =
  Instances
    { classInstances = filter (\instance_ -> GHC.getName instance_ `Set.member` rightClassNames) left.classInstances,
      familyInstances = filter (\instance_ -> GHC.getName instance_ `Set.member` rightFamilyNames) left.familyInstances
    }
  where
    rightClassNames = Set.fromList (map GHC.getName right.classInstances)
    rightFamilyNames = Set.fromList (map GHC.getName right.familyInstances)

renderInstances :: Instances -> [String]
renderInstances instances_ =
  map (Outputable.showSDocUnsafe . Outputable.ppr) instances_.classInstances
    <> map (Outputable.showSDocUnsafe . Outputable.ppr) instances_.familyInstances

matchesRenderedInstance :: String -> [String] -> Bool
matchesRenderedInstance expected = \case
  [rendered] -> expected `isInfixOf` rendered
  _ -> False

demoCategories :: [SymbolInfo] -> [SymbolCategory]
demoCategories =
  map (classifySymbolCategory . symbolThing)
    . filter ((== "Demo") . GHC.moduleNameString . GHC.moduleName . definedIn)

symbolExportedFromModule :: String -> SymbolInfo -> Bool
symbolExportedFromModule moduleName symbolInfo =
  case symbolInfo.visibility of
    Symbol'Unexported ->
      False
    Symbol'ExportedFrom modules_ ->
      moduleName `elem` map (GHC.moduleNameString . GHC.moduleName) (Set.toList modules_)
