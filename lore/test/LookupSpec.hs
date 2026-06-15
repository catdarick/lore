module LookupSpec
  ( spec,
  )
where

import Control.Monad (forM)
import Data.List (isInfixOf)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
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
    PathToRoot (..),
    Symbol (..),
    SymbolCategory (..),
    SymbolInfo (..),
    SymbolVisibility (..),
    classifySymbolCategory,
    defaultLoadHomeModulesOptions,
    findMatchingSymbols,
    listAssociatedInstances,
    listDirectInstances,
    loadHomeModules,
    lookupSymbolInfo,
    parseAndNormalizeName,
    resolvePathToRoot,
  )
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Test.Hspec
import TestSupport
  ( FixtureContext,
    filterExportedSymbolNodesByTypeHint,
    findRootSymbols,
    findSymbols,
    fixtureLore,
    fixtureLoreAt,
    listExportedSymbolsByModule,
    lookupRootSymbolInfo,
    withFixtureCopy,
    withFixtureSpec,
  )

spec :: Spec
spec =
  withFixtureSpec do
    describe "lookupRootSymbolInfo" do
      it "deduplicates root-resolved results when a type and constructor share a name" \fixture -> do
        result <-
          fixtureLore fixture do
            _ <- loadHomeModules defaultLoadHomeModulesOptions
            lookupRootSymbolInfo "Indexed"

        length result `shouldBe` 1
        fmap (Outputable.showSDocUnsafe . Outputable.ppr . symbolName) result
          `shouldSatisfy` any (isInfixOf "Indexed")

      it "classifies non-value symbols by declaration kind" \fixture -> do
        indexedInfo <-
          fixtureLore fixture do
            _ <- loadHomeModules defaultLoadHomeModulesOptions
            resolvePreferredRootSymbolInfos "Demo.Indexed"

        nameSetInfo <-
          fixtureLore fixture do
            _ <- loadHomeModules defaultLoadHomeModulesOptions
            resolvePreferredRootSymbolInfos "Demo.NameSet"

        hasIndexInfo <-
          fixtureLore fixture do
            _ <- loadHomeModules defaultLoadHomeModulesOptions
            resolvePreferredRootSymbolInfos "Demo.HasIndex"

        elemInfo <-
          fixtureLore fixture do
            _ <- loadHomeModules defaultLoadHomeModulesOptions
            resolvePreferredRootSymbolInfos "Demo.Elem"

        bucketInfo <-
          fixtureLore fixture do
            _ <- loadHomeModules defaultLoadHomeModulesOptions
            resolvePreferredRootSymbolInfos "Demo.Bucket"

        demoCategories indexedInfo `shouldBe` [SymbolData]
        demoCategories nameSetInfo `shouldBe` [SymbolTypeAlias]
        demoCategories hasIndexInfo `shouldBe` [SymbolClass]
        demoCategories elemInfo `shouldBe` [SymbolTypeFamily]
        demoCategories bucketInfo `shouldBe` [SymbolDataFamily]

      it "filters symbol matches by definition module hint" \fixture -> do
        result <-
          fixtureLore fixture do
            _ <- loadHomeModules defaultLoadHomeModulesOptions
            lookupRootSymbolInfo "Demo.Support.supportSeed"

        length result `shouldBe` 1
        fmap (GHC.moduleNameString . GHC.moduleName . definedIn) result
          `shouldBe` ["Demo.Support"]

      it "filters symbol matches by export module hint" \fixture -> do
        result <-
          fixtureLore fixture do
            _ <- loadHomeModules defaultLoadHomeModulesOptions
            lookupRootSymbolInfo "Prelude.map"

        null result `shouldBe` False
        all (symbolExportedFromModule "Prelude") result `shouldBe` True

      it "accepts both defining and re-exporting module qualifiers" \fixture -> do
        withFixtureCopy fixture \fixtureRoot -> do
          addReexportQualifierFixture fixtureRoot
          (internalQualified, exportingQualified) <-
            fixtureLoreAt fixture fixtureRoot do
              _ <- loadHomeModules defaultLoadHomeModulesOptions
              internalQualified <- lookupRootSymbolInfo "Some.Internal.Module.foo"
              exportingQualified <- lookupRootSymbolInfo "Some.Exporting.Module.foo"
              pure (internalQualified, exportingQualified)

          length internalQualified `shouldBe` 1
          length exportingQualified `shouldBe` 1
          fmap (GHC.moduleNameString . GHC.moduleName . definedIn) internalQualified
            `shouldBe` ["Some.Internal.Module"]
          fmap (GHC.moduleNameString . GHC.moduleName . definedIn) exportingQualified
            `shouldBe` ["Some.Internal.Module"]

      it "supports module-qualified dotted operators" \fixture -> do
        result <-
          fixtureLore fixture do
            _ <- loadHomeModules defaultLoadHomeModulesOptions
            lookupRootSymbolInfo "Demo.Support..+."

        length result `shouldBe` 1
        fmap (Outputable.showSDocUnsafe . Outputable.ppr . symbolName) result
          `shouldSatisfy` any (isInfixOf ".+.")

      it "supports parenthesized operator queries" \fixture -> do
        result <-
          fixtureLore fixture do
            _ <- loadHomeModules defaultLoadHomeModulesOptions
            lookupRootSymbolInfo "(.+.)"

        length result `shouldBe` 1
        fmap (Outputable.showSDocUnsafe . Outputable.ppr . symbolName) result
          `shouldSatisfy` any (isInfixOf ".+.")

      it "supports module-qualified parenthesized operator queries" \fixture -> do
        result <-
          fixtureLore fixture do
            _ <- loadHomeModules defaultLoadHomeModulesOptions
            lookupRootSymbolInfo "Demo.Support.(.+.)"

        length result `shouldBe` 1
        fmap (Outputable.showSDocUnsafe . Outputable.ppr . symbolName) result
          `shouldSatisfy` any (isInfixOf ".+.")

      it "survives two consecutive reloads before lookupRootSymbolInfo" \fixture -> do
        result <-
          fixtureLore fixture do
            _ <- loadHomeModules defaultLoadHomeModulesOptions
            _ <- loadHomeModules defaultLoadHomeModulesOptions
            lookupRootSymbolInfo "Demo.Support.supportSeed"

        length result `shouldBe` 1
        fmap (GHC.moduleNameString . GHC.moduleName . definedIn) result
          `shouldBe` ["Demo.Support"]

      it "finds exported and unexported record fields from DuplicateRecordFields modules" \fixture -> do
        withFixtureCopy fixture \fixtureRoot -> do
          addRecordFieldLookupFixture fixtureRoot
          (exportedFieldSymbols, unexportedFieldSymbols, qualifiedFieldSymbols, constructorSymbols) <-
            fixtureLoreAt fixture fixtureRoot do
              _ <- loadHomeModules defaultLoadHomeModulesOptions
              exportedFieldSymbols <- findSymbols "userName"
              unexportedFieldSymbols <- findSymbols "hiddenValue"
              qualifiedFieldSymbols <- findSymbols "Demo.hiddenValue"
              constructorSymbols <- findSymbols "Demo.Hidden"
              pure (exportedFieldSymbols, unexportedFieldSymbols, qualifiedFieldSymbols, constructorSymbols)

          let isExportedFrom moduleName visibility =
                case visibility of
                  Symbol'ExportedFrom modules_ ->
                    moduleName `elem` map (GHC.moduleNameString . GHC.moduleName) (Set.toList modules_)
                  Symbol'Unexported ->
                    False

              isDefinedInModule moduleName symbol =
                fmap (GHC.moduleNameString . GHC.moduleName) (Plugins.nameModule_maybe symbol.name) == Just moduleName

              demoUnexportedFieldSymbols =
                filter (isDefinedInModule "Demo") unexportedFieldSymbols

              demoQualifiedFieldSymbols =
                filter (isDefinedInModule "Demo") qualifiedFieldSymbols

              demoUnexportedFieldOccs =
                fmap (Plugins.getOccString . (.name)) demoUnexportedFieldSymbols

              demoQualifiedFieldOccs =
                fmap (Plugins.getOccString . (.name)) demoQualifiedFieldSymbols

          length exportedFieldSymbols `shouldBe` 1
          fmap (Plugins.getOccString . (.name)) exportedFieldSymbols
            `shouldBe` ["userName"]
          fmap (fmap (GHC.moduleNameString . GHC.moduleName) . Plugins.nameModule_maybe . (.name)) exportedFieldSymbols
            `shouldBe` [Just "Demo"]
          all (isExportedFrom "Demo" . (.visibility)) exportedFieldSymbols `shouldBe` True

          length demoUnexportedFieldSymbols `shouldSatisfy` (> 0)
          demoUnexportedFieldOccs `shouldSatisfy` all (isInfixOf "hiddenValue")

          fmap (fmap (GHC.moduleNameString . GHC.moduleName) . Plugins.nameModule_maybe . (.name)) demoUnexportedFieldSymbols
            `shouldSatisfy` all (== Just "Demo")
          all ((== Symbol'Unexported) . (.visibility)) demoUnexportedFieldSymbols `shouldBe` True

          length demoQualifiedFieldSymbols `shouldSatisfy` (> 0)
          demoQualifiedFieldOccs `shouldSatisfy` all (isInfixOf "hiddenValue")
          fmap (fmap (GHC.moduleNameString . GHC.moduleName) . Plugins.nameModule_maybe . (.name)) demoQualifiedFieldSymbols
            `shouldSatisfy` all (== Just "Demo")
          all ((== Symbol'Unexported) . (.visibility)) demoQualifiedFieldSymbols `shouldBe` True

          null constructorSymbols `shouldBe` False
          fmap (Outputable.showSDocUnsafe . Outputable.ppr . name) constructorSymbols
            `shouldSatisfy` any (isInfixOf "Hidden")

    describe "findSymbols" do
      it "supports module-qualified hints before filtering candidates" \fixture -> do
        result <-
          fixtureLore fixture do
            _ <- loadHomeModules defaultLoadHomeModulesOptions
            findSymbols "Demo.Support.supportSeed"

        length result `shouldBe` 1
        fmap (\exportedSymbol -> maybe "" (GHC.moduleNameString . GHC.moduleName) (Plugins.nameModule_maybe exportedSymbol.name)) result
          `shouldBe` ["Demo.Support"]

      it "accepts re-exporting module-qualified hints" \fixture -> do
        withFixtureCopy fixture \fixtureRoot -> do
          addReexportQualifierFixture fixtureRoot
          (internalQualified, exportingQualified) <-
            fixtureLoreAt fixture fixtureRoot do
              _ <- loadHomeModules defaultLoadHomeModulesOptions
              internalQualified <- findSymbols "Some.Internal.Module.foo"
              exportingQualified <- findSymbols "Some.Exporting.Module.foo"
              pure (internalQualified, exportingQualified)

          length internalQualified `shouldBe` 1
          length exportingQualified `shouldBe` 1
          fmap (fmap (GHC.moduleNameString . GHC.moduleName) . Plugins.nameModule_maybe . (.name)) internalQualified
            `shouldBe` [Just "Some.Internal.Module"]
          fmap (fmap (GHC.moduleNameString . GHC.moduleName) . Plugins.nameModule_maybe . (.name)) exportingQualified
            `shouldBe` [Just "Some.Internal.Module"]

      it "includes non-exported top-level symbols from home modules" \fixture -> do
        result <-
          fixtureLore fixture do
            _ <- loadHomeModules defaultLoadHomeModulesOptions
            findSymbols "supportValues"

        length result `shouldBe` 1
        all (== Symbol'Unexported) (fmap (.visibility) result) `shouldBe` True
        fmap (\symbol -> maybe "" (GHC.moduleNameString . GHC.moduleName) (Plugins.nameModule_maybe symbol.name)) result
          `shouldBe` ["Demo.Support"]

      it "supports owner-qualified lookups for same-module DuplicateRecordFields selectors" \fixture -> do
        withFixtureCopy fixture \fixtureRoot -> do
          addRecordFieldLookupFixture fixtureRoot
          (leftTaggedSymbols, rightTaggedSymbols) <-
            fixtureLoreAt fixture fixtureRoot do
              _ <- loadHomeModules defaultLoadHomeModulesOptions
              leftTaggedSymbols <- findSymbols "Demo.sharedValue@LeftTagged"
              rightTaggedSymbols <- findSymbols "Demo.sharedValue@RightTagged"
              pure (leftTaggedSymbols, rightTaggedSymbols)

          let renderOcc = Plugins.getOccString . (.name)

          length leftTaggedSymbols `shouldBe` 1
          length rightTaggedSymbols `shouldBe` 1

          map renderOcc leftTaggedSymbols
            `shouldSatisfy` all (isInfixOf "sharedValue")
          map renderOcc rightTaggedSymbols
            `shouldSatisfy` all (isInfixOf "sharedValue")

      it "supports module-qualified dotted operators" \fixture -> do
        result <-
          fixtureLore fixture do
            _ <- loadHomeModules defaultLoadHomeModulesOptions
            findSymbols "Demo.Support..+."

        length result `shouldBe` 1
        fmap (Outputable.showSDocUnsafe . Outputable.ppr . name) result
          `shouldSatisfy` any (isInfixOf ".+.")

      it "supports parenthesized operator queries" \fixture -> do
        result <-
          fixtureLore fixture do
            _ <- loadHomeModules defaultLoadHomeModulesOptions
            findSymbols "(.+.)"

        length result `shouldBe` 1
        fmap (Outputable.showSDocUnsafe . Outputable.ppr . name) result
          `shouldSatisfy` any (isInfixOf ".+.")

      it "supports module-qualified parenthesized operator queries" \fixture -> do
        result <-
          fixtureLore fixture do
            _ <- loadHomeModules defaultLoadHomeModulesOptions
            findSymbols "Demo.Support.(.+.)"

        length result `shouldBe` 1
        fmap (Outputable.showSDocUnsafe . Outputable.ppr . name) result
          `shouldSatisfy` any (isInfixOf ".+.")

      it "survives two consecutive reloads before findSymbols" \fixture -> do
        result <-
          fixtureLore fixture do
            _ <- loadHomeModules defaultLoadHomeModulesOptions
            _ <- loadHomeModules defaultLoadHomeModulesOptions
            findSymbols "supportValues"

        length result `shouldBe` 1
        all (== Symbol'Unexported) (fmap (.visibility) result) `shouldBe` True
        fmap (\symbol -> maybe "" (GHC.moduleNameString . GHC.moduleName) (Plugins.nameModule_maybe symbol.name)) result
          `shouldBe` ["Demo.Support"]

    describe "listExportedSymbolsByModule" do
      it "lists exported symbols for the requested module and excludes unexported ones" \fixture -> do
        result <-
          fixtureLore fixture do
            _ <- loadHomeModules defaultLoadHomeModulesOptions
            listExportedSymbolsByModule "Demo.Support" Nothing

        let occNames = exportedNodeOccNames result
        occNames `shouldSatisfy` elem "supportSeed"
        occNames `shouldSatisfy` elem "supportStep"
        occNames `shouldSatisfy` elem "mkSupportRecord"
        occNames `shouldSatisfy` elem ".+."
        occNames `shouldSatisfy` not . elem "supportValues"

      it "returns an empty list when the module is not visible" \fixture -> do
        result <-
          fixtureLore fixture do
            _ <- loadHomeModules defaultLoadHomeModulesOptions
            listExportedSymbolsByModule "No.Such.Module" Nothing

        null result `shouldBe` True

      it "supports packageName when it references the loaded home package" \fixture -> do
        result <-
          fixtureLore fixture do
            _ <- loadHomeModules defaultLoadHomeModulesOptions
            listExportedSymbolsByModule "Demo.Support" (Just "demo-fixture")

        let occNames = exportedNodeOccNames result
        occNames `shouldSatisfy` elem "supportSeed"
        occNames `shouldSatisfy` elem "supportStep"
        occNames `shouldSatisfy` elem "mkSupportRecord"
        occNames `shouldSatisfy` elem ".+."
        occNames `shouldSatisfy` not . elem "supportValues"

      it "filters by direct surface type mentions instead of transitive metadata" \fixture -> do
        withFixtureCopy fixture \fixtureRoot -> do
          let moduleDir = fixtureRoot </> "src" </> "TestHint"
              moduleFile = moduleDir </> "Filter.hs"
          createDirectoryIfMissing True moduleDir
          TIO.writeFile moduleFile directTypeHintFixtureModuleSource

          result <-
            fixtureLoreAt fixture fixtureRoot do
              _ <- loadHomeModules defaultLoadHomeModulesOptions
              exports <- listExportedSymbolsByModule "TestHint.Filter" Nothing
              pure (filterExportedSymbolNodesByTypeHint "String" exports)

          let occNames = exportedNodeOccNames result
          occNames `shouldSatisfy` elem "directStringConsumer"
          occNames `shouldSatisfy` not . elem "WrapSomeException"

    describe "lookupIntersectingInstances" do
      it "intersects class instances across multiple symbol queries" \fixture -> do
        withFixtureInstances fixture \fixtureRoot -> do
          (queryMatchCounts, renderedInstances) <-
            fixtureLoreAt fixture fixtureRoot do
              _ <- loadHomeModules defaultLoadHomeModulesOptions
              lookupIntersectingInstancesForQueries False ["HasIndex", "Indexed"]

          queryMatchCounts `shouldSatisfy` all (> 0)
          renderedInstances `shouldSatisfy` matchesRenderedInstance "HasIndex (Indexed Int)"

      it "supports root-resolved symbol queries before intersecting instances" \fixture -> do
        withFixtureInstances fixture \fixtureRoot -> do
          (queryMatchCounts, renderedInstances) <-
            fixtureLoreAt fixture fixtureRoot do
              _ <- loadHomeModules defaultLoadHomeModulesOptions
              lookupIntersectingInstancesForRootQueries ["indexedValues", "HasIndex"]

          queryMatchCounts `shouldSatisfy` all (> 0)
          renderedInstances `shouldSatisfy` matchesRenderedInstance "HasIndex (Indexed Int)"

      it "supports module-qualified symbol queries" \fixture -> do
        withFixtureInstances fixture \fixtureRoot -> do
          (queryMatchCounts, renderedInstances) <-
            fixtureLoreAt fixture fixtureRoot do
              _ <- loadHomeModules defaultLoadHomeModulesOptions
              lookupIntersectingInstancesForQueries True ["Demo.Indexed", "HasIndex"]

          queryMatchCounts `shouldSatisfy` all (> 0)
          renderedInstances `shouldSatisfy` matchesRenderedInstance "HasIndex (Indexed Int)"

      it "intersects family instances across multiple symbol queries" \fixture -> do
        withFixtureInstances fixture \fixtureRoot -> do
          (queryMatchCounts, renderedInstances) <-
            fixtureLoreAt fixture fixtureRoot do
              _ <- loadHomeModules defaultLoadHomeModulesOptions
              lookupIntersectingInstancesForQueries False ["Elem", "Indexed"]

          queryMatchCounts `shouldSatisfy` all (> 0)
          renderedInstances `shouldSatisfy` matchesRenderedInstance "Elem (Indexed a) = a"

    describe "listDirectInstances" do
      it "filters out non-direct associated instances" \fixture -> do
        withFixtureIndirectInstances fixture \fixtureRoot -> do
          (renderedAssociated, renderedDirect) <-
            fixtureLoreAt fixture fixtureRoot do
              _ <- loadHomeModules defaultLoadHomeModulesOptions
              indexedSymbolInfos <- lookupRootSymbolInfo "Indexed"
              case indexedSymbolInfos of
                [] -> pure ([], [])
                indexedInfo : _ -> do
                  associated <- listAssociatedInstances indexedInfo.symbolName
                  direct <- listDirectInstances indexedInfo.symbolName
                  pure (renderInstances associated, renderInstances direct)

          renderedAssociated `shouldSatisfy` any (isInfixOf "HasIndex (Maybe (Indexed Int))")
          renderedDirect `shouldSatisfy` any (isInfixOf "HasIndex (Indexed Int)")
          renderedDirect `shouldSatisfy` not . any (isInfixOf "HasIndex (Maybe (Indexed Int))")

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

withFixtureInstances :: FixtureContext -> (FilePath -> IO a) -> IO a
withFixtureInstances fixture action =
  withFixtureCopy fixture \fixtureRoot -> do
    enableFlexibleInstances fixtureRoot
    appendDemoInstances fixtureRoot
    action fixtureRoot

withFixtureIndirectInstances :: FixtureContext -> (FilePath -> IO a) -> IO a
withFixtureIndirectInstances fixture action =
  withFixtureCopy fixture \fixtureRoot -> do
    enableFlexibleInstances fixtureRoot
    appendDemoInstances fixtureRoot
    appendDemoIndirectInstances fixtureRoot
    action fixtureRoot

appendDemoInstances :: FilePath -> IO ()
appendDemoInstances fixtureRoot = do
  let demoFile = fixtureRoot </> "src" </> "Demo.hs"
  TIO.appendFile demoFile instanceDefinitions

appendDemoIndirectInstances :: FilePath -> IO ()
appendDemoIndirectInstances fixtureRoot = do
  let demoFile = fixtureRoot </> "src" </> "Demo.hs"
  TIO.appendFile demoFile indirectInstanceDefinitions

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

indirectInstanceDefinitions :: T.Text
indirectInstanceDefinitions =
  T.unlines
    [ "",
      "instance HasIndex (Maybe (Indexed Int)) where",
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

lookupIntersectingInstancesForRootQueries :: (MonadLore m) => [T.Text] -> m ([Int], [String])
lookupIntersectingInstancesForRootQueries queries = do
  rootNamesPerQuery <- mapM resolvePreferredRootNames queries
  queryInstances <- mapM listUnionInstancesForNames rootNamesPerQuery
  let queryMatchCounts = map length rootNamesPerQuery
      intersectedInstances = intersectAllInstances queryInstances
  pure (queryMatchCounts, renderInstances intersectedInstances)

resolvePreferredRootSymbolInfos :: (MonadLore m) => T.Text -> m [SymbolInfo]
resolvePreferredRootSymbolInfos query = do
  rootNames <- resolvePreferredRootNames query
  catMaybes <$> mapM lookupSymbolInfo rootNames

resolvePreferredRootNames :: (MonadLore m) => T.Text -> m [GHC.Name]
resolvePreferredRootNames query = do
  symbols <- Set.toList <$> findMatchingSymbols (parseAndNormalizeName query)
  pathsToRoot <- mapM (resolvePathToRoot . (.name)) symbols
  let groupedByOccName =
        Map.fromListWith
          (<>)
          [ (T.pack (Plugins.getOccString rootName), [rootName])
          | pathToRoot <- pathsToRoot,
            let rootName = NE.last pathToRoot.unPathToRoot
          ]
  concat <$> mapM pickPreferredByOccName (Map.elems groupedByOccName)
  where
    pickPreferredByOccName [] =
      pure []
    pickPreferredByOccName namesForOcc = do
      symbolInfos <- catMaybes <$> mapM lookupSymbolInfo namesForOcc
      let nonValueNames =
            [ info.symbolName
            | info <- symbolInfos,
              classifySymbolCategory info.symbolThing /= SymbolValue
            ]
      pure $
        case nonValueNames of
          preferredName : _ -> [preferredName]
          [] -> take 1 namesForOcc

listUnionInstancesForNames :: (MonadLore m) => [GHC.Name] -> m Instances
listUnionInstancesForNames names = do
  instancesPerName <- mapM listAssociatedInstances names
  pure (List.foldl' unionInstances (Instances [] []) instancesPerName)

listUnionInstancesForSymbols :: (MonadLore m) => [Symbol] -> m Instances
listUnionInstancesForSymbols symbols = do
  instancesPerSymbol <- mapM (listAssociatedInstances . (.name)) symbols
  pure (List.foldl' unionInstances (Instances [] []) instancesPerSymbol)

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
    List.foldl' intersectInstances firstInstances restInstances

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

addReexportQualifierFixture :: FilePath -> IO ()
addReexportQualifierFixture fixtureRoot = do
  let internalDir = fixtureRoot </> "src" </> "Some" </> "Internal"
      exportingDir = fixtureRoot </> "src" </> "Some" </> "Exporting"
      internalFile = internalDir </> "Module.hs"
      exportingFile = exportingDir </> "Module.hs"
      demoFile = fixtureRoot </> "src" </> "Demo.hs"
  createDirectoryIfMissing True internalDir
  createDirectoryIfMissing True exportingDir
  TIO.writeFile internalFile reexportInternalModuleSource
  TIO.writeFile exportingFile reexportingModuleSource
  demoSource <- TIO.readFile demoFile
  let sourceWithImport =
        if T.isInfixOf reexportDemoImportLine demoSource
          then demoSource
          else T.replace reexportDemoImportAnchor (reexportDemoImportAnchor <> reexportDemoImportLine <> "\n") demoSource
      sourceWithFixtureValue =
        if T.isInfixOf reexportDemoValueAnchor sourceWithImport
          then sourceWithImport
          else sourceWithImport <> "\n\n" <> reexportDemoValueAnchor
  TIO.writeFile demoFile sourceWithFixtureValue

reexportInternalModuleSource :: T.Text
reexportInternalModuleSource =
  T.unlines
    [ "module Some.Internal.Module (foo) where",
      "",
      "foo :: Int",
      "foo = 42"
    ]

reexportingModuleSource :: T.Text
reexportingModuleSource =
  T.unlines
    [ "module Some.Exporting.Module (module Some.Internal.Module) where",
      "",
      "import Some.Internal.Module"
    ]

reexportDemoImportAnchor :: T.Text
reexportDemoImportAnchor =
  "import qualified Demo.Support as Support (SupportRecord, mkSupportRecord, supportSeed, supportStep)\n"

reexportDemoImportLine :: T.Text
reexportDemoImportLine =
  "import qualified Some.Exporting.Module as SomeExport (foo)"

reexportDemoValueAnchor :: T.Text
reexportDemoValueAnchor =
  T.unlines
    [ "_fixtureReexportedFoo :: Int",
      "_fixtureReexportedFoo = SomeExport.foo"
    ]

addRecordFieldLookupFixture :: FilePath -> IO ()
addRecordFieldLookupFixture fixtureRoot = do
  let demoFile = fixtureRoot </> "src" </> "Demo.hs"
  source <- TIO.readFile demoFile
  let sourceWithDuplicateRecordFields =
        if T.isInfixOf "{-# LANGUAGE DuplicateRecordFields #-}" source
          then source
          else "{-# LANGUAGE DuplicateRecordFields #-}\n" <> source
      sourceWithExports =
        T.replace recordFieldLookupExportAnchor recordFieldLookupExportReplacement sourceWithDuplicateRecordFields
  TIO.writeFile demoFile (sourceWithExports <> "\n\n" <> recordFieldLookupFixtureDeclarations)

recordFieldLookupExportAnchor :: T.Text
recordFieldLookupExportAnchor =
  T.unlines
    [ "    HasIndex (..),",
      "  )",
      "where"
    ]

recordFieldLookupExportReplacement :: T.Text
recordFieldLookupExportReplacement =
  T.unlines
    [ "    HasIndex (..),",
      "    User(..),",
      "    Hidden(Hidden),",
      "    publicFn,",
      "  )",
      "where"
    ]

recordFieldLookupFixtureDeclarations :: T.Text
recordFieldLookupFixtureDeclarations =
  T.unlines
    [ "data User = User",
      "  { userName :: String",
      "  }",
      "",
      "data Hidden = Hidden",
      "  { hiddenValue :: Int",
      "  }",
      "",
      "data LeftTagged = LeftTagged",
      "  { sharedValue :: Int",
      "  }",
      "",
      "data RightTagged = RightTagged",
      "  { sharedValue :: String",
      "  }",
      "",
      "publicFn :: Hidden -> Int",
      "publicFn = hiddenValue"
    ]
