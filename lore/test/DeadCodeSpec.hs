module DeadCodeSpec (spec) where

import qualified Data.List as List
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified GHC.Plugins as GHC
import Lore
  ( DeadCodeOptions (..),
    DeadCodeResult (..),
    DeadDefinition (..),
    DefinitionSource (..),
    Diagnostic (..),
    LoadHomeModulesResult (..),
    MonadLore,
    Symbol (..),
    defaultLoadHomeModulesOptions,
    findDeadCode,
    loadHomeModules,
  )
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
import System.FilePath ((</>))
import Test.Hspec
import TestSupport (FixtureContext, findSymbols, fixtureLoreAt, withFixtureCopy, withFixtureSpec)

spec :: Spec
spec = withFixtureSpec do
  describe "findDeadCode" do
    it "keeps transitive dependencies reachable from executable main alive" \fixture -> do
      withFixtureDeadCodeProject fixture \fixtureRoot -> do
        result <-
          runFindDeadCode fixture fixtureRoot defaultDeadCodeOptions
        let deadNames = deadDefinitionOccNames result
        deadNames `shouldContain` ["deadRoot", "deadDependency"]
        deadNames `shouldNotContain` ["liveRoot", "liveDependency"]
        deadNames `shouldNotContain` ["testMainHelper"]

    it "filters reported dead definitions by target module without changing global reachability" \fixture -> do
      withFixtureDeadCodeProject fixture \fixtureRoot -> do
        result <-
          fixtureLoreAt fixture fixtureRoot do
            requireLoadedHomeModules
            targetModule <- requireSymbolModule "DeadCode.Lib" "deadRoot"
            findDeadCode defaultDeadCodeOptions {deadCodeTargetModules = Just (Set.singleton targetModule)}

        let deadNames = deadDefinitionOccNames result
        deadNames `shouldContain` ["deadRoot", "deadDependency"]
        deadNames `shouldNotContain` ["liveRoot", "liveDependency"]
        result.deadCodeTotalDefinitions `shouldSatisfy` (> 0)

    it "treats alive modules as root sets" \fixture -> do
      withFixtureDeadCodeProject fixture \fixtureRoot -> do
        result <-
          fixtureLoreAt fixture fixtureRoot do
            requireLoadedHomeModules
            aliveModule <- requireSymbolModule "Dev" "runSomething"
            findDeadCode defaultDeadCodeOptions {deadCodeAliveModules = Set.singleton aliveModule}

        let deadNames = deadDefinitionOccNames result
        deadNames `shouldNotContain` ["deadRoot", "deadDependency"]

    it "treats alive symbols as roots" \fixture -> do
      withFixtureDeadCodeProject fixture \fixtureRoot -> do
        result <-
          fixtureLoreAt fixture fixtureRoot do
            requireLoadedHomeModules
            deadRoot <- requireSymbol "DeadCode.Lib" "deadRoot"
            findDeadCode defaultDeadCodeOptions {deadCodeAliveNames = Set.singleton deadRoot}

        let deadNames = deadDefinitionOccNames result
        deadNames `shouldNotContain` ["deadRoot", "deadDependency"]

    it "does not treat test mains as alive roots" \fixture -> do
      withFixtureDeadCodeProject fixture \fixtureRoot -> do
        result <-
          runFindDeadCode fixture fixtureRoot defaultDeadCodeOptions
        deadDefinitionOccNames result `shouldContain` ["testOnly"]

    it "keeps used type-family instances alive" \fixture -> do
      withFixtureDeadCodeProject fixture \fixtureRoot -> do
        result <-
          runFindDeadCode fixture fixtureRoot defaultDeadCodeOptions
        let moduleDeadNames =
              deadDefinitionOccNamesInModule "DeadCode.TypeFamily" result
        moduleDeadNames `shouldBe` []

    it "reports unused type/data family instances as dead" \fixture -> do
      withFixtureDeadCodeProject fixture \fixtureRoot -> do
        result <-
          runFindDeadCode fixture fixtureRoot defaultDeadCodeOptions
        let deadNames = deadDefinitionOccNames result
        deadNames `shouldContainPrefix` "D:R:UnusedDisplay"

    it "keeps unused external-only type/data family instances alive by default" \fixture -> do
      withFixtureDeadCodeProject fixture \fixtureRoot -> do
        result <-
          runFindDeadCode fixture fixtureRoot defaultDeadCodeOptions
        let deadNames = deadDefinitionOccNames result
        deadNames `shouldNotContainPrefix` "D:R:ExternalDisplay"

    it "reports unused class instances as dead" \fixture -> do
      withFixtureDeadCodeProject fixture \fixtureRoot -> do
        result <-
          runFindDeadCode fixture fixtureRoot defaultDeadCodeOptions
        let deadNames = deadDefinitionOccNames result
        deadNames `shouldContain` ["UnusedClassData"]
        deadNames `shouldContainPrefix` "$fShowUnusedClassData"

    it "keeps instances with only external head types alive by default" \fixture -> do
      withFixtureDeadCodeProject fixture \fixtureRoot -> do
        result <-
          runFindDeadCode fixture fixtureRoot defaultDeadCodeOptions
        let moduleDeadNames =
              deadDefinitionOccNamesInModule "DeadCode.ExternalOnlyInstance" result
        moduleDeadNames `shouldNotContainPrefix` "$fExternalOnlyInt"

    it "reports unused derived instances as dead" \fixture -> do
      withFixtureDeadCodeProject fixture \fixtureRoot -> do
        result <-
          runFindDeadCode fixture fixtureRoot defaultDeadCodeOptions
        let moduleDeadNames =
              deadDefinitionOccNamesInModule "DeadCode.UnusedDerived" result
        moduleDeadNames `shouldContain` ["DerivedData"]
        moduleDeadNames `shouldContainPrefix` "$fGenericDerivedData"

    it "keeps associated type family instances declared in class instances alive" \fixture -> do
      withFixtureDeadCodeProject fixture \fixtureRoot -> do
        result <-
          fixtureLoreAt fixture fixtureRoot do
            requireLoadedHomeModules
            wrapperType <- requireSymbol "DeadCode.AssociatedTypeInstance" "Wrapper"
            findDeadCode defaultDeadCodeOptions {deadCodeAliveNames = Set.singleton wrapperType}
        let moduleDeadNames =
              deadDefinitionOccNamesInModule "DeadCode.AssociatedTypeInstance" result
        moduleDeadNames `shouldNotContainPrefix` "$fHasAssocInt"
        moduleDeadNames `shouldNotContainPrefix` "$fHasAssocWrapper"
        moduleDeadNames `shouldNotContainPrefix` "D:R:AssocWrapper"

    it "keeps associated type family instances alive for multi-parameter class instances" \fixture -> do
      withFixtureDeadCodeProject fixture \fixtureRoot -> do
        result <-
          fixtureLoreAt fixture fixtureRoot do
            requireLoadedHomeModules
            wrapperType <- requireSymbol "DeadCode.AssociatedTypeInstanceMulti" "Wrapper2"
            findDeadCode defaultDeadCodeOptions {deadCodeAliveNames = Set.singleton wrapperType}
        let moduleDeadNames =
              deadDefinitionOccNamesInModule "DeadCode.AssociatedTypeInstanceMulti" result
        moduleDeadNames `shouldNotContainPrefix` "$fHasAssoc2IntBool"
        moduleDeadNames `shouldNotContainPrefix` "$fHasAssoc2Wrapper2"
        moduleDeadNames `shouldNotContainPrefix` "D:R:Assoc2Wrapper2"

    it "keeps associated type family instances alive when class is imported" \fixture -> do
      withFixtureDeadCodeProject fixture \fixtureRoot -> do
        result <-
          runFindDeadCode fixture fixtureRoot defaultDeadCodeOptions
        let moduleDeadNames =
              deadDefinitionOccNamesInModule "DeadCode.AssociatedTypeImportedInstance" result
        moduleDeadNames `shouldNotContainPrefix` "D:R:AssocImported"

    it "keeps class instances alive when any instance-head type is alive" \fixture -> do
      withFixtureDeadCodeProject fixture \fixtureRoot -> do
        result <-
          fixtureLoreAt fixture fixtureRoot do
            requireLoadedHomeModules
            headAliveType <- requireSymbol "DeadCode.InstanceHeadAlive" "HeadAlive"
            findDeadCode defaultDeadCodeOptions {deadCodeAliveNames = Set.singleton headAliveType}
        let moduleDeadNames =
              deadDefinitionOccNamesInModule "DeadCode.InstanceHeadAlive" result
        moduleDeadNames `shouldNotContainPrefix` "$fHeadClassHeadAlive"

    it "keeps plain class instances alive when any instance-head type is alive" \fixture -> do
      withFixtureDeadCodeProject fixture \fixtureRoot -> do
        result <-
          fixtureLoreAt fixture fixtureRoot do
            requireLoadedHomeModules
            headAliveType <- requireSymbol "DeadCode.PlainInstanceHeadAlive" "HeadAlive2"
            findDeadCode defaultDeadCodeOptions {deadCodeAliveNames = Set.singleton headAliveType}
        let moduleDeadNames =
              deadDefinitionOccNamesInModule "DeadCode.PlainInstanceHeadAlive" result
        moduleDeadNames `shouldNotContainPrefix` "$fMarkerHeadAlive2"

    it "reports entry-module resolution warnings instead of silently dropping them" \fixture -> do
      withFixtureDeadCodeProject fixture \fixtureRoot -> do
        TIO.appendFile (fixtureRoot </> "package.yaml") brokenEntryPackageSuffix
        result <-
          runFindDeadCode fixture fixtureRoot defaultDeadCodeOptions
        map T.unpack result.deadCodeWarnings
          `shouldSatisfy` any (List.isInfixOf "Failed to resolve entry module for")

defaultDeadCodeOptions :: DeadCodeOptions
defaultDeadCodeOptions =
  DeadCodeOptions
    { deadCodeTargetModules = Nothing,
      deadCodeAliveModules = Set.empty,
      deadCodeAliveNames = Set.empty
    }

runFindDeadCode :: FixtureContext -> FilePath -> DeadCodeOptions -> IO DeadCodeResult
runFindDeadCode fixture fixtureRoot options =
  fixtureLoreAt fixture fixtureRoot do
    requireLoadedHomeModules
    findDeadCode options

requireSymbol :: (MonadLore m) => String -> String -> m GHC.Name
requireSymbol moduleName occName = do
  symbols <- findSymbols (T.pack (moduleName <> "." <> occName))
  case List.find (matchesSymbol moduleName occName) symbols of
    Just symbol ->
      pure symbol.name
    Nothing ->
      error ("failed to resolve symbol: " <> moduleName <> "." <> occName)

requireSymbolModule :: (MonadLore m) => String -> String -> m GHC.Module
requireSymbolModule moduleName occName = do
  symbolName <- requireSymbol moduleName occName
  case GHC.nameModule_maybe symbolName of
    Just module_ ->
      pure module_
    Nothing ->
      error ("resolved symbol has no module: " <> moduleName <> "." <> occName)

requireLoadedHomeModules :: (MonadLore m) => m ()
requireLoadedHomeModules = do
  loadResult <- loadHomeModules defaultLoadHomeModulesOptions
  if loadResult.loadHomeModulesSucceeded
    then pure ()
    else error (renderLoadFailure loadResult)

renderLoadFailure :: LoadHomeModulesResult -> String
renderLoadFailure loadResult =
  unlines $
    [ "loadHomeModules failed in dead-code fixture",
      "loaded=" <> show loadResult.loadHomeModulesLoaded,
      "failed=" <> show loadResult.loadHomeModulesFailed,
      "total=" <> show loadResult.loadHomeModulesTotal
    ]
      <> map (T.unpack . (.diagnosticMessage)) loadResult.loadHomeModulesDiagnostics

matchesSymbol :: String -> String -> Symbol -> Bool
matchesSymbol moduleName occName symbol =
  case GHC.nameModule_maybe symbol.name of
    Nothing ->
      False
    Just module_ ->
      GHC.moduleNameString (GHC.moduleName module_) == moduleName
        && GHC.getOccString symbol.name == occName

deadDefinitionOccNames :: DeadCodeResult -> [String]
deadDefinitionOccNames result =
  [ GHC.getOccString name
  | deadDefinition <- result.deadCodeDeadDefinitions,
    name <- Set.toList deadDefinition.deadDefinitionNames
  ]

deadDefinitionOccNamesInModule :: String -> DeadCodeResult -> [String]
deadDefinitionOccNamesInModule moduleName result =
  [ GHC.getOccString name
  | deadDefinition <- result.deadCodeDeadDefinitions,
    isDefinitionInModule moduleName deadDefinition,
    name <- Set.toList deadDefinition.deadDefinitionNames
  ]

shouldNotContainPrefix :: [String] -> String -> Expectation
shouldNotContainPrefix names prefix =
  any (List.isPrefixOf prefix) names `shouldBe` False

shouldContainPrefix :: [String] -> String -> Expectation
shouldContainPrefix names prefix =
  any (List.isPrefixOf prefix) names `shouldBe` True

withFixtureDeadCodeProject :: FixtureContext -> (FilePath -> IO a) -> IO a
withFixtureDeadCodeProject fixture action =
  withFixtureCopy fixture \fixtureRoot -> do
    writeFixtureDeadCodeModules fixtureRoot
    appendFixtureDeadCodeComponents fixtureRoot
    action fixtureRoot

writeFixtureDeadCodeModules :: FilePath -> IO ()
writeFixtureDeadCodeModules fixtureRoot = do
  let srcDir = fixtureRoot </> "src" </> "DeadCode"
      appDir = fixtureRoot </> "app"
      devDir = fixtureRoot </> "src"
      testDir = fixtureRoot </> "test"
      testSupportDir = fixtureRoot </> "test" </> "TestOnly"
  createDirectoryIfMissing True srcDir
  createDirectoryIfMissing True appDir
  createDirectoryIfMissing True devDir
  createDirectoryIfMissing True testDir
  createDirectoryIfMissing True testSupportDir
  TIO.writeFile (srcDir </> "Lib.hs") deadCodeLibSource
  TIO.writeFile (srcDir </> "TypeclassMetadata.hs") deadCodeTypeclassMetadataSource
  TIO.writeFile (srcDir </> "TypeFamily.hs") deadCodeTypeFamilySource
  TIO.writeFile (srcDir </> "UnusedTypeFamily.hs") deadCodeUnusedTypeFamilySource
  TIO.writeFile (srcDir </> "ExternalOnlyTypeFamily.hs") deadCodeExternalOnlyTypeFamilySource
  TIO.writeFile (srcDir </> "UnusedClassInstance.hs") deadCodeUnusedClassInstanceSource
  TIO.writeFile (srcDir </> "ExternalOnlyInstance.hs") deadCodeExternalOnlyInstanceSource
  TIO.writeFile (srcDir </> "UnusedDerived.hs") deadCodeUnusedDerivedSource
  TIO.writeFile (srcDir </> "AssociatedTypeInstance.hs") deadCodeAssociatedTypeInstanceSource
  TIO.writeFile (srcDir </> "AssociatedTypeInstanceMulti.hs") deadCodeAssociatedTypeInstanceMultiSource
  TIO.writeFile (srcDir </> "AssociatedTypeClass.hs") deadCodeAssociatedTypeClassSource
  TIO.writeFile (srcDir </> "AssociatedTypeImportedInstance.hs") deadCodeAssociatedTypeImportedInstanceSource
  TIO.writeFile (srcDir </> "InstanceHeadAlive.hs") deadCodeInstanceHeadAliveSource
  TIO.writeFile (srcDir </> "PlainInstanceHeadAlive.hs") deadCodePlainInstanceHeadAliveSource
  TIO.writeFile (devDir </> "Dev.hs") deadCodeDevSource
  TIO.writeFile (appDir </> "Main.hs") deadCodeMainSource
  TIO.writeFile (testDir </> "SpecMain.hs") deadCodeTestMainSource
  TIO.writeFile (testSupportDir </> "Helper.hs") deadCodeTestSupportSource

appendFixtureDeadCodeComponents :: FilePath -> IO ()
appendFixtureDeadCodeComponents fixtureRoot = do
  TIO.appendFile (fixtureRoot </> "package.yaml") deadCodePackageSuffix
  let fixtureCabalFile = fixtureRoot </> "demo-fixture.cabal"
  fixtureCabalExists <- doesFileExist fixtureCabalFile
  if fixtureCabalExists
    then removeFile fixtureCabalFile
    else pure ()

deadCodeLibSource :: Text
deadCodeLibSource =
  T.unlines
    [ "module DeadCode.Lib where",
      "",
      "import DeadCode.TypeclassMetadata (useLiveTool)",
      "",
      "liveRoot :: Int",
      "liveRoot = liveDependency + useLiveTool",
      "",
      "liveDependency :: Int",
      "liveDependency = 1",
      "",
      "deadRoot :: Int",
      "deadRoot = deadDependency",
      "",
      "deadDependency :: Int",
      "deadDependency = 2",
      "",
      "testOnly :: Int",
      "testOnly = 3"
    ]

deadCodeTypeclassMetadataSource :: Text
deadCodeTypeclassMetadataSource =
  T.unlines
    [ "{-# LANGUAGE DataKinds #-}",
      "{-# LANGUAGE FlexibleContexts #-}",
      "{-# LANGUAGE FlexibleInstances #-}",
      "{-# LANGUAGE GADTs #-}",
      "{-# LANGUAGE MultiParamTypeClasses #-}",
      "{-# LANGUAGE ScopedTypeVariables #-}",
      "{-# LANGUAGE TypeApplications #-}",
      "{-# LANGUAGE AllowAmbiguousTypes #-}",
      "",
      "module DeadCode.TypeclassMetadata where",
      "",
      "import Data.Proxy (Proxy (..))",
      "import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)",
      "",
      "data FieldMetadata a metadata = FieldMetadata",
      "",
      "data Description (description :: Symbol)",
      "",
      "class IsExample a (example :: Symbol) where",
      "  exampleToJSON :: String",
      "",
      "instance KnownSymbol description => IsExample String description where",
      "  exampleToJSON = symbolVal (Proxy @description)",
      "",
      "class IsFieldMetadata a metadata where",
      "  modifySchema :: String",
      "",
      "instance IsExample String description => IsFieldMetadata String (Description description) where",
      "  modifySchema = exampleToJSON @String @description",
      "",
      "class ToSchema a where",
      "  schema :: String",
      "",
      "instance IsFieldMetadata String metadata => ToSchema (FieldMetadata String metadata) where",
      "  schema = modifySchema @String @metadata",
      "",
      "data SomeTool where",
      "  SomeTool :: ToSchema a => Proxy a -> SomeTool",
      "",
      "liveTool :: SomeTool",
      "liveTool = SomeTool (Proxy @(FieldMetadata String (Description \"x\")))",
      "",
      "useLiveTool :: Int",
      "useLiveTool =",
      "  case liveTool of",
      "    SomeTool _ -> 1"
    ]

deadCodeDevSource :: Text
deadCodeDevSource =
  T.unlines
    [ "module Dev where",
      "",
      "import DeadCode.Lib (deadRoot)",
      "",
      "runSomething :: Int",
      "runSomething = deadRoot"
    ]

deadCodeMainSource :: Text
deadCodeMainSource =
  T.unlines
    [ "module Main where",
      "",
      "import DeadCode.Lib (liveRoot)",
      "import DeadCode.AssociatedTypeInstance (useWrapperAssoc)",
      "import DeadCode.AssociatedTypeInstanceMulti (useWrapperAssoc2)",
      "import DeadCode.AssociatedTypeImportedInstance (useImportedAssoc)",
      "import DeadCode.InstanceHeadAlive (useHeadAlive)",
      "import DeadCode.PlainInstanceHeadAlive (useHeadAlive2)",
      "import DeadCode.TypeFamily (someAliveFunction)",
      "",
      "main :: IO ()",
      "main = print (liveRoot + length someAliveFunction + useWrapperAssoc + useWrapperAssoc2 + useImportedAssoc + useHeadAlive + useHeadAlive2)"
    ]

deadCodeTypeFamilySource :: Text
deadCodeTypeFamilySource =
  T.unlines
    [ "module DeadCode.TypeFamily where",
      "",
      "data LocalArg = LocalArg",
      "",
      "type family DisplayType a",
      "type instance DisplayType LocalArg = String",
      "",
      "someAliveFunction :: DisplayType LocalArg",
      "someAliveFunction = \"This is a string, but DisplayType LocalArg resolves to String\""
    ]

deadCodeUnusedTypeFamilySource :: Text
deadCodeUnusedTypeFamilySource =
  T.unlines
    [ "module DeadCode.UnusedTypeFamily where",
      "",
      "data UnusedLocal = UnusedLocal",
      "",
      "type family UnusedDisplay a",
      "type instance UnusedDisplay UnusedLocal = Int"
    ]

deadCodeExternalOnlyTypeFamilySource :: Text
deadCodeExternalOnlyTypeFamilySource =
  T.unlines
    [ "module DeadCode.ExternalOnlyTypeFamily where",
      "",
      "type family ExternalDisplay a",
      "type instance ExternalDisplay Bool = Int"
    ]

deadCodeUnusedClassInstanceSource :: Text
deadCodeUnusedClassInstanceSource =
  T.unlines
    [ "module DeadCode.UnusedClassInstance where",
      "",
      "data UnusedClassData = UnusedClassData",
      "",
      "instance Show UnusedClassData where",
      "  show _ = \"unused\""
    ]

deadCodeExternalOnlyInstanceSource :: Text
deadCodeExternalOnlyInstanceSource =
  T.unlines
    [ "module DeadCode.ExternalOnlyInstance where",
      "",
      "class ExternalOnly a where",
      "  externalOnlyTag :: proxy a -> Int",
      "",
      "instance ExternalOnly Int where",
      "  externalOnlyTag _ = 42"
    ]

deadCodeUnusedDerivedSource :: Text
deadCodeUnusedDerivedSource =
  T.unlines
    [ "{-# LANGUAGE DeriveGeneric #-}",
      "",
      "module DeadCode.UnusedDerived where",
      "",
      "import GHC.Generics (Generic)",
      "",
      "data DerivedData = DerivedData deriving (Generic)"
    ]

deadCodeAssociatedTypeInstanceSource :: Text
deadCodeAssociatedTypeInstanceSource =
  T.unlines
    [ "{-# LANGUAGE ScopedTypeVariables #-}",
      "{-# LANGUAGE TypeApplications #-}",
      "{-# LANGUAGE TypeFamilies #-}",
      "",
      "module DeadCode.AssociatedTypeInstance where",
      "",
      "import Data.Proxy (Proxy (..))",
      "",
      "data Wrapper api = Wrapper",
      "",
      "class HasAssoc api where",
      "  type Assoc api :: *",
      "  marker :: proxy api -> Int",
      "",
      "instance HasAssoc Int where",
      "  type Assoc Int = String",
      "  marker _ = 1",
      "",
      "instance HasAssoc api => HasAssoc (Wrapper api) where",
      "  type Assoc (Wrapper api) = Assoc api",
      "  marker _ = marker @api Proxy",
      "",
      "useWrapperAssoc :: Int",
      "useWrapperAssoc = marker @(Wrapper Int) Proxy"
    ]

deadCodeAssociatedTypeInstanceMultiSource :: Text
deadCodeAssociatedTypeInstanceMultiSource =
  T.unlines
    [ "{-# LANGUAGE FlexibleInstances #-}",
      "{-# LANGUAGE MultiParamTypeClasses #-}",
      "{-# LANGUAGE ScopedTypeVariables #-}",
      "{-# LANGUAGE TypeApplications #-}",
      "{-# LANGUAGE TypeFamilies #-}",
      "",
      "module DeadCode.AssociatedTypeInstanceMulti where",
      "",
      "import Data.Kind (Type)",
      "import Data.Proxy (Proxy (..))",
      "",
      "data Wrapper2 api = Wrapper2",
      "",
      "class HasAssoc2 a b where",
      "  type Assoc2 a b x :: Type",
      "  marker2 :: proxy a -> proxy b -> Int",
      "",
      "instance HasAssoc2 Int Bool where",
      "  type Assoc2 Int Bool x = [x]",
      "  marker2 _ _ = 2",
      "",
      "instance HasAssoc2 a b => HasAssoc2 (Wrapper2 a) b where",
      "  type Assoc2 (Wrapper2 a) b x = Assoc2 a b x",
      "  marker2 _ _ = marker2 @a @b Proxy Proxy",
      "",
      "useWrapperAssoc2 :: Int",
      "useWrapperAssoc2 = marker2 @(Wrapper2 Int) @Bool Proxy Proxy"
    ]

deadCodeAssociatedTypeClassSource :: Text
deadCodeAssociatedTypeClassSource =
  T.unlines
    [ "{-# LANGUAGE ScopedTypeVariables #-}",
      "{-# LANGUAGE TypeApplications #-}",
      "{-# LANGUAGE TypeFamilies #-}",
      "",
      "module DeadCode.AssociatedTypeClass where",
      "",
      "import Data.Kind (Type)",
      "import Data.Proxy (Proxy (..))",
      "",
      "class HasAssocImported api where",
      "  type AssocImported api :: Type",
      "  markerImported :: proxy api -> Int",
      "",
      "instance HasAssocImported Int where",
      "  type AssocImported Int = String",
      "  markerImported _ = 3",
      "",
      "instance HasAssocImported api => HasAssocImported [api] where",
      "  type AssocImported [api] = AssocImported api",
      "  markerImported _ = markerImported @api Proxy"
    ]

deadCodeAssociatedTypeImportedInstanceSource :: Text
deadCodeAssociatedTypeImportedInstanceSource =
  T.unlines
    [ "{-# LANGUAGE ScopedTypeVariables #-}",
      "{-# LANGUAGE TypeApplications #-}",
      "{-# LANGUAGE TypeFamilies #-}",
      "",
      "module DeadCode.AssociatedTypeImportedInstance where",
      "",
      "import Data.Proxy (Proxy (..))",
      "import DeadCode.AssociatedTypeClass (HasAssocImported (..))",
      "",
      "data ImportedWrapper api = ImportedWrapper",
      "",
      "instance HasAssocImported api => HasAssocImported (ImportedWrapper api) where",
      "  type AssocImported (ImportedWrapper api) = AssocImported api",
      "  markerImported _ = markerImported @api Proxy",
      "",
      "useImportedAssoc :: Int",
      "useImportedAssoc = markerImported @(ImportedWrapper Int) Proxy"
    ]

deadCodeInstanceHeadAliveSource :: Text
deadCodeInstanceHeadAliveSource =
  T.unlines
    [ "{-# LANGUAGE TypeFamilies #-}",
      "",
      "module DeadCode.InstanceHeadAlive where",
      "",
      "import Data.Kind (Type)",
      "",
      "data HeadAlive = HeadAlive",
      "",
      "class HeadClass a where",
      "  type HeadAssoc a :: Type",
      "",
      "instance HeadClass HeadAlive where",
      "  type HeadAssoc HeadAlive = Int",
      "",
      "useHeadAlive :: Int",
      "useHeadAlive = case HeadAlive of HeadAlive -> 1"
    ]

deadCodePlainInstanceHeadAliveSource :: Text
deadCodePlainInstanceHeadAliveSource =
  T.unlines
    [ "module DeadCode.PlainInstanceHeadAlive where",
      "",
      "data HeadAlive2 = HeadAlive2",
      "",
      "class Marker a",
      "",
      "instance Marker HeadAlive2",
      "",
      "useHeadAlive2 :: Int",
      "useHeadAlive2 = case HeadAlive2 of HeadAlive2 -> 1"
    ]

deadCodeTestMainSource :: Text
deadCodeTestMainSource =
  T.unlines
    [ "module Main where",
      "",
      "import DeadCode.Lib (testOnly)",
      "import TestOnly.Helper (testMainHelper)",
      "",
      "main :: IO ()",
      "main = print (testOnly + testMainHelper)"
    ]

deadCodeTestSupportSource :: Text
deadCodeTestSupportSource =
  T.unlines
    [ "module TestOnly.Helper where",
      "",
      "testMainHelper :: Int",
      "testMainHelper = 4"
    ]

deadCodePackageSuffix :: Text
deadCodePackageSuffix =
  T.unlines
    [ "",
      "executables:",
      "  demo-fixture-exe:",
      "    main: Main.hs",
      "    source-dirs:",
      "    - app",
      "    dependencies:",
      "    - base",
      "    - demo-fixture",
      "",
      "tests:",
      "  demo-fixture-test:",
      "    main: SpecMain.hs",
      "    source-dirs:",
      "    - test",
      "    dependencies:",
      "    - base",
      "    - demo-fixture"
    ]

brokenEntryPackageSuffix :: Text
brokenEntryPackageSuffix =
  T.unlines
    [ "",
      "  demo-fixture-broken-exe:",
      "    main: MissingMain.hs",
      "    source-dirs:",
      "    - app",
      "    dependencies:",
      "    - base",
      "    - demo-fixture"
    ]

isDefinitionInModule :: String -> DeadDefinition -> Bool
isDefinitionInModule moduleName deadDefinition =
  GHC.moduleNameString (GHC.moduleName deadDefinition.deadDefinitionSource.definitionSourceModule) == moduleName
