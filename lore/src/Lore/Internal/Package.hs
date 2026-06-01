{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Move filter" #-}
module Lore.Internal.Package
  ( ComponentKind (..),
    ComponentData (..),
    PackageData (..),
    prepareComponentsData,
    discoverProject,
    componentMainModulePathCandidates,
    normalizeRelativePath,
    firstExistingPath,
    commonSetIntersection,
    extractDependencies,
    extractSourceDirs,
  )
where

import Control.Exception (IOException, try)
import Control.Monad (forM, unless)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.RWS (asks)
import qualified Data.ByteString as BS
import Data.Either (partitionEithers)
import Data.List (intercalate, isPrefixOf)
import Data.Maybe (maybeToList)
import qualified Data.Set as Set
import qualified Distribution.Compiler as CabalCompiler
import qualified Distribution.ModuleName as CabalModuleName
import qualified Distribution.Package as CabalPackage
import qualified Distribution.PackageDescription as Cabal
import qualified Distribution.PackageDescription.Configuration as CabalConfig
import qualified Distribution.PackageDescription.Parsec as CabalParsec
import qualified Distribution.System as CabalSystem
import qualified Distribution.Types.ComponentRequestedSpec as CabalRequested
import qualified Distribution.Types.Dependency as CabalDependency
import qualified Distribution.Types.LibraryName as CabalLibraryName
import qualified Distribution.Types.PackageName as CabalPackageName
import qualified Distribution.Types.UnqualComponentName as CabalComponentName
import qualified Distribution.Utils.Path as CabalPath
import qualified Distribution.Version as CabalVersion
import qualified GHC
import qualified Language.Haskell.Extension as CabalExtension
import Lore.Internal.Ghc.DynFlags (Extension (..), GhcOption (..), Language (..))
import Lore.Internal.Ghc.PackageEnvironment.Types (GhcEnvironmentSnapshot (..))
import Lore.Internal.Package.Materialize (materializeCabalPackageFiles)
import Lore.Internal.Package.Path
  ( commonSetIntersection,
    componentMainModulePathCandidates,
    extractDependencies,
    extractSourceDirs,
    firstExistingPath,
    normalizeRelativePath,
  )
import Lore.Internal.Package.Types (ComponentData (..), ComponentKind (..), PackageData (..))
import Lore.Internal.Session (SessionContext (..))
import qualified Lore.Logger as Log
import Lore.Monad (MonadLore)
import System.FilePath (takeDirectory)
import UnliftIO.Exception (throwString)

prepareComponentsData :: (MonadLore m) => m [PackageData]
prepareComponentsData = do
  ghcVersion <- asks (ghcEnvironmentCompilerVersion . ghcEnvironmentSnapshot)
  packageRoots <- asks sessionPackageRoots
  cabalFiles <- materializeCabalPackageFiles packageRoots
  Log.debug $
    "Loading .cabal manifests and extracting package/component data: "
      <> show cabalFiles
  eiPackages <- forM cabalFiles (processCabalPackage ghcVersion)
  let (errors, packages) = partitionEithers eiPackages
  unless (null errors) $
    throwString $
      intercalate
        "\n  -"
        ("Errors encountered while loading package .cabal files:" : errors)
  let componentsCount = length (concatMap (.components) packages)
  Log.debug $
    "Successfully loaded "
      <> show (length packages)
      <> " .cabal package manifests, resulting in "
      <> show componentsCount
      <> " components"
  pure packages

discoverProject :: (MonadLore m) => m [PackageData]
discoverProject =
  prepareComponentsData

processCabalPackage :: (MonadIO m) => CabalVersion.Version -> FilePath -> m (Either String PackageData)
processCabalPackage ghcVersion cabalFile = do
  eiCabalFileContent <- liftIO (try (BS.readFile cabalFile) :: IO (Either IOException BS.ByteString))
  pure $ do
    cabalFileContent <-
      case eiCabalFileContent of
        Left ioErr ->
          Left
            ( "Failed to read .cabal file "
                <> cabalFile
                <> ": "
                <> show ioErr
            )
        Right content -> Right content
    let (warnings, parseResult) =
          CabalParsec.runParseResult (CabalParsec.parseGenericPackageDescription cabalFileContent)
    genericPackageDescription <-
      case parseResult of
        Left (_maybeVersion, parseErrors) ->
          Left
            ( "Failed to parse .cabal file "
                <> cabalFile
                <> ": "
                <> show parseErrors
            )
        Right parsed -> Right parsed
    let compilerInfo =
          CabalCompiler.unknownCompilerInfo
            (CabalCompiler.CompilerId CabalCompiler.GHC ghcVersion)
            CabalCompiler.NoAbiTag
        platform = CabalSystem.Platform CabalSystem.buildArch CabalSystem.buildOS
        requestedComponents =
          CabalRequested.defaultComponentRequestedSpec
            { CabalRequested.testsRequested = True,
              CabalRequested.benchmarksRequested = True
            }
        finalized =
          CabalConfig.finalizePD
            mempty
            requestedComponents
            (const True)
            platform
            compilerInfo
            []
            genericPackageDescription
    (packageDescription, _flags) <-
      firstRight
        ("Failed to finalize package description for " <> cabalFile <> formatWarnings warnings)
        finalized
    pure (packageDataFromCabalDescription cabalFile packageDescription)

packageDataFromCabalDescription :: FilePath -> Cabal.PackageDescription -> PackageData
packageDataFromCabalDescription packageFile packageDescription =
  PackageData
    { packageManifestPath = packageFile,
      packageRoot = takeDirectory packageFile,
      packageName = CabalPackageName.unPackageName (CabalPackage.packageName packageDescription),
      components = extractCabalComponents packageDescription
    }

extractCabalComponents :: Cabal.PackageDescription -> [ComponentData]
extractCabalComponents packageDescription =
  maybeToList (mkMainLibraryComponent <$> Cabal.library packageDescription)
    <> map mkSubLibraryComponent (Cabal.subLibraries packageDescription)
    <> map mkExecutableComponent (Cabal.executables packageDescription)
    <> map mkTestComponent (Cabal.testSuites packageDescription)
    <> map mkBenchmarkComponent (Cabal.benchmarks packageDescription)
  where
    mkMainLibraryComponent libraryComponent =
      mkComponentData
        ComponentKindLibrary
        "library"
        Nothing
        (Cabal.libBuildInfo libraryComponent)
        (extractLibraryModules libraryComponent)

    mkSubLibraryComponent libraryComponent =
      let libraryComponentName =
            case Cabal.libName libraryComponent of
              CabalLibraryName.LMainLibName -> "library"
              CabalLibraryName.LSubLibName name -> "library:" <> CabalComponentName.unUnqualComponentName name
       in mkComponentData
            ComponentKindInternalLibrary
            libraryComponentName
            Nothing
            (Cabal.libBuildInfo libraryComponent)
            (extractLibraryModules libraryComponent)

    mkExecutableComponent executableComponent =
      mkComponentData
        ComponentKindExecutable
        ("executable:" <> CabalComponentName.unUnqualComponentName (Cabal.exeName executableComponent))
        (Just executableComponent.modulePath)
        executableComponent.buildInfo
        (extractBuildInfoModules executableComponent.buildInfo)

    mkTestComponent testSuite =
      mkComponentData
        ComponentKindTest
        ("test:" <> CabalComponentName.unUnqualComponentName (Cabal.testName testSuite))
        (extractTestMainModulePath testSuite)
        testSuite.testBuildInfo
        (extractTestModules testSuite)

    mkBenchmarkComponent benchmark =
      mkComponentData
        ComponentKindBenchmark
        ("benchmark:" <> CabalComponentName.unUnqualComponentName (Cabal.benchmarkName benchmark))
        (extractBenchmarkMainModulePath benchmark)
        benchmark.benchmarkBuildInfo
        (extractBenchmarkModules benchmark)

mkComponentData :: ComponentKind -> String -> Maybe FilePath -> Cabal.BuildInfo -> Set.Set GHC.ModuleName -> ComponentData
mkComponentData kind name maybeMainPath buildInfo modulesToLoad =
  ComponentData
    { componentKind = kind,
      componentName = name,
      mainModulePath = maybeMainPath,
      language = (Language . renderCabalLanguage) <$> buildInfo.defaultLanguage,
      ghcOptions = Set.fromList (map GhcOption (Cabal.hcOptions CabalCompiler.GHC buildInfo)),
      defaultExtensions = Set.fromList (map (Extension . renderCabalExtension) (buildInfo.defaultExtensions <> buildInfo.otherExtensions)),
      dependencies = Set.fromList (map (CabalPackageName.unPackageName . CabalDependency.depPkgName) buildInfo.targetBuildDepends),
      sourceDirs = normalizeSourceDirs (Set.fromList (map CabalPath.getSymbolicPath buildInfo.hsSourceDirs)),
      modules = modulesToLoad
    }

extractLibraryModules :: Cabal.Library -> Set.Set GHC.ModuleName
extractLibraryModules libraryComponent =
  Set.fromList
    [ cabalModuleNameToGhcModuleName moduleName
    | moduleName <- Cabal.exposedModules libraryComponent <> Cabal.otherModules (Cabal.libBuildInfo libraryComponent),
      let renderedModuleName = renderCabalModuleName moduleName,
      not (shouldDropModuleName renderedModuleName)
    ]

extractBuildInfoModules :: Cabal.BuildInfo -> Set.Set GHC.ModuleName
extractBuildInfoModules buildInfo =
  Set.fromList
    [ cabalModuleNameToGhcModuleName moduleName
    | moduleName <- Cabal.otherModules buildInfo,
      let renderedModuleName = renderCabalModuleName moduleName,
      not (shouldDropModuleName renderedModuleName)
    ]

extractTestModules :: Cabal.TestSuite -> Set.Set GHC.ModuleName
extractTestModules testSuite =
  Set.fromList
    [ cabalModuleNameToGhcModuleName moduleName
    | moduleName <- testLibraryModules <> testSuiteModules,
      let renderedModuleName = renderCabalModuleName moduleName,
      not (shouldDropModuleName renderedModuleName)
    ]
  where
    testSuiteModules = Cabal.testModules testSuite <> Cabal.otherModules testSuite.testBuildInfo
    testLibraryModules =
      case Cabal.testInterface testSuite of
        Cabal.TestSuiteLibV09 _ moduleName -> [moduleName]
        _ -> []

extractBenchmarkModules :: Cabal.Benchmark -> Set.Set GHC.ModuleName
extractBenchmarkModules benchmark =
  Set.fromList
    [ cabalModuleNameToGhcModuleName moduleName
    | moduleName <- Cabal.benchmarkModules benchmark <> Cabal.otherModules benchmark.benchmarkBuildInfo,
      let renderedModuleName = renderCabalModuleName moduleName,
      not (shouldDropModuleName renderedModuleName)
    ]

extractTestMainModulePath :: Cabal.TestSuite -> Maybe FilePath
extractTestMainModulePath testSuite =
  case Cabal.testInterface testSuite of
    Cabal.TestSuiteExeV10 _ mainFilePath -> Just mainFilePath
    _ -> Nothing

extractBenchmarkMainModulePath :: Cabal.Benchmark -> Maybe FilePath
extractBenchmarkMainModulePath benchmark =
  case Cabal.benchmarkInterface benchmark of
    Cabal.BenchmarkExeV10 _ mainFilePath -> Just mainFilePath
    _ -> Nothing

normalizeSourceDirs :: Set.Set FilePath -> Set.Set FilePath
normalizeSourceDirs sourceDirectorySet
  | Set.null sourceDirectorySet = Set.singleton "."
  | otherwise = sourceDirectorySet

renderCabalLanguage :: CabalExtension.Language -> String
renderCabalLanguage language =
  case language of
    CabalExtension.UnknownLanguage customLanguage -> customLanguage
    _ -> show language

renderCabalExtension :: CabalExtension.Extension -> String
renderCabalExtension extension =
  case extension of
    CabalExtension.EnableExtension knownExtension -> show knownExtension
    CabalExtension.DisableExtension knownExtension -> "No" <> show knownExtension
    CabalExtension.UnknownExtension customExtension -> customExtension

renderCabalModuleName :: CabalModuleName.ModuleName -> String
renderCabalModuleName = intercalate "." . CabalModuleName.components

cabalModuleNameToGhcModuleName :: CabalModuleName.ModuleName -> GHC.ModuleName
cabalModuleNameToGhcModuleName = GHC.mkModuleName . renderCabalModuleName

firstRight :: String -> Either [a] b -> Either String b
firstRight errorPrefix =
  either
    (\_ -> Left (errorPrefix <> ": dependency resolution failed during Cabal package finalization."))
    Right

formatWarnings :: (Show a) => [a] -> String
formatWarnings warnings =
  case warnings of
    [] -> ""
    _ -> " (parse warnings: " <> show warnings <> ")"

shouldDropModuleName :: String -> Bool
shouldDropModuleName modName =
  isPrefixOf "Paths_" modName || modName == "Main"
