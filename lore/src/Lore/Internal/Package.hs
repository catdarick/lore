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

import Control.DeepSeq (NFData (..))
import Control.Exception (IOException, try)
import Control.Monad (forM, unless)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.RWS (asks)
import qualified Data.ByteString as BS
import Data.Either (partitionEithers)
import Data.List (intercalate, isPrefixOf, nub, sort, stripPrefix)
import qualified Data.Map as Map
import Data.Maybe (maybeToList)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
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
import qualified Hpack.Config as Hpack
import qualified Language.Haskell.Extension as CabalExtension
import Lore.Internal.File (findFilesByExtensionRecursively)
import Lore.Internal.Ghc.DynFlags (Extension (..), GhcOption (..), Language (..))
import Lore.Internal.ProjectProvider (ProjectProvider (..))
import Lore.Internal.Session (SessionContext (..))
import qualified Lore.Logger as Log
import Lore.Monad (MonadLore)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (dropTrailingPathSeparator, makeRelative, normalise, splitDirectories, takeDirectory, takeExtension, takeFileName, (</>))
import UnliftIO.Exception (throwString)

data ComponentData = ComponentData
  { componentKind :: ComponentKind,
    componentName :: String,
    mainModulePath :: Maybe FilePath,
    language :: Maybe Language,
    ghcOptions :: Set.Set GhcOption,
    defaultExtensions :: Set.Set Extension,
    dependencies :: Set.Set String,
    sourceDirs :: Set.Set FilePath,
    modules :: Set.Set GHC.ModuleName
  }
  deriving (Show)

data ComponentKind
  = ComponentKindLibrary
  | ComponentKindInternalLibrary
  | ComponentKindExecutable
  | ComponentKindTest
  | ComponentKindBenchmark
  deriving (Eq, Ord, Show)

instance NFData ComponentKind where
  rnf componentKind =
    componentKind `seq` ()

data PackageData = PackageData
  { packageManifestPath :: FilePath,
    packageRoot :: FilePath,
    packageName :: String,
    components :: [ComponentData]
  }
  deriving (Show)

data StackConfig = StackConfig
  { packages :: [StackPackageEntry]
  }

data StackPackageEntry = StackPackageEntry
  { packagePath :: FilePath,
    extraDep :: Bool
  }

instance Yaml.FromJSON StackConfig where
  parseJSON = Yaml.withObject "StackConfig" \obj ->
    StackConfig
      <$> obj Yaml..:? "packages" Yaml..!= []

instance Yaml.FromJSON StackPackageEntry where
  parseJSON = \case
    Yaml.String pathText ->
      pure
        StackPackageEntry
          { packagePath = T.unpack pathText,
            extraDep = False
          }
    value ->
      Yaml.withObject "StackPackageEntry" parsePackageObject value
    where
      parsePackageObject obj = do
        locationText <- obj Yaml..: "location"
        isExtraDep <- obj Yaml..:? "extra-dep" Yaml..!= False
        pure
          StackPackageEntry
            { packagePath = T.unpack locationText,
              extraDep = isExtraDep
            }

prepareComponentsData :: (MonadLore m) => m [PackageData]
prepareComponentsData = do
  provider <- asks projectProvider
  sessionProjectRoot <- asks projectRoot
  let manifestKindLabel =
        case provider of
          StackProject -> "package.yaml"
          CabalProject -> ".cabal"
  Log.debug $ "Loading " <> manifestKindLabel <> " manifests and extracting units data..."
  eiManifestPaths <- liftIO (discoverManifestPaths provider sessionProjectRoot)
  case eiManifestPaths of
    Left err -> do
      throwString err
    Right manifestPaths -> do
      eiPackages <- forM manifestPaths (processManifest provider)
      let (errors, packages) = partitionEithers eiPackages
      unless (null errors) $
        throwString (intercalate "\n  -" ("Errors encountered while loading package manifests:" : errors))
      let componentsCount = length (concatMap (.components) packages)
      Log.debug $ "Successfully loaded " <> show (length packages) <> " package manifests, resulting in " <> show componentsCount <> " components"
      pure packages
  where
    processManifest provider manifestPath =
      case provider of
        StackProject ->
          processStackManifest manifestPath
        CabalProject ->
          processCabalPackage manifestPath

discoverProject :: (MonadLore m) => m [PackageData]
discoverProject =
  prepareComponentsData

discoverManifestPaths :: ProjectProvider -> FilePath -> IO (Either String [FilePath])
discoverManifestPaths provider projectRoot =
  case provider of
    StackProject ->
      discoverStackManifestPaths projectRoot
    CabalProject ->
      discoverCabalManifestPaths projectRoot

discoverStackManifestPaths :: FilePath -> IO (Either String [FilePath])
discoverStackManifestPaths projectRoot = do
  stackYamlContent <- BS.readFile (projectRoot </> "stack.yaml")
  case Yaml.decodeEither' stackYamlContent of
    Left parseError ->
      pure (Left ("Failed to parse stack.yaml: " <> show parseError))
    Right StackConfig {packages = packageEntries} -> do
      let configuredPackageEntries = map packagePath (filter (not . extraDep) packageEntries)
          localPackageEntries =
            if null configuredPackageEntries
              then ["."]
              else configuredPackageEntries
      eiManifestPaths <- mapM (resolveStackManifestPathsFromEntry projectRoot) localPackageEntries
      pure $ do
        manifestPaths <- sequence eiManifestPaths
        pure (sort (nub (concat manifestPaths)))

resolveStackManifestPathsFromEntry :: FilePath -> FilePath -> IO (Either String [FilePath])
resolveStackManifestPathsFromEntry projectRoot packageEntry
  | containsWildcard packageEntry =
      pure
        ( Left
            ( "Unsupported wildcard package entry in stack.yaml: "
                <> packageEntry
                <> ". Please list package directories explicitly in the packages: section."
            )
        )
  | otherwise = do
      let resolvedEntryPath = normalizeRelativePath (projectRoot </> packageEntry)
      isManifestFile <- doesFileExist resolvedEntryPath
      if isManifestFile
        then
          if isSupportedStackManifestPath resolvedEntryPath
            then pure (Right [resolvedEntryPath])
            else pure (Left ("stack.yaml package entry does not point to package.yaml or .cabal manifest: " <> packageEntry))
        else do
          isEntryDirectory <- doesDirectoryExist resolvedEntryPath
          if isEntryDirectory
            then resolveStackDirectoryManifestPath packageEntry resolvedEntryPath
            else pure (Left ("stack.yaml package entry does not exist: " <> packageEntry))

isSupportedStackManifestPath :: FilePath -> Bool
isSupportedStackManifestPath path =
  takeFileName path == "package.yaml" || takeExtension path == ".cabal"

resolveStackDirectoryManifestPath :: FilePath -> FilePath -> IO (Either String [FilePath])
resolveStackDirectoryManifestPath packageEntry resolvedEntryPath = do
  let packageYamlPath = resolvedEntryPath </> "package.yaml"
  packageYamlExists <- doesFileExist packageYamlPath
  entries <- listDirectory resolvedEntryPath
  let topLevelCabalFiles =
        sort
          [ resolvedEntryPath </> entry
          | entry <- entries,
            takeExtension entry == ".cabal"
          ]
  if packageYamlExists
    then pure (Right [packageYamlPath])
    else case topLevelCabalFiles of
      [singleCabalFile] -> pure (Right [singleCabalFile])
      [] -> pure (Left ("No package.yaml or .cabal file found in stack package directory: " <> packageEntry))
      _ ->
        pure
          ( Left
              ( "Multiple .cabal files found in stack package directory: "
                  <> packageEntry
                  <> ". Use an explicit package manifest path in stack.yaml."
              )
          )

discoverCabalManifestPaths :: FilePath -> IO (Either String [FilePath])
discoverCabalManifestPaths projectRoot = do
  cabalProjectExists <- doesFileExist (projectRoot </> "cabal.project")
  if cabalProjectExists
    then resolveCabalManifestPathsFromProjectFile projectRoot
    else discoverRootCabalManifestPaths projectRoot

resolveCabalManifestPathsFromProjectFile :: FilePath -> IO (Either String [FilePath])
resolveCabalManifestPathsFromProjectFile projectRoot = do
  cabalProjectText <- readFile (projectRoot </> "cabal.project")
  let packageEntries = extractCabalProjectPackageEntries cabalProjectText
  if null packageEntries
    then pure (Left "Detected Cabal project, but 'cabal.project' does not define any package entries in the packages: section.")
    else do
      eiManifestPaths <- mapM (resolveCabalManifestPathsFromEntry projectRoot) packageEntries
      pure $ do
        manifestPaths <- sequence eiManifestPaths
        pure (sort (nub (concat manifestPaths)))

discoverRootCabalManifestPaths :: FilePath -> IO (Either String [FilePath])
discoverRootCabalManifestPaths projectRoot = do
  rootEntries <- listDirectory projectRoot
  let rootManifests =
        sort
          [ projectRoot </> entry
          | entry <- rootEntries,
            takeExtension entry == ".cabal"
          ]
  case rootManifests of
    [] ->
      pure
        ( Left
            "Detected Cabal project, but no *.cabal file was found at the project root. Add a root package file or a cabal.project packages: stanza."
        )
    _ -> pure (Right rootManifests)

resolveCabalManifestPathsFromEntry :: FilePath -> FilePath -> IO (Either String [FilePath])
resolveCabalManifestPathsFromEntry projectRoot packageEntry
  | hasUnsupportedWildcard packageEntry =
      pure
        ( Left
            ( "Unsupported wildcard package entry in cabal.project: "
                <> packageEntry
                <> ". Supported wildcard tokens are '*' and '?'."
            )
        )
  | containsWildcard packageEntry =
      resolveCabalManifestPathsFromWildcardEntry projectRoot packageEntry
  | otherwise = do
      let resolvedEntryPath = normalizeRelativePath (projectRoot </> packageEntry)
      isManifestFile <- doesFileExist resolvedEntryPath
      if isManifestFile
        then
          if takeExtension resolvedEntryPath == ".cabal"
            then pure (Right [resolvedEntryPath])
            else pure (Left ("cabal.project package entry does not point to a .cabal file: " <> packageEntry))
        else do
          isEntryDirectory <- doesDirectoryExist resolvedEntryPath
          if isEntryDirectory
            then do
              cabalFiles <- sort <$> findFilesByExtensionRecursively Nothing resolvedEntryPath ".cabal"
              let topLevelCabalFiles =
                    filter ((== resolvedEntryPath) . takeDirectory) cabalFiles
              case topLevelCabalFiles of
                [singleManifest] -> pure (Right [singleManifest])
                [] -> pure (Left ("No .cabal file found in package directory: " <> packageEntry))
                _ -> pure (Left ("Multiple .cabal files found in package directory: " <> packageEntry <> ". Use explicit manifest file paths in cabal.project."))
            else pure (Left ("cabal.project package entry does not exist: " <> packageEntry))

containsWildcard :: FilePath -> Bool
containsWildcard path =
  any (`elem` ['*', '?', '[', ']']) path

hasUnsupportedWildcard :: FilePath -> Bool
hasUnsupportedWildcard path =
  any (`elem` ['[', ']']) path

resolveCabalManifestPathsFromWildcardEntry :: FilePath -> FilePath -> IO (Either String [FilePath])
resolveCabalManifestPathsFromWildcardEntry projectRoot packageEntry = do
  candidateCabalFiles <- sort <$> findFilesByExtensionRecursively Nothing projectRoot ".cabal"
  let normalizedPatternSegments = splitDirectories (normalizeRelativePath packageEntry)
      matches =
        [ path
        | path <- candidateCabalFiles,
          let relativePath = normalizeRelativePath (makeRelative projectRoot path),
          wildcardPathMatches normalizedPatternSegments (splitDirectories relativePath)
        ]
  if null matches
    then pure (Left ("No package manifests matched cabal.project wildcard entry: " <> packageEntry))
    else pure (Right matches)

wildcardPathMatches :: [FilePath] -> [FilePath] -> Bool
wildcardPathMatches patternSegments candidateSegments =
  length patternSegments == length candidateSegments
    && and (zipWith wildcardSegmentMatches patternSegments candidateSegments)

wildcardSegmentMatches :: String -> String -> Bool
wildcardSegmentMatches patternText candidateText =
  go patternText candidateText
  where
    go [] [] = True
    go [] _ = False
    go ('*' : patternTail) text =
      go patternTail text
        || case text of
          [] -> False
          (_ : textTail) -> go ('*' : patternTail) textTail
    go ('?' : patternTail) (_ : textTail) =
      go patternTail textTail
    go ('?' : _) [] = False
    go (patternChar : patternTail) (textChar : textTail)
      | patternChar == textChar = go patternTail textTail
      | otherwise = False
    go _ _ = False

extractCabalProjectPackageEntries :: String -> [FilePath]
extractCabalProjectPackageEntries = go False . lines
  where
    go _ [] = []
    go inPackages (line : rest)
      | startsPackagesSection strippedLine =
          inlineEntries <> go True rest
      | inPackages && isIndentedLine line =
          parsePackageEntryList strippedLine <> go True rest
      | inPackages =
          go False (line : rest)
      | otherwise =
          go False rest
      where
        strippedLine = stripLineComments (trim line)
        inlineEntries =
          case stripPrefix "packages:" strippedLine of
            Just remainder -> parsePackageEntryList remainder
            Nothing -> []

    startsPackagesSection line =
      "packages:" `isPrefixOf` line

    isIndentedLine = \case
      [] -> False
      firstChar : _ -> firstChar == ' ' || firstChar == '\t'

    stripLineComments text =
      takeWhileBeforeComment text

    takeWhileBeforeComment [] = []
    takeWhileBeforeComment ('-' : '-' : _) = []
    takeWhileBeforeComment (char : chars) = char : takeWhileBeforeComment chars

    parsePackageEntryList = filter (not . null) . concatMap splitComma . words . trim

    splitComma text =
      map trim (splitByComma text)

    splitByComma text =
      case break (== ',') text of
        (entry, []) -> [entry]
        (entry, _ : rest) -> entry : splitByComma rest

    trim = reverse . dropWhile isSpaceChar . reverse . dropWhile isSpaceChar

    isSpaceChar ch = ch `elem` [' ', '\t']

processStackPackage :: (MonadIO m) => FilePath -> m (Either String PackageData)
processStackPackage packageFile = do
  r <-
    liftIO $
      Hpack.readPackageConfig
        Hpack.defaultDecodeOptions
          { Hpack.decodeOptionsTarget = packageFile
          }
  case r of
    Left err -> pure $ Left $ "Failed to read " <> packageFile <> ": " <> err
    Right config -> do
      let pkg = Hpack.decodeResultPackage config
      pure $
        Right
          PackageData
            { packageManifestPath = packageFile,
              packageRoot = takeDirectory packageFile,
              packageName = extractPackageNameFromHpack pkg,
              components = extractHpackComponents pkg
            }

processStackManifest :: (MonadIO m) => FilePath -> m (Either String PackageData)
processStackManifest manifestPath
  | takeFileName manifestPath == "package.yaml" =
      processStackPackage manifestPath
  | takeExtension manifestPath == ".cabal" =
      processCabalPackage manifestPath
  | otherwise =
      pure
        ( Left
            ( "Unsupported stack package manifest: "
                <> manifestPath
                <> ". Expected package.yaml or .cabal."
            )
        )

extractHpackComponents :: Hpack.Package -> [ComponentData]
extractHpackComponents pkg =
  let libs =
        maybeToList ((ComponentKindLibrary,"library",) <$> Hpack.packageLibrary pkg)
          <> map (\(name, section) -> (ComponentKindInternalLibrary, "library:" <> name, section)) (Map.toList (Hpack.packageInternalLibraries pkg))
      exes = map (\(name, section) -> (ComponentKindExecutable, "executable:" <> name, section)) (Map.toList (Hpack.packageExecutables pkg))
      tests = map (\(name, section) -> (ComponentKindTest, "test:" <> name, section)) (Map.toList (Hpack.packageTests pkg))
      benches = map (\(name, section) -> (ComponentKindBenchmark, "benchmark:" <> name, section)) (Map.toList (Hpack.packageBenchmarks pkg))
   in map (\(kind, name, section) -> extractHpackComponentData kind extractHpackLibraryModules (const Nothing) name section) libs
        <> map (\(kind, name, section) -> extractHpackComponentData kind extractHpackExecutableModules Hpack.executableMain name section) (exes <> tests <> benches)

extractPackageNameFromHpack :: Hpack.Package -> String
extractPackageNameFromHpack = Hpack.packageName

extractHpackComponentData :: ComponentKind -> (a -> Set.Set GHC.ModuleName) -> (a -> Maybe FilePath) -> String -> Hpack.Section a -> ComponentData
extractHpackComponentData kind moduleExtractor extractMainModule name section =
  ComponentData
    { componentKind = kind,
      componentName = name,
      mainModulePath = extractMainModule (Hpack.sectionData section),
      language = (\(Hpack.Language lang) -> Language lang) <$> Hpack.sectionLanguage section,
      ghcOptions = Set.fromList $ map GhcOption $ Hpack.sectionGhcOptions section,
      defaultExtensions = Set.fromList $ map Extension $ Hpack.sectionDefaultExtensions section,
      dependencies = extractHpackSectionDependencies section,
      sourceDirs = Set.fromList $ Hpack.sectionSourceDirs section,
      modules = moduleExtractor (Hpack.sectionData section)
    }

extractHpackSectionDependencies :: Hpack.Section a -> Set.Set String
extractHpackSectionDependencies section = Set.fromList $ Map.keys $ Hpack.unDependencies $ Hpack.sectionDependencies section

extractHpackLibraryModules :: Hpack.Library -> Set.Set GHC.ModuleName
extractHpackLibraryModules lib =
  Set.fromList $
    map hpackModuleToGhcModuleName $
      filter (not . shouldDropHpackModule) $
        concat
          [ Hpack.libraryExposedModules lib,
            Hpack.libraryOtherModules lib,
            Hpack.libraryGeneratedModules lib
          ]

extractHpackExecutableModules :: Hpack.Executable -> Set.Set GHC.ModuleName
extractHpackExecutableModules exe =
  Set.fromList $
    map hpackModuleToGhcModuleName $
      filter (not . shouldDropHpackModule) $
        concat
          [ Hpack.executableOtherModules exe,
            Hpack.executableGeneratedModules exe
          ]

hpackModuleToGhcModuleName :: Hpack.Module -> GHC.ModuleName
hpackModuleToGhcModuleName (Hpack.Module modName) = GHC.mkModuleName modName

shouldDropHpackModule :: Hpack.Module -> Bool
shouldDropHpackModule (Hpack.Module modName) =
  shouldDropModuleName modName

processCabalPackage :: (MonadIO m) => FilePath -> m (Either String PackageData)
processCabalPackage cabalFile = do
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
            (CabalCompiler.CompilerId CabalCompiler.GHC CabalVersion.nullVersion)
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

componentMainModulePathCandidates :: FilePath -> ComponentData -> [FilePath]
componentMainModulePathCandidates packageRoot component =
  case component.mainModulePath of
    Nothing -> []
    Just mainPath ->
      nub (preferredMainPath : fallbackCandidates)
      where
        preferredMainPath = packageRoot </> normalizedMainPathFromRoot
        sourceDirs = Set.toAscList component.sourceDirs
        normalizedMainPath = normalizeRelativePath mainPath
        normalizedMainPathFromRoot =
          if any (`isAncestorPath` normalizedMainPath) sourceDirs
            then normalizedMainPath
            else case sourceDirs of
              [singleSourceDir] -> normalizeRelativePath (singleSourceDir </> normalizedMainPath)
              _ -> normalizedMainPath
        fallbackCandidates =
          map resolveThroughSourceDir sourceDirs
            <> [packageRoot </> normalizedMainPath]

        resolveThroughSourceDir sourceDir
          | sourceDir `isAncestorPath` normalizedMainPath =
              packageRoot </> normalizedMainPath
          | otherwise =
              packageRoot </> normalizeRelativePath (sourceDir </> normalizedMainPath)

normalizeRelativePath :: FilePath -> FilePath
normalizeRelativePath path =
  case dropTrailingPathSeparator (normalise path) of
    "" -> "."
    normalized -> normalized

isAncestorPath :: FilePath -> FilePath -> Bool
isAncestorPath ancestor path =
  splitDirectories (normalizeRelativePath ancestor)
    `isPrefixOf` splitDirectories (normalizeRelativePath path)

firstExistingPath :: (MonadLore m) => [FilePath] -> m (Maybe FilePath)
firstExistingPath [] = pure Nothing
firstExistingPath (path : rest) = do
  exists <- liftIO (doesFileExist path)
  if exists
    then pure (Just path)
    else firstExistingPath rest

commonSetIntersection :: (Ord a) => [Set.Set a] -> Set.Set a
commonSetIntersection [] = Set.empty
commonSetIntersection sets = foldr1 Set.intersection sets

extractDependencies :: [ComponentData] -> Set.Set String
extractDependencies components =
  Set.unions (map dependencies components)

extractSourceDirs :: PackageData -> Set.Set FilePath
extractSourceDirs packageData = do
  Set.map (packageData.packageRoot </>) rawSourceDirs
  where
    rawSourceDirs = Set.unions $ map sourceDirs packageData.components
