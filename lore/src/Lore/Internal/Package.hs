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

import Control.Monad (forM, unless)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.RWS (asks)
import Data.Either (partitionEithers)
import Data.List (intercalate, isPrefixOf, nub)
import qualified Data.Map as Map
import Data.Maybe (maybeToList)
import qualified Data.Set as Set
import qualified GHC
import qualified Hpack.Config as Hpack
import Lore.Internal.Ghc.DynFlags (Extension (..), GhcOption (..), Language (..))
import Lore.Internal.Session (SessionContext (..))
import qualified Lore.Logger as Log
import Lore.Monad (MonadLore)
import System.Directory (doesFileExist)
import System.FilePath (dropTrailingPathSeparator, normalise, splitDirectories, takeDirectory, (</>))

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

data PackageData = PackageData
  { packageYamlPath :: FilePath,
    packageRoot :: FilePath,
    packageName :: String,
    components :: [ComponentData]
  }
  deriving (Show)

prepareComponentsData :: (MonadLore m) => m [PackageData]
prepareComponentsData = do
  Log.debug "Loading package.yaml files and extracting units data..."
  packageFiles <- asks packageFiles
  eiPackages <- forM packageFiles processPackage
  let (errors, packages) = partitionEithers eiPackages
  unless (null errors) $ do
    Log.err $ intercalate "\n  -" ("Errors encountered while reading package.yaml files:" : errors)
  let components = concatMap (.components) packages
  Log.debug $ "Successfully loaded " <> show (length packages) <> " package.yaml files, resulting in " <> show (length components) <> " components"
  pure packages
  where
    processPackage packageFile = do
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
                { packageYamlPath = packageFile,
                  packageRoot = takeDirectory packageFile,
                  packageName = extractPackageName pkg,
                  components = extractPackageComponents pkg
                }

    extractPackageComponents :: Hpack.Package -> [ComponentData]
    extractPackageComponents pkg =
      let libs =
            maybeToList ((ComponentKindLibrary,"library",) <$> Hpack.packageLibrary pkg)
              <> map (\(name, section) -> (ComponentKindInternalLibrary, "library:" <> name, section)) (Map.toList (Hpack.packageInternalLibraries pkg))
          exes = map (\(name, section) -> (ComponentKindExecutable, "executable:" <> name, section)) (Map.toList (Hpack.packageExecutables pkg))
          tests = map (\(name, section) -> (ComponentKindTest, "test:" <> name, section)) (Map.toList (Hpack.packageTests pkg))
          benches = map (\(name, section) -> (ComponentKindBenchmark, "benchmark:" <> name, section)) (Map.toList (Hpack.packageBenchmarks pkg))
       in map (\(kind, name, section) -> extractComponentData kind extractLibraryModules (const Nothing) name section) libs
            <> map (\(kind, name, section) -> extractComponentData kind extractExecutableModules Hpack.executableMain name section) (exes <> tests <> benches)

    extractPackageName :: Hpack.Package -> String
    extractPackageName = Hpack.packageName

    extractComponentData :: ComponentKind -> (a -> Set.Set GHC.ModuleName) -> (a -> Maybe FilePath) -> String -> Hpack.Section a -> ComponentData
    extractComponentData kind moduleExtractor extractMainModule name section =
      ComponentData
        { componentKind = kind,
          componentName = name,
          mainModulePath = extractMainModule (Hpack.sectionData section),
          language = (\(Hpack.Language lang) -> Language lang) <$> Hpack.sectionLanguage section,
          ghcOptions = Set.fromList $ map GhcOption $ Hpack.sectionGhcOptions section,
          defaultExtensions = Set.fromList $ map Extension $ Hpack.sectionDefaultExtensions section,
          dependencies = extractSectionDependencies section,
          sourceDirs = Set.fromList $ Hpack.sectionSourceDirs section,
          modules = moduleExtractor (Hpack.sectionData section)
        }

    extractSectionDependencies :: Hpack.Section a -> Set.Set String
    extractSectionDependencies section = Set.fromList $ Map.keys $ Hpack.unDependencies $ Hpack.sectionDependencies section

    extractLibraryModules :: Hpack.Library -> Set.Set GHC.ModuleName
    extractLibraryModules lib =
      Set.fromList $
        map moduleToName $
          filter (not . shouldDropModule) $
            concat
              [ Hpack.libraryExposedModules lib,
                Hpack.libraryOtherModules lib,
                Hpack.libraryGeneratedModules lib
              ]

    extractExecutableModules :: Hpack.Executable -> Set.Set GHC.ModuleName
    extractExecutableModules exe =
      Set.fromList $
        map moduleToName $
          filter (not . shouldDropModule) $
            concat
              [ Hpack.executableOtherModules exe,
                Hpack.executableGeneratedModules exe
              ]

    moduleToName :: Hpack.Module -> GHC.ModuleName
    moduleToName (Hpack.Module modName) = GHC.mkModuleName modName

    shouldDropModule :: Hpack.Module -> Bool
    shouldDropModule (Hpack.Module modName) =
      isPrefixOf "Paths_" modName || modName == "Main"

discoverProject :: (MonadLore m) => m [PackageData]
discoverProject =
  prepareComponentsData

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
