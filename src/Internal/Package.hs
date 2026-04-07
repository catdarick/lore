{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Move filter" #-}
module Internal.Package where

import Control.Monad (forM, unless)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.RWS (asks)
import Data.Either (partitionEithers)
import Data.List (intercalate, isPrefixOf)
import qualified Data.Map as Map
import Data.Maybe (maybeToList)
import qualified Data.Set as Set
import qualified GHC
import GHC.DynFlags (Extension (..), GhcOption (..))
import qualified Hpack.Config as Hpack
import qualified Internal.Logger as Log
import Monad (MonadLore)
import Session (SessionContext (..))
import System.FilePath (takeDirectory, (</>))

data ComponentData = ComponentData
  { mainModulePath :: Maybe FilePath,
    ghcOptions :: Set.Set GhcOption,
    defaultExtensions :: Set.Set Extension,
    dependencies :: Set.Set String,
    sourceDirs :: Set.Set FilePath,
    modules :: Set.Set GHC.ModuleName
  }
  deriving (Show)

data PackageData = PackageData
  { packageRoot :: FilePath,
    packageName :: String,
    components :: [ComponentData]
  }

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
                { packageRoot = takeDirectory packageFile,
                  packageName = extractPackageName pkg,
                  components = extractPackageComponents pkg
                }

    extractPackageComponents :: Hpack.Package -> [ComponentData]
    extractPackageComponents pkg =
      let libs = maybeToList (Hpack.packageLibrary pkg) <> Map.elems (Hpack.packageInternalLibraries pkg)
          exes = Map.elems (Hpack.packageExecutables pkg) <> Map.elems (Hpack.packageTests pkg) <> Map.elems (Hpack.packageBenchmarks pkg)
       in map (extractComponentData extractLibraryModules (const Nothing)) libs
            <> map (extractComponentData extractExecutableModules Hpack.executableMain) exes

    extractPackageName :: Hpack.Package -> String
    extractPackageName = Hpack.packageName

    extractComponentData :: (a -> Set.Set GHC.ModuleName) -> (a -> Maybe FilePath) -> Hpack.Section a -> ComponentData
    extractComponentData moduleExtractor extractMainModule section =
      ComponentData
        { mainModulePath = extractMainModule (Hpack.sectionData section),
          ghcOptions = Set.fromList $ map GhcOption $ Hpack.sectionGhcOptions section,
          defaultExtensions = Set.fromList $ map Extension $ Hpack.sectionDefaultExtensions section,
          dependencies = getSectionDependencies section,
          sourceDirs = Set.fromList $ Hpack.sectionSourceDirs section,
          modules = moduleExtractor (Hpack.sectionData section)
        }

    getSectionDependencies :: Hpack.Section a -> Set.Set String
    getSectionDependencies section = Set.fromList $ Map.keys $ Hpack.unDependencies $ Hpack.sectionDependencies section

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

extractGhcOptions :: [ComponentData] -> Set.Set GhcOption
extractGhcOptions components = do
  let allOptions = Set.unions (map (.ghcOptions) components)
   in Set.filter (not . shouldDrop) allOptions
  where
    shouldDrop (GhcOption opt) =
      opt `elem` flagsToDrop
        || any (`isPrefixOf` opt) prefixesToDrop
    flagsToDrop =
      [ "-odir",
        "-hidir",
        "-stubdir",
        "-outputdir",
        "-main-is",
        "-this-unit-id",
        "-this-package-name",
        "-working-dir",
        "-debug",
        "-threaded",
        "-ticky",
        "-static",
        "-rtsopts",
        "-with-rtsopts"
      ]
    prefixesToDrop =
      [ "-odir=",
        "-hidir=",
        "-stubdir=",
        "-outputdir=",
        "-main-is=",
        "-this-unit-id=",
        "-this-package-name=",
        "-working-dir=",
        "-with-rtsopts=",
        "-O"
      ]

extractDependencies :: [ComponentData] -> Set.Set String
extractDependencies components =
  Set.unions (map dependencies components)

extractSourceDirs :: PackageData -> Set.Set FilePath
extractSourceDirs packageData = do
  Set.map (packageData.packageRoot </>) rawSourceDirs
  where
    rawSourceDirs = Set.unions $ map sourceDirs packageData.components
