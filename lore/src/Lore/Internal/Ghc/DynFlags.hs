{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lore.Internal.Ghc.DynFlags
  ( ParallelWorkersCount (..),
    GhcOption (..),
    Extension (..),
    modifySessionDynFlags,
    modifySessionDynFlagsM,
    setGhciLikeDynFlags,
    setGhcWorkDirs,
    setGhcOptionsAndExtensions,
    setGhcSourceDirs,
    setDependencies,
    setPackageDbs,
  )
where

import Control.Monad.IO.Class (MonadIO)
import qualified GHC
import qualified GHC.Data.EnumSet as EnumSet
import qualified GHC.Driver.Session as GHC
import qualified GHC.Utils.Logger as GHC
import qualified GHC.Utils.TmpFs as GHC
import System.FilePath (normalise, (</>))

data ParallelWorkersCount
  = ThisWorkersCount Int
  | WorkersAsNumProcessors

modifySessionDynFlags :: (GHC.GhcMonad m) => (GHC.DynFlags -> GHC.DynFlags) -> m ()
modifySessionDynFlags f = do
  modifySessionDynFlagsM (pure . f)

modifySessionDynFlagsM :: (GHC.GhcMonad m) => (GHC.DynFlags -> m GHC.DynFlags) -> m ()
modifySessionDynFlagsM f = do
  dflags <- GHC.getSessionDynFlags
  dflags' <- f dflags
  GHC.setSessionDynFlags dflags'

setGhciLikeDynFlags :: ParallelWorkersCount -> GHC.DynFlags -> GHC.DynFlags
setGhciLikeDynFlags parallelWorkersLimit dflags0 =
  let dflags1 =
        dflags0
          { GHC.ghcMode = GHC.CompManager,
            GHC.backend = GHC.interpreterBackend,
            GHC.ghcLink = GHC.LinkInMemory,
            GHC.verbosity = 1
          }
      dflags2 =
        case parallelWorkersLimit of
          ThisWorkersCount jobs -> dflags1 {GHC.parMakeCount = Just jobs}
          WorkersAsNumProcessors -> dflags1 {GHC.parMakeCount = Nothing}
   in dflags2
        `GHC.gopt_set` GHC.Opt_UseBytecodeRatherThanObjects
        `GHC.gopt_set` GHC.Opt_Haddock
        `GHC.gopt_set` GHC.Opt_IgnoreHpcChanges
        `GHC.gopt_set` GHC.Opt_IgnoreOptimChanges
        `GHC.gopt_set` GHC.Opt_ImplicitImportQualified

setGhcWorkDirs :: FilePath -> GHC.DynFlags -> GHC.DynFlags
setGhcWorkDirs ghcWorkDir dflags =
  dflags
    { GHC.objectDir = Just (ghcWorkDir </> "obj"),
      GHC.hiDir = Just (ghcWorkDir </> "hi"),
      GHC.hieDir = Just (ghcWorkDir </> "hie"),
      GHC.stubDir = Just (ghcWorkDir </> "stub"),
      GHC.tmpDir = GHC.TempDir (ghcWorkDir </> "tmp")
    }

newtype GhcOption = GhcOption
  { unGhcOption :: String
  }
  deriving newtype (Eq, Ord, Show)

newtype Extension = Extension
  { unGhcExtension :: String
  }
  deriving newtype (Eq, Ord, Show)

setGhcOptionsAndExtensions :: (MonadIO m, GHC.HasLogger m) => [GhcOption] -> [Extension] -> GHC.DynFlags -> m GHC.DynFlags
setGhcOptionsAndExtensions ghcOptions extensions dflags = do
  logger <- GHC.getLogger
  (dflags', _, _) <- GHC.parseDynamicFlags logger (resetExtensions dflags) (map GHC.noLoc (ghcOptionsToOpts <> extensionsToOpts))
  pure dflags'
  where
    ghcOptionsToOpts = map unGhcOption ghcOptions
    extensionsToOpts = map (("-X" <>) . unGhcExtension) extensions

setGhcSourceDirs :: [FilePath] -> GHC.DynFlags -> GHC.DynFlags
setGhcSourceDirs sourceDirs dflags =
  dflags
    { GHC.importPaths = map normalise sourceDirs
    }

setDependencies :: [String] -> GHC.DynFlags -> GHC.DynFlags
setDependencies dependencies dflags =
  dflags
    { GHC.packageFlags = map mkPackageFlag dependencies
    }
    `GHC.gopt_set` GHC.Opt_HideAllPackages
  where
    mkPackageFlag dep = GHC.ExposePackage ("-package " <> dep) (GHC.PackageArg dep) (GHC.ModRenaming True [])

setPackageDbs :: [FilePath] -> GHC.DynFlags -> GHC.DynFlags
setPackageDbs dbPaths dflags =
  dflags
    { GHC.packageDBFlags =
        reverse $
          concat
            [ [GHC.ClearPackageDBs], -- 1. First, usually we want to clear whatever default databases GHC assumed
              map (GHC.PackageDB . GHC.PkgDbPath) dbPaths, -- 2. Then, add the explicit database paths that Stack/Cabal provided
              GHC.packageDBFlags dflags -- 3. GHC applies these in reverse order
            ]
    }

resetExtensions :: GHC.DynFlags -> GHC.DynFlags
resetExtensions dflags =
  dflags
    { GHC.extensions = [],
      GHC.extensionFlags = EnumSet.fromList (GHC.languageExtensions (GHC.language dflags))
    }
