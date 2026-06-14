{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lore.Internal.Ghc.DynFlags
  ( ParallelWorkersCount (..),
    Language (..),
    GhcOption (..),
    Extension (..),
    modifySessionDynFlagsM,
    setGhciLikeDynFlags,
    setGhcWorkDirs,
    setGhcOptionsAndExtensions,
    addGhcOptionsAndExtensions,
    setGhcSourceDirs,
    invalidatePackageDbCacheM,
    setPackageEnvironmentM,
  )
where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.Set as Set
import qualified GHC
import qualified GHC.Data.EnumSet as EnumSet
import qualified GHC.Driver.Env.Types as GHC
import qualified GHC.Driver.Monad as GHC
import qualified GHC.Driver.Session as GHC
import qualified GHC.Unit.Env as GHC
import qualified GHC.Utils.Logger as GHC
import qualified GHC.Utils.TmpFs as GHC
import Lore.Internal.Ghc.PackageEnvironment.Types
  ( PackageDbFlagTarget (PackageDbFlagsForGhc),
    ResolvedPackageEnvironment (..),
    UnitIdText (..),
    renderPackageDbStackFlags,
  )
import System.FilePath (normalise, (</>))

data ParallelWorkersCount
  = ThisWorkersCount Int
  | WorkersAsNumProcessors
  deriving stock (Eq, Show)

modifySessionDynFlagsM :: (GHC.GhcMonad m) => (GHC.DynFlags -> m GHC.DynFlags) -> m ()
modifySessionDynFlagsM f = do
  dflags <- GHC.getSessionDynFlags
  dflags' <- f dflags
  GHC.setSessionDynFlags dflags'

invalidatePackageDbCacheM :: (GHC.GhcMonad m) => m ()
invalidatePackageDbCacheM =
  GHC.modifySession \hscEnv ->
    hscEnv
      { GHC.hsc_unit_env =
          GHC.ue_setUnitDbs
            Nothing
            (GHC.hsc_unit_env hscEnv)
      }

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
        `GHC.gopt_set` GHC.Opt_ExternalInterpreter
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

newtype Language = Language
  { unLanguage :: String
  }
  deriving newtype (Eq, Ord, Show)

newtype Extension = Extension
  { unGhcExtension :: String
  }
  deriving newtype (Eq, Ord, Show)

setGhcOptionsAndExtensions :: (MonadIO m, GHC.HasLogger m) => Maybe Language -> [GhcOption] -> [Extension] -> GHC.DynFlags -> m GHC.DynFlags
setGhcOptionsAndExtensions language ghcOptions extensions dflags = do
  addGhcOptionsAndExtensions language ghcOptions extensions (resetExtensions dflags)

addGhcOptionsAndExtensions :: (MonadIO m, GHC.HasLogger m) => Maybe Language -> [GhcOption] -> [Extension] -> GHC.DynFlags -> m GHC.DynFlags
addGhcOptionsAndExtensions language ghcOptions extensions dflags = do
  logger <- GHC.getLogger
  (dflags', _, _) <- GHC.parseDynamicFlags logger dflags (map GHC.noLoc (languageToOpts <> ghcOptionsToOpts <> extensionsToOpts))
  pure dflags'
  where
    languageToOpts = maybe [] (\lang -> ["-X" <> unLanguage lang]) language
    ghcOptionsToOpts = map unGhcOption ghcOptions
    extensionsToOpts = map (("-X" <>) . unGhcExtension) extensions

setGhcSourceDirs :: [FilePath] -> GHC.DynFlags -> GHC.DynFlags
setGhcSourceDirs sourceDirs dflags =
  dflags
    { GHC.importPaths = map normalise sourceDirs
    }

setPackageEnvironmentM :: (MonadIO m, GHC.HasLogger m) => ResolvedPackageEnvironment -> GHC.DynFlags -> m GHC.DynFlags
setPackageEnvironmentM environment dflags = do
  let renderedFlags = renderPackageEnvironmentFlags environment
      resetPackageFlags = resetPackageEnvironmentFlags dflags
  if null renderedFlags
    then pure resetPackageFlags
    else do
      logger <- GHC.getLogger
      (dflags', _, _) <-
        GHC.parseDynamicFlags logger resetPackageFlags (map GHC.noLoc renderedFlags)
      pure dflags'

resetExtensions :: GHC.DynFlags -> GHC.DynFlags
resetExtensions dflags =
  dflags
    { GHC.extensions = [],
      GHC.extensionFlags = EnumSet.fromList (GHC.languageExtensions (GHC.language dflags))
    }

resetPackageEnvironmentFlags :: GHC.DynFlags -> GHC.DynFlags
resetPackageEnvironmentFlags dflags =
  GHC.gopt_unset
    ( dflags
        { GHC.packageFlags = [],
          GHC.packageDBFlags = []
        }
    )
    GHC.Opt_HideAllPackages

renderPackageEnvironmentFlags :: ResolvedPackageEnvironment -> [String]
renderPackageEnvironmentFlags environment =
  ["-clear-package-db"]
    <> renderPackageDbStackFlags PackageDbFlagsForGhc environment.resolvedPackageDbStack
    <> hideAllPackagesFlag
    <> concatMap renderUnitId (Set.toAscList environment.resolvedExposedUnitIds)
  where
    hideAllPackagesFlag =
      if Set.null environment.resolvedExposedUnitIds
        then []
        else ["-hide-all-packages"]

    renderUnitId unitId =
      ["-package-id", unitId.unUnitIdText]
