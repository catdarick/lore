{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lore.Internal.Ghc.PackageEnvironment.Types
  ( UnitIdText (..),
    PackageNameText (..),
    PackageDb (..),
    PackageDbStack (..),
    PackageIndexEntry (..),
    PackageIndex (..),
    GhcToolchain (..),
    PackageEnvironmentSnapshot (..),
    CapturedGhcEnvironment (..),
    ResolvedPackageEnvironment (..),
    ParsedGhcEnvironmentFile (..),
    PackageResolutionError (..),
    PackageDbFlagTarget (..),
    renderPackageDbStackFlags,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Distribution.Version as CabalVersion

newtype UnitIdText = UnitIdText
  { unUnitIdText :: String
  }
  deriving newtype (Eq, Ord, Show)

newtype PackageNameText = PackageNameText
  { unPackageNameText :: String
  }
  deriving newtype (Eq, Ord, Show)

data PackageDb
  = GlobalPackageDb
  | UserPackageDb
  | SpecificPackageDb FilePath
  deriving (Eq, Ord, Show)

newtype PackageDbStack = PackageDbStack
  { unPackageDbStack :: [PackageDb]
  }
  deriving newtype (Eq, Show)

data PackageIndexEntry = PackageIndexEntry
  { packageIndexPackageName :: PackageNameText,
    packageIndexUnitId :: UnitIdText,
    packageIndexVersion :: String,
    packageIndexExposed :: Bool
  }
  deriving (Eq, Show)

data PackageIndex = PackageIndex
  { packageIndexByUnitId :: Map.Map UnitIdText PackageIndexEntry,
    packageIndexByPackageName :: Map.Map PackageNameText [PackageIndexEntry]
  }
  deriving (Eq, Show)

data GhcToolchain = GhcToolchain
  { ghcToolchainCompilerExe :: FilePath,
    ghcToolchainCompilerVersion :: CabalVersion.Version,
    ghcToolchainGhcPkgExe :: FilePath,
    ghcToolchainLibDir :: FilePath
  }
  deriving (Eq, Show)

data PackageEnvironmentSnapshot = PackageEnvironmentSnapshot
  { packageEnvironmentPackageDbStack :: PackageDbStack,
    packageEnvironmentPackageIndex :: PackageIndex,
    packageEnvironmentSelectedUnitIdsByPackageName :: Map.Map PackageNameText (Set.Set UnitIdText)
  }
  deriving (Eq, Show)

data CapturedGhcEnvironment = CapturedGhcEnvironment
  { capturedGhcToolchain :: GhcToolchain,
    capturedPackageEnvironment :: PackageEnvironmentSnapshot
  }
  deriving (Eq, Show)

data ResolvedPackageEnvironment = ResolvedPackageEnvironment
  { resolvedPackageDbStack :: PackageDbStack,
    resolvedExposedUnitIds :: Set.Set UnitIdText
  }
  deriving (Eq, Show)

data ParsedGhcEnvironmentFile = ParsedGhcEnvironmentFile
  { parsedEnvPackageDbStack :: PackageDbStack,
    parsedEnvSelectedUnitIds :: Set.Set UnitIdText
  }
  deriving (Eq, Show)

data PackageResolutionError
  = MissingPackage PackageNameText
  | AmbiguousPackage PackageNameText [UnitIdText]
  deriving (Eq, Show)

data PackageDbFlagTarget
  = PackageDbFlagsForGhc
  | PackageDbFlagsForGhcPkg
  deriving (Eq, Ord, Show)

renderPackageDbStackFlags :: PackageDbFlagTarget -> PackageDbStack -> [String]
renderPackageDbStackFlags target packageDbStack =
  concatMap (renderPackageDb target) packageDbStack.unPackageDbStack

renderPackageDb :: PackageDbFlagTarget -> PackageDb -> [String]
renderPackageDb target packageDb =
  case (target, packageDb) of
    (PackageDbFlagsForGhc, GlobalPackageDb) -> ["-global-package-db"]
    (PackageDbFlagsForGhc, UserPackageDb) -> ["-user-package-db"]
    (PackageDbFlagsForGhc, SpecificPackageDb dbPath) -> ["-package-db", dbPath]
    (PackageDbFlagsForGhcPkg, GlobalPackageDb) -> ["--global"]
    (PackageDbFlagsForGhcPkg, UserPackageDb) -> ["--user"]
    (PackageDbFlagsForGhcPkg, SpecificPackageDb dbPath) -> ["--package-db=" <> dbPath]
