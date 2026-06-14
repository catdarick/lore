module Lore.Internal.Package.Types
  ( ComponentData (..),
    ComponentKind (..),
    ComponentIdentity (..),
    DependencyFingerprint,
    PackageData (..),
  )
where

import Control.DeepSeq (NFData (..))
import qualified Data.Set as Set
import qualified GHC
import Lore.Internal.Ghc.DynFlags (Extension, GhcOption, Language)

data ComponentIdentity = ComponentIdentity
  { componentIdentityPackageName :: String,
    componentIdentityComponentName :: String
  }
  deriving (Eq, Ord, Show)

type DependencyFingerprint = String

data ComponentData = ComponentData
  { componentKind :: ComponentKind,
    componentName :: String,
    mainModulePath :: Maybe FilePath,
    language :: Maybe Language,
    ghcOptions :: Set.Set GhcOption,
    defaultExtensions :: Set.Set Extension,
    dependencies :: Set.Set String,
    dependencyRequirements :: Set.Set DependencyFingerprint,
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
