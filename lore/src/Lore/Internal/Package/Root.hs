module Lore.Internal.Package.Root
  ( PackageRoot (..),
    normalizePackageRoot,
    normalizePackageRoots,
  )
where

import Data.List (nub, sort)
import Lore.Internal.Package.Path (normalizeRelativePath)

data PackageRoot = PackageRoot
  { packageRootPath :: FilePath,
    packageRootPreferredCabalFile :: Maybe FilePath
  }
  deriving (Eq, Ord, Show)

normalizePackageRoot :: PackageRoot -> PackageRoot
normalizePackageRoot root =
  PackageRoot
    { packageRootPath = normalizeRelativePath root.packageRootPath,
      packageRootPreferredCabalFile = normalizeRelativePath <$> root.packageRootPreferredCabalFile
    }

normalizePackageRoots :: [PackageRoot] -> [PackageRoot]
normalizePackageRoots =
  sort . nub . map normalizePackageRoot
