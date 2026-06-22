module Lore.Project
  ( GhcOption (..),
    Extension (..),
    ComponentKind (..),
    PackageData (..),
    ComponentData (..),
    componentMainModulePathCandidates,
    normalizeRelativePath,
    commonSetIntersection,
    discoverProject,
  )
where

import Lore.Internal.Ghc.DynFlags (Extension (..), GhcOption (..))
import Lore.Internal.Package
  ( ComponentData (..),
    ComponentKind (..),
    PackageData (..),
    commonSetIntersection,
    componentMainModulePathCandidates,
    normalizeRelativePath,
  )
import Lore.Internal.ProjectEnvironment.Access (getProjectPackages)
import Lore.Monad (MonadLore)

discoverProject :: (MonadLore m) => m [PackageData]
discoverProject =
  getProjectPackages
