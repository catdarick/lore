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
    discoverProject,
    normalizeRelativePath,
  )
