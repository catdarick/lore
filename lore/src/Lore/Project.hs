module Lore.Project
  ( GhcOption (..),
    Extension (..),
    ComponentKind (..),
    PackageData (..),
    ComponentData (..),
    discoverProject,
  )
where

import Lore.Internal.Ghc.DynFlags (Extension (..), GhcOption (..))
import Lore.Internal.Package (ComponentData (..), ComponentKind (..), PackageData (..), discoverProject)
