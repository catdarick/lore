module Lore.Project
  ( GhcOption (..),
    Extension (..),
    PackageData (..),
    ComponentData (..),
    discoverProject,
  )
where

import Lore.Internal.Ghc.DynFlags (Extension (..), GhcOption (..))
import Lore.Internal.Package (ComponentData (..), PackageData (..), discoverProject)
