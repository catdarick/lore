module Lore.Project
  ( GhcOption (..),
    Extension (..),
    PackageData (..),
    ComponentData (..),
    discoverProject,
    projectRootPath,
  )
where

import Control.Monad.RWS (asks)
import Lore.Internal.Ghc.DynFlags (Extension (..), GhcOption (..))
import Lore.Internal.Package (ComponentData (..), PackageData (..), discoverProject)
import Lore.Internal.Session (SessionContext (..))
import Lore.Monad (MonadLore)

projectRootPath :: (MonadLore m) => m FilePath
projectRootPath =
  asks projectRoot
