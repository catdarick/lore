module Lore.TemporalModules
  ( TemporalModule (..),
    createTemporalModule,
  )
where

import Lore.Internal.TemporalModules (TemporalModule (..))
import qualified Lore.Internal.TemporalModules as Internal
import Lore.Monad (MonadLore)

createTemporalModule :: (MonadLore m) => m FilePath
createTemporalModule =
  Internal.createTemporalModule
