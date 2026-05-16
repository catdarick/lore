module Lore.TemporalModules
  ( createTemporalModule,
  )
where

import qualified Lore.Internal.TemporalModules as Internal
import Lore.Monad (MonadLore)

createTemporalModule :: (MonadLore m) => m FilePath
createTemporalModule =
  Internal.createTemporalModule
