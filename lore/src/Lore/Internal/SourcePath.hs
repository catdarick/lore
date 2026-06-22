module Lore.Internal.SourcePath
  ( normalizeSourceFilePathM,
  )
where

import Control.Monad.RWS (asks)
import Lore.Internal.ProjectPath (absoluteProjectPath)
import Lore.Internal.Session (SessionContext (..))
import Lore.Monad (MonadLore)

normalizeSourceFilePathM :: (MonadLore m) => FilePath -> m FilePath
normalizeSourceFilePathM sourceFilePath = do
  root <- asks projectRoot
  pure (absoluteProjectPath root sourceFilePath)
