module Lore.Internal.SourcePath
  ( normalizeSourceFilePathM,
  )
where

import Control.Monad.RWS (asks)
import Lore.Internal.Session (SessionContext (..))
import Lore.Monad (MonadLore)
import System.FilePath (isRelative, normalise, (</>))

normalizeSourceFilePathM :: (MonadLore m) => FilePath -> m FilePath
normalizeSourceFilePathM sourceFilePath = do
  root <- asks projectRoot
  let rootedPath =
        if isRelative sourceFilePath
          then root </> sourceFilePath
          else sourceFilePath
  pure (normalise rootedPath)
