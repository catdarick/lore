module Internal.AutoRefact.CollapseImports
  ( collapseImportsInFiles,
  )
where

import qualified Data.Map.Strict as Map
import Internal.AutoRefact.Edit (AppliedFileEdits (..))
import Monad (MonadLore)

collapseImportsInFiles :: (MonadLore m) => Map.Map FilePath a -> [FilePath] -> m AppliedFileEdits
collapseImportsInFiles _ _ =
  pure
    AppliedFileEdits
      { appliedChangedFiles = [],
        appliedOriginalContents = Map.empty
      }
