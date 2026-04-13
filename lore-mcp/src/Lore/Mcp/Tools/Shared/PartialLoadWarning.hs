module Lore.Mcp.Tools.Shared.PartialLoadWarning
  ( mkPartialWarning,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Lore (LoadTargetsResult (..))

mkPartialWarning :: LoadTargetsResult -> Maybe Text
mkPartialWarning loadResult =
  if loadResult.loadTargetsModulesFailed > 0
    then Just partialLoadWarning
    else Nothing
  where
    partialLoadWarning =
      "Warning: only "
        <> T.pack (show loadResult.loadTargetsModulesLoaded)
        <> " of "
        <> T.pack (show loadResult.loadTargetsModulesTotal)
        <> " modules loaded successfully. This may lead to incomplete or inaccurate results."
