module Lore.Mcp.Tools.Shared.PartialLoadWarning
  ( mkPartialWarning,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Lore (LoadHomeModulesResult (..))

mkPartialWarning :: LoadHomeModulesResult -> Maybe Text
mkPartialWarning loadResult =
  if loadResult.loadHomeModulesFailed > 0
    then Just partialLoadWarning
    else Nothing
  where
    partialLoadWarning =
      "Warning: only "
        <> T.pack (show loadResult.loadHomeModulesLoaded)
        <> " of "
        <> T.pack (show loadResult.loadHomeModulesTotal)
        <> " modules loaded successfully. This may lead to incomplete or inaccurate results."
