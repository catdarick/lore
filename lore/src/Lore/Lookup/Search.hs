module Lore.Lookup.Search
  ( ModulePattern,
    ModulePatternError (..),
    compileModulePattern,
    FindSimilarSymbolsOptions (..),
  )
where

import Lore.Internal.Lookup.ModulePattern
  ( ModulePattern,
    ModulePatternError (..),
    compileModulePattern,
  )
import Lore.Lookup (FindSimilarSymbolsOptions (..))
