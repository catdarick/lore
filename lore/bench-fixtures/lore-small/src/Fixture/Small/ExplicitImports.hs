module Fixture.Small.ExplicitImports
  ( indexedNamesCount,
    commonRun,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Fixture.Small.Core (Indexed (..), commonRun, mkIndexed)

indexedNamesCount :: Int
indexedNamesCount =
  Map.size (indexedValues (mkIndexed Set.empty))
