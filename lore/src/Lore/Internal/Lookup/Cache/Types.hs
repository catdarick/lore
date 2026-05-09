module Lore.Internal.Lookup.Cache.Types
  ( HomeSymbolsIndexCache (..),
    ExternalSymbolsIndexCache (..),
    ExternalSymbolsSnapshot (..),
    SymbolsDependencySetCache (..),
    ModSummariesCache (..),
    NameToInstancesIndexCache (..),
  )
where

import qualified Data.Set as Set
import Lore.Internal.Lookup.Types (ModSummaries, NameToInstancesIndex, SymbolsIndex)

newtype HomeSymbolsIndexCache = HomeSymbolsIndexCache
  { cachedHomeSymbolsIndex :: Maybe SymbolsIndex
  }

newtype ExternalSymbolsIndexCache = ExternalSymbolsIndexCache
  { cachedExternalSymbolsSnapshot :: Maybe ExternalSymbolsSnapshot
  }

data ExternalSymbolsSnapshot = ExternalSymbolsSnapshot
  { externalSymbolsSnapshotDependencies :: Set.Set String,
    externalSymbolsSnapshotIndex :: SymbolsIndex
  }

newtype SymbolsDependencySetCache = SymbolsDependencySetCache
  { cachedSymbolsDependencySet :: Set.Set String
  }

newtype ModSummariesCache = ModSummariesCache
  { cachedModSummaries :: Maybe ModSummaries
  }

newtype NameToInstancesIndexCache = NameToInstancesIndexCache
  { cachedNameToInstancesIndex :: Maybe NameToInstancesIndex
  }
