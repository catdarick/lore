module Lore.Internal.Lookup.Cache.Types
  ( HomeSymbolsIndexCache (..),
    ExternalSymbolsIndexCache (..),
    ExternalSymbolsSnapshot (..),
    SymbolSearchIndexCache (..),
    ExternalSymbolsEnvironmentKeyCache (..),
    ModSummariesCache (..),
    NameToInstancesIndexCache (..),
  )
where

import qualified Data.Set as Set
import Lore.Internal.Lookup.SymbolSearch.Types (SymbolSearchIndex)
import Lore.Internal.Lookup.Types (ModSummaries, NameToInstancesIndex, SymbolsIndex)

newtype HomeSymbolsIndexCache = HomeSymbolsIndexCache
  { cachedHomeSymbolsIndex :: Maybe SymbolsIndex
  }

newtype ExternalSymbolsIndexCache = ExternalSymbolsIndexCache
  { cachedExternalSymbolsSnapshot :: Maybe ExternalSymbolsSnapshot
  }

data ExternalSymbolsSnapshot = ExternalSymbolsSnapshot
  { externalSymbolsSnapshotEnvironmentKey :: Set.Set String,
    externalSymbolsSnapshotIndex :: SymbolsIndex
  }

newtype SymbolSearchIndexCache = SymbolSearchIndexCache
  { cachedSymbolSearchIndex :: Maybe SymbolSearchIndex
  }

newtype ExternalSymbolsEnvironmentKeyCache = ExternalSymbolsEnvironmentKeyCache
  { cachedExternalSymbolsEnvironmentKey :: Set.Set String
  }

newtype ModSummariesCache = ModSummariesCache
  { cachedModSummaries :: Maybe ModSummaries
  }

newtype NameToInstancesIndexCache = NameToInstancesIndexCache
  { cachedNameToInstancesIndex :: Maybe NameToInstancesIndex
  }
