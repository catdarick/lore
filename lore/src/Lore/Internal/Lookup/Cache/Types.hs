module Lore.Internal.Lookup.Cache.Types
  ( HomeSymbolsIndexCache (..),
    ExternalSymbolsIndexCache (..),
    ExternalSymbolsSnapshot (..),
    SimilarSymbolsSearchIndexCache (..),
    SimilarSymbolsSearchIndex (..),
    SymbolsDependencySetCache (..),
    ModSummariesCache (..),
    NameToInstancesIndexCache (..),
  )
where

import qualified Data.Set as Set
import Lore.Internal.Lookup.Name (NormalizedOccName)
import Lore.Internal.Lookup.Search.Types (TokenSearchIndex)
import Lore.Internal.Lookup.Types (ModSummaries, NameToInstancesIndex, Symbol, SymbolsIndex)

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

newtype SimilarSymbolsSearchIndexCache = SimilarSymbolsSearchIndexCache
  { cachedSimilarSymbolsSearchIndex :: Maybe SimilarSymbolsSearchIndex
  }

newtype SimilarSymbolsSearchIndex = SimilarSymbolsSearchIndex
  { unSimilarSymbolsSearchIndex :: TokenSearchIndex NormalizedOccName (Set.Set Symbol)
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
