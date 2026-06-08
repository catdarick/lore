module Lore.Internal.Lookup.Cache.Types
  ( HomeSymbolsIndexCache (..),
    ExternalSymbolsIndexCache (..),
    ExternalSymbolsSnapshot (..),
    SimilarSymbolsSearchIndexCache (..),
    SimilarSymbolsSearchIndex (..),
    SimilarSymbolSearchKey (..),
    SymbolsDependencySetCache (..),
    ModSummariesCache (..),
    NameToInstancesIndexCache (..),
  )
where

import qualified Data.Set as Set
import qualified GHC
import Lore.Internal.Lookup.Name (NormalizedOccName)
import Lore.Internal.Lookup.Search.Types (TokenSearchIndex)
import Lore.Internal.Lookup.Types (ModSummaries, NameToInstancesIndex, Symbol, SymbolsIndex)

data SimilarSymbolSearchKey = SimilarSymbolSearchKey
  { searchLookupName :: NormalizedOccName,
    searchSymbolName :: GHC.Name
  }
  deriving stock (Eq, Ord)

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
  { unSimilarSymbolsSearchIndex :: TokenSearchIndex SimilarSymbolSearchKey Symbol
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
