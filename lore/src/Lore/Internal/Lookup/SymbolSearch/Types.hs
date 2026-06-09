{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lore.Internal.Lookup.SymbolSearch.Types
  ( module Lore.Internal.Lookup.SymbolSearch.Base,
    IndexedNameVariant (..),
    IndexedTokenSequence (..),
    SymbolSearchDocument (..),
    SymbolSearchIndex (..),
  )
where

import Control.DeepSeq (NFData)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Set (Set)
import GHC.Generics (Generic)
import qualified GHC.Types.Name as GHC
import Lore.Internal.Lookup.Name (NormalizedModuleName, NormalizedOccName)
import Lore.Internal.Lookup.SymbolSearch.Base
import Lore.Internal.Lookup.Types (Symbol)

data IndexedNameVariant = IndexedNameVariant
  { indexedName :: NormalizedOccName,
    indexedNameTokens :: NonEmpty SearchToken
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

newtype IndexedTokenSequence = IndexedTokenSequence
  { indexedSequenceTokens :: NonEmpty SearchToken
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

data SymbolSearchDocument = SymbolSearchDocument
  { symbolSearchSymbol :: Symbol,
    symbolSearchNames :: NonEmpty IndexedNameVariant,
    symbolSearchModules :: Set NormalizedModuleName,
    symbolSearchModuleTokenSequences :: Set IndexedTokenSequence,
    symbolSearchResultTypeTokenSequences :: Set IndexedTokenSequence,
    symbolSearchArgumentTypeTokenSequences :: Set IndexedTokenSequence
  }
  deriving stock (Eq, Generic)
  deriving anyclass (NFData)

data SymbolSearchIndex = SymbolSearchIndex
  { searchDocuments :: Map GHC.Name SymbolSearchDocument,
    searchPostings :: Map SymbolSearchField (Map SearchToken (Set GHC.Name)),
    searchDocumentFrequencies :: Map SymbolSearchField (Map SearchToken Int),
    searchFieldDocumentCounts :: Map SymbolSearchField Int,
    searchVocabulary :: Set SearchToken,
    searchTokensByCanonical :: Map SearchToken (Set SearchToken)
  }
  deriving stock (Eq, Generic)
  deriving anyclass (NFData)
