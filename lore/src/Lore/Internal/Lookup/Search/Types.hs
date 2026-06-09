{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lore.Internal.Lookup.Search.Types
  ( SearchToken (..),
    SearchContextField (..),
    SearchDocument (..),
    IndexedOccurrence (..),
    TokenSearchIndex (..),
    QueryTokenMatch (..),
    TokenMatchKind (..),
    SearchResult (..),
  )
where

import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)

newtype SearchToken = SearchToken
  { unSearchToken :: Text
  }
  deriving newtype (Eq, Ord, Show)

data SearchDocument = SearchDocument
  { primaryText :: Text,
    contextTexts :: Map SearchContextField [Text]
  }
  deriving stock (Eq, Show)

data SearchContextField
  = SearchContextModule
  | SearchContextResultType
  | SearchContextArgumentType
  deriving stock (Eq, Ord, Show)

data IndexedOccurrence key value = IndexedOccurrence
  { indexedOccurrenceKey :: key,
    indexedOccurrencePrimaryText :: Text,
    indexedOccurrencePrimaryTokens :: [SearchToken],
    indexedOccurrenceContextTokens :: Map SearchContextField (Set SearchToken),
    indexedOccurrenceValue :: value
  }
  deriving stock (Eq, Show)

data TokenSearchIndex key value = TokenSearchIndex
  { indexedOccurrences :: Map key (IndexedOccurrence key value),
    occurrencesByToken :: Map SearchToken (Set key),
    primaryTokenFrequency :: Map SearchToken Int,
    contextTokenFrequency :: Map SearchContextField (Map SearchToken Int),
    totalPrimaryOccurrences :: Int,
    totalContextOccurrences :: Map SearchContextField Int
  }
  deriving stock (Eq, Show)

data QueryTokenMatch = QueryTokenMatch
  { queryToken :: SearchToken,
    matchedToken :: SearchToken,
    tokenMatchKind :: TokenMatchKind,
    tokenDistance :: Int,
    tokenSimilarityWeight :: Double
  }
  deriving stock (Eq, Show)

data TokenMatchKind
  = TokenMatchExact
  | TokenMatchCanonical
  | TokenMatchSynonym
  | TokenMatchFuzzy
  deriving stock (Eq, Ord, Show)

data SearchResult key value = SearchResult
  { searchResultKey :: key,
    searchResultText :: Text,
    searchResultValue :: value,
    searchResultScore :: Double,
    searchResultWholeDistance :: Int
  }
  deriving stock (Eq, Show)
