{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lore.Internal.Lookup.Search.Types
  ( SearchToken (..),
    IndexedOccurrence (..),
    TokenSearchIndex (..),
    QueryTokenMatch (..),
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

data IndexedOccurrence key value = IndexedOccurrence
  { indexedOccurrenceKey :: key,
    indexedOccurrenceText :: Text,
    indexedOccurrenceTokens :: [SearchToken],
    indexedOccurrenceValue :: value
  }
  deriving stock (Eq, Show)

data TokenSearchIndex key value = TokenSearchIndex
  { indexedOccurrences :: Map key (IndexedOccurrence key value),
    occurrencesByToken :: Map SearchToken (Set key),
    tokenFrequency :: Map SearchToken Int,
    totalOccurrences :: Int
  }
  deriving stock (Eq, Show)

data QueryTokenMatch = QueryTokenMatch
  { queryToken :: SearchToken,
    matchedToken :: SearchToken,
    tokenDistance :: Int,
    tokenWeight :: Double
  }
  deriving stock (Eq, Show)

data SearchResult key value = SearchResult
  { searchResultKey :: key,
    searchResultText :: Text,
    searchResultValue :: value,
    searchResultScore :: Double,
    searchResultWholeDistance :: Int
  }
  deriving stock (Eq, Show)
