{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lore.Internal.Lookup.SymbolSearch.Base
  ( SearchToken (..),
    SymbolSearchField (..),
    TokenMatchKind (..),
    SymbolSearchQuery (..),
    QueryTokenMatch (..),
    TokenMatchEvidence (..),
    SymbolScoreBreakdown (..),
  )
where

import Control.DeepSeq (NFData)
import Data.Text (Text)
import GHC.Generics (Generic)
import Lore.Internal.Lookup.Name (NormalizedModuleName, NormalizedOccName)

newtype SearchToken = SearchToken
  { unSearchToken :: Text
  }
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Show)
  deriving anyclass (NFData)

data SymbolSearchField
  = SearchName
  | SearchResultType
  | SearchArgumentType
  | SearchModule
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

data TokenMatchKind
  = TokenMatchExact
  | TokenMatchCanonical
  | TokenMatchSynonym
  | TokenMatchFuzzy
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

data SymbolSearchQuery = SymbolSearchQuery
  { symbolSearchText :: Text,
    symbolSearchTokens :: [SearchToken],
    symbolSearchExactModule :: Maybe NormalizedModuleName
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

data QueryTokenMatch = QueryTokenMatch
  { matchedQueryToken :: SearchToken,
    matchedStoredToken :: SearchToken,
    matchedKind :: TokenMatchKind,
    matchedDistance :: Int,
    matchedQuality :: Double
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

data TokenMatchEvidence = TokenMatchEvidence
  { evidenceQueryToken :: SearchToken,
    evidenceStoredToken :: SearchToken,
    evidenceField :: SymbolSearchField,
    evidenceNameVariant :: Maybe NormalizedOccName,
    evidenceMatchKind :: TokenMatchKind,
    evidenceMatchDistance :: Int,
    evidenceMatchQuality :: Double,
    evidenceIdf :: Double,
    evidenceContribution :: Double
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

data SymbolScoreBreakdown = SymbolScoreBreakdown
  { matchedEvidenceScore :: Double,
    unmatchedTokenPenalty :: Double,
    orderedNameBonus :: Double,
    nameSpecificityBonus :: Double,
    capitalizationPenalty :: Double
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)
