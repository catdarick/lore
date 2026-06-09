{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lore.Internal.Lookup.SymbolSearch.Base
  ( SearchToken (..),
    SynonymTerm (..),
    SymbolSearchField (..),
    TokenMatchKind (..),
    TokenSpan (..),
    SymbolSearchQuery (..),
    StoredMatchPattern (..),
    QueryTermMatch (..),
    TermMatchEvidence (..),
    SymbolScoreBreakdown (..),
  )
where

import Control.DeepSeq (NFData)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import GHC.Generics (Generic)
import Lore.Internal.Lookup.Name (NormalizedModuleName, NormalizedOccName)

newtype SearchToken = SearchToken
  { unSearchToken :: Text
  }
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Show)
  deriving anyclass (NFData)

newtype SynonymTerm = SynonymTerm
  { synonymTermTokens :: NonEmpty SearchToken
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

data TokenSpan = TokenSpan
  { tokenSpanStart :: Int,
    tokenSpanLength :: Int
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

data SymbolSearchQuery = SymbolSearchQuery
  { symbolSearchText :: Text,
    symbolSearchTokens :: [SearchToken],
    symbolSearchExactModule :: Maybe NormalizedModuleName
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

data StoredMatchPattern
  = StoredTokenPattern SearchToken
  | StoredSynonymTermPattern SynonymTerm
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

data QueryTermMatch = QueryTermMatch
  { matchedQuerySpan :: TokenSpan,
    matchedQueryTokens :: NonEmpty SearchToken,
    matchedStoredPattern :: StoredMatchPattern,
    matchedKind :: TokenMatchKind,
    matchedEditDistance :: Maybe Int,
    matchedQuality :: Double
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

data TermMatchEvidence = TermMatchEvidence
  { evidenceQuerySpan :: TokenSpan,
    evidenceQueryTokens :: NonEmpty SearchToken,
    evidenceStoredTokens :: NonEmpty SearchToken,
    evidenceStoredSpan :: TokenSpan,
    evidenceSourceSequence :: NonEmpty SearchToken,
    evidenceField :: SymbolSearchField,
    evidenceNameVariant :: Maybe NormalizedOccName,
    evidenceMatchKind :: TokenMatchKind,
    evidenceEditDistance :: Maybe Int,
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
