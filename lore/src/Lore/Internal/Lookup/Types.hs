{-# LANGUAGE DeriveAnyClass #-}

module Lore.Internal.Lookup.Types
  ( SymbolsMap (..),
    SymbolsIndex (..),
    ModSummaries (..),
    NameToInstancesIndex (..),
    Symbol (..),
    SymbolSuggestionCandidate (..),
    SymbolSuggestion (..),
    SymbolVisibility (..),
    symbolExportedFrom,
  )
where

import Control.DeepSeq (NFData)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified GHC
import GHC.Generics (Generic)
import qualified GHC.Types.Name.Env as NameEnv
import Lore.Internal.Lookup.Name (NormalizedOccName)

newtype SymbolsIndex = SymbolsIndex
  { unSymbolsIndex :: Map.Map NormalizedOccName (Set.Set Symbol)
  }

data SymbolsMap = SymbolsMap
  { homeSymbolsMap :: SymbolsIndex,
    externalSymbolsMap :: SymbolsIndex
  }

newtype ModSummaries = ModSummaries
  { unModSummaries :: Map.Map GHC.Module GHC.ModSummary
  }

newtype NameToInstancesIndex = NameToInstancesIndex
  { unNameToInstancesIndex :: NameEnv.NameEnv ([GHC.ClsInst], [GHC.FamInst])
  }

data Symbol = Symbol
  { name :: GHC.Name,
    visibility :: SymbolVisibility
  }
  deriving (Generic, NFData, Eq, Ord)

data SymbolSuggestion = SymbolSuggestion
  { suggestedSymbol :: Symbol,
    suggestedLookupName :: Text,
    suggestionScore :: Double
  }
  deriving (Generic, NFData, Eq, Ord)

data SymbolSuggestionCandidate = SymbolSuggestionCandidate
  { suggestionCandidateSymbols :: Set.Set Symbol,
    suggestionCandidateLookupName :: Text,
    suggestionCandidateScore :: Double
  }
  deriving (Generic, NFData, Eq, Ord)

data SymbolVisibility
  = Symbol'ExportedFrom (Set.Set GHC.Module)
  | Symbol'Unexported
  deriving (Generic, NFData, Eq, Ord)

symbolExportedFrom :: Symbol -> Set.Set GHC.Module
symbolExportedFrom symbol =
  case symbol.visibility of
    Symbol'ExportedFrom modules_ -> modules_
    Symbol'Unexported -> Set.empty
