{-# LANGUAGE DeriveAnyClass #-}

module Lore.Internal.Lookup.Types
  ( SymbolsMap (..),
    SymbolsIndex (..),
    ModSummaries (..),
    NameToInstancesIndex (..),
    Symbol (..),
    SymbolVisibility (..),
    symbolExportedFrom,
    isSymbolNameMatching,
  )
where

import Control.DeepSeq (NFData)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified GHC
import GHC.Generics (Generic)
import qualified GHC.Plugins as GHC
import Lore.Internal.Lookup.Name (NormalizedName (..), NormalizedOccName, extractAndNormalizeModuleName, normalizeName)

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
  { unNameToInstancesIndex :: GHC.NameEnv ([GHC.ClsInst], [GHC.FamInst])
  }

data Symbol = Symbol
  { name :: GHC.Name,
    visibility :: SymbolVisibility
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

isSymbolNameMatching :: NormalizedName -> Symbol -> Bool
isSymbolNameMatching name symbol =
  let symbolName = normalizeName symbol.name
      definingModuleName = maybe Set.empty Set.singleton symbolName.moduleName
      exportingModuleNames = Set.map extractAndNormalizeModuleName (symbolExportedFrom symbol)
      symbolAssociatedModules = definingModuleName <> exportingModuleNames
   in case name.moduleName of
        Nothing -> symbolName.occName == name.occName
        Just hintedModule ->
          symbolName.occName == name.occName
            && hintedModule `Set.member` symbolAssociatedModules
