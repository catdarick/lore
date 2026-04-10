module Lore.Internal.Lookup.Types
  ( SymbolsMap (..),
    SymbolsIndex (..),
    ExternalPackagesSymbolsCache (..),
    ModSummaries (..),
    NameToInstancesIndex (..),
    Symbol (..),
    SymbolVisibility (..),
    symbolExportedFrom,
  )
where

import Data.List (intercalate)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified GHC
import qualified GHC.Plugins as GHC

newtype SymbolsIndex = SymbolsIndex
  { unSymbolsIndex :: Map.Map Text [Symbol]
  }

data SymbolsMap = SymbolsMap
  { homeSymbolsMap :: SymbolsIndex,
    externalSymbolsMap :: SymbolsIndex
  }

data ExternalPackagesSymbolsCache = ExternalPackagesSymbolsCache
  { externalPackagesDependencies :: Set.Set String,
    externalPackagesSymbolsMap :: SymbolsIndex
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

data SymbolVisibility
  = Symbol'ExportedFrom [GHC.Module]
  | Symbol'Unexported
  deriving stock (Eq)

symbolExportedFrom :: Symbol -> [GHC.Module]
symbolExportedFrom symbol =
  case symbol.visibility of
    Symbol'ExportedFrom modules_ -> modules_
    Symbol'Unexported -> []

instance Show Symbol where
  show symbol = showName symbol.name <> " (" <> showVisibility symbol.visibility <> ")"
    where
      showName n = case GHC.nameModule_maybe n of
        Nothing -> "<UNKNOWN>." <> GHC.occNameString (GHC.nameOccName n)
        Just m -> GHC.moduleNameString (GHC.moduleName m) <> "." <> GHC.occNameString (GHC.nameOccName n)
      showModule m = GHC.moduleNameString (GHC.moduleName m)
      showModules xs = intercalate ", " (map showModule xs)
      showVisibility visibility_ =
        case visibility_ of
          Symbol'ExportedFrom modules_ -> "exported from: " <> showModules modules_
          Symbol'Unexported -> "unexported"
