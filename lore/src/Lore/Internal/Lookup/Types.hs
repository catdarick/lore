module Lore.Internal.Lookup.Types
  ( SymbolsMap (..),
    SymbolsIndex (..),
    ExternalPackagesSymbolsCache (..),
    ModSummaries (..),
    NameToInstancesIndex (..),
    ExportedSymbol (..),
  )
where

import Data.List (intercalate)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified GHC
import qualified GHC.Plugins as GHC

newtype SymbolsIndex = SymbolsIndex
  { unSymbolsIndex :: Map.Map Text [ExportedSymbol]
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

data ExportedSymbol = ExportedSymbol
  { name :: GHC.Name,
    exportedFrom :: [GHC.Module]
  }

instance Show ExportedSymbol where
  show es = showName es.name <> " (exported from: " <> showModules es.exportedFrom <> ")"
    where
      showName n = case GHC.nameModule_maybe n of
        Nothing -> "<UNKNOWN>." <> GHC.occNameString (GHC.nameOccName n)
        Just m -> GHC.moduleNameString (GHC.moduleName m) <> "." <> GHC.occNameString (GHC.nameOccName n)
      showModule m = GHC.moduleNameString (GHC.moduleName m)
      showModules xs = intercalate ", " (map showModule xs)
