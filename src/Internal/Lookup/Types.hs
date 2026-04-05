module Internal.Lookup.Types where

import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified GHC.Plugins as GHC

newtype SymbolsMap = SymbolsMap
  { unSymbolsMap :: Map.Map Text [ExportedSymbol]
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
