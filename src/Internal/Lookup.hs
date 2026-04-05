{-# OPTIONS_GHC -Wno-orphans #-}

module Internal.Lookup (findSymbol) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Internal.Lookup.SymbolsMap (getSymbolsMap)
import Internal.Lookup.Types (ExportedSymbol, SymbolsMap (..))
import Monad (MonadLore)

findSymbol :: (MonadLore m) => Text -> m [ExportedSymbol]
findSymbol needle = do
  SymbolsMap symbolsMap <- getSymbolsMap
  case Map.lookup needle symbolsMap of
    Nothing -> pure []
    Just names -> pure names
