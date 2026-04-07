{-# LANGUAGE LambdaCase #-}

module Internal.AutoRefact
  ( applyAutoRefact,
  )
where

import qualified Data.Map.Strict as Map
import qualified GHC
import Internal.AutoRefact.CollapseImports (collapseImportsInFiles)
import Internal.AutoRefact.Edit (applyFileEdits)
import Internal.AutoRefact.MissingImports (suggestMissingImportEdits)
import Internal.AutoRefact.RedundantImports (suggestRedundantImportEdits)
import Internal.Diagnostics (Diagnostic)
import Internal.Lookup.SymbolsMap (getSymbolsMap)
import Internal.Lookup.Types (SymbolsMap (..))
import Monad (MonadLore)
import System.FilePath (normalise)

applyAutoRefact :: (MonadLore m) => [Diagnostic] -> m Bool
applyAutoRefact diagnostics = do
  SymbolsMap symbolsMap <- getSymbolsMap
  modSummariesByFile <- currentModSummariesByFile
  missingImportEdits <- concat <$> mapM (suggestMissingImportEdits symbolsMap) diagnostics
  redundantImportEdits <- concat <$> mapM (suggestRedundantImportEdits modSummariesByFile) diagnostics
  changedFiles <- applyFileEdits (missingImportEdits <> redundantImportEdits)
  refreshedModSummariesByFile <- currentModSummariesByFile
  collapsedImports <- collapseImportsInFiles refreshedModSummariesByFile changedFiles
  pure (not (null changedFiles) || collapsedImports)

currentModSummariesByFile :: (MonadLore m) => m (Map.Map FilePath GHC.ModSummary)
currentModSummariesByFile = do
  moduleGraph <- GHC.depanal [] False
  pure $
    Map.fromList
      [ (normalise sourceFile, summary)
      | summary <- GHC.mgModSummaries moduleGraph,
        sourceFile <- maybeToList (GHC.ml_hs_file (GHC.ms_location summary))
      ]

maybeToList :: Maybe a -> [a]
maybeToList = \case
  Just value -> [value]
  Nothing -> []
