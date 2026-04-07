{-# LANGUAGE LambdaCase #-}

module Internal.AutoRefact
  ( AutoRefactResult (..),
    applyAutoRefact,
    rollbackAutoRefactEdits,
  )
where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified GHC
import Internal.AutoRefact.CollapseImports (collapseImportsInFiles)
import Internal.AutoRefact.Edit (AppliedFileEdits (..), applyFileEdits, restoreFileContents)
import Internal.AutoRefact.MissingImports (suggestMissingImportEdits)
import Internal.AutoRefact.RedundantImports (suggestRedundantImportEdits)
import Internal.Diagnostics (Diagnostic)
import Internal.Lookup.SymbolsMap (getSymbolsMap)
import Internal.Lookup.Types (SymbolsMap (..))
import Monad (MonadLore)
import System.FilePath (normalise)

data AutoRefactResult = AutoRefactResult
  { autoRefactApplied :: Bool,
    autoRefactOriginalContents :: Map.Map FilePath Text
  }

applyAutoRefact :: (MonadLore m) => [Diagnostic] -> m AutoRefactResult
applyAutoRefact diagnostics = do
  SymbolsMap symbolsMap <- getSymbolsMap
  modSummariesByFile <- currentModSummariesByFile
  missingImportEdits <- suggestMissingImportEdits modSummariesByFile symbolsMap diagnostics
  redundantImportEdits <- concat <$> mapM (suggestRedundantImportEdits modSummariesByFile) diagnostics
  AppliedFileEdits {appliedChangedFiles, appliedOriginalContents} <- applyFileEdits (missingImportEdits <> redundantImportEdits)
  refreshedModSummariesByFile <- currentModSummariesByFile
  AppliedFileEdits {appliedChangedFiles = collapsedChangedFiles, appliedOriginalContents = collapsedOriginalContents} <-
    collapseImportsInFiles refreshedModSummariesByFile appliedChangedFiles
  pure
    AutoRefactResult
      { autoRefactApplied = not (null appliedChangedFiles) || not (null collapsedChangedFiles),
        autoRefactOriginalContents = Map.union appliedOriginalContents collapsedOriginalContents
      }

rollbackAutoRefactEdits :: (MonadLore m) => Map.Map FilePath Text -> m ()
rollbackAutoRefactEdits =
  restoreFileContents

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
