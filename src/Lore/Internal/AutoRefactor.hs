{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Lore.Internal.AutoRefactor
  ( AutoRefactorResult (..),
    applyAutoRefactor,
    rollbackAutoRefactorEdits,
  )
where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import qualified GHC
import Lore.Diagnostics (Diagnostic (..), DiagnosticSpan (..), Span (..))
import Lore.Internal.AutoRefactor.Edit (AppliedFileEdits (..), applyFileEdits, restoreFileContents)
import Lore.Internal.AutoRefactor.ImportDecl (parseImports)
import Lore.Internal.AutoRefactor.ImportRewrite (ImportRewriteResult (..), rewriteImportsInFile)
import Lore.Internal.AutoRefactor.MissingImports (suggestMissingImportOperations)
import Lore.Internal.AutoRefactor.RedundantImports (suggestRedundantImportOperations)
import Lore.Internal.Lookup.SymbolsMap (getSymbolsMap)
import Lore.Internal.Lookup.Types (ExportedSymbol, SymbolsMap (..))
import qualified Lore.Logger as Log
import Lore.Monad (MonadLore)
import System.FilePath (normalise)

data AutoRefactorResult = AutoRefactorResult
  { autoRefactorApplied :: Bool,
    autoRefactorOriginalContents :: Map.Map FilePath Text
  }

applyAutoRefactor :: (MonadLore m) => [Diagnostic] -> m AutoRefactorResult
applyAutoRefactor diagnostics = do
  SymbolsMap symbolsMap <- getSymbolsMap
  modSummariesByFile <- currentModSummariesByFile
  rewriteResults <- mapM (rewriteDiagnosticsInFile symbolsMap modSummariesByFile) (Map.toList (groupDiagnosticsByFile diagnostics))
  let edits = concatMap rewriteEdits rewriteResults
      logs = concatMap rewriteLogs rewriteResults
  forM_ logs Log.info
  AppliedFileEdits {appliedChangedFiles, appliedOriginalContents} <- applyFileEdits edits
  pure
    AutoRefactorResult
      { autoRefactorApplied = not (null appliedChangedFiles),
        autoRefactorOriginalContents = appliedOriginalContents
      }

rollbackAutoRefactorEdits :: (MonadLore m) => Map.Map FilePath Text -> m ()
rollbackAutoRefactorEdits =
  restoreFileContents

rewriteDiagnosticsInFile ::
  (MonadLore m) =>
  Map.Map Text [ExportedSymbol] ->
  Map.Map FilePath GHC.ModSummary ->
  (FilePath, [Diagnostic]) ->
  m ImportRewriteResult
rewriteDiagnosticsInFile symbolsMap modSummariesByFile (filePath, fileDiagnostics) =
  case Map.lookup filePath modSummariesByFile of
    Nothing ->
      pure (ImportRewriteResult [] [])
    Just summary ->
      GHC.handleSourceError
        (const (pure (ImportRewriteResult [] [])))
        do
          parsedModule <- GHC.parseModule summary
          source <- liftIO $ TIO.readFile filePath
          let parsedImports = parseImports parsedModule
              redundantOperations = suggestRedundantImportOperations parsedImports fileDiagnostics
          missingOperations <- suggestMissingImportOperations parsedImports symbolsMap fileDiagnostics
          pure $
            rewriteImportsInFile
              filePath
              parsedModule
              source
              (missingOperations <> redundantOperations)

currentModSummariesByFile :: (MonadLore m) => m (Map.Map FilePath GHC.ModSummary)
currentModSummariesByFile = do
  moduleGraph <- GHC.depanal [] False
  pure $
    Map.fromList
      [ (normalise sourceFile, summary)
      | summary <- GHC.mgModSummaries moduleGraph,
        sourceFile <- maybeToList (GHC.ml_hs_file (GHC.ms_location summary))
      ]

groupDiagnosticsByFile :: [Diagnostic] -> Map.Map FilePath [Diagnostic]
groupDiagnosticsByFile =
  Map.fromListWith
    (<>)
    . mapMaybeToList diagnosticEntry
  where
    diagnosticEntry diagnostic =
      case diagnostic.diagnosticSpan of
        RealDiagnosticSpan Span {spanFile} ->
          Just (normalise spanFile, [diagnostic])
        UnhelpfulDiagnosticSpan {} ->
          Nothing

maybeToList :: Maybe a -> [a]
maybeToList = \case
  Just value -> [value]
  Nothing -> []

mapMaybeToList :: (a -> Maybe b) -> [a] -> [b]
mapMaybeToList f =
  foldr
    (\value acc -> maybe acc (: acc) (f value))
    []
