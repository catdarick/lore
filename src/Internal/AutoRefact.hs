{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Internal.AutoRefact
  ( AutoRefactResult (..),
    applyAutoRefact,
    rollbackAutoRefactEdits,
  )
where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import qualified GHC
import Internal.AutoRefact.Edit (AppliedFileEdits (..), applyFileEdits, restoreFileContents)
import Internal.AutoRefact.ImportDecl (parseImports)
import Internal.AutoRefact.ImportRewrite (ImportRewriteResult (..), rewriteImportsInFile)
import Internal.AutoRefact.MissingImports (suggestMissingImportOperations)
import Internal.AutoRefact.RedundantImports (suggestRedundantImportOperations)
import Internal.Diagnostics (Diagnostic (..), DiagnosticSpan (..), Span (..))
import qualified Internal.Logger as Log
import Internal.Lookup.SymbolsMap (getSymbolsMap)
import Internal.Lookup.Types (ExportedSymbol, SymbolsMap (..))
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
  rewriteResults <- mapM (rewriteDiagnosticsInFile symbolsMap modSummariesByFile) (Map.toList (groupDiagnosticsByFile diagnostics))
  let edits = concatMap rewriteEdits rewriteResults
      logs = concatMap rewriteLogs rewriteResults
  forM_ logs Log.info
  AppliedFileEdits {appliedChangedFiles, appliedOriginalContents} <- applyFileEdits edits
  pure
    AutoRefactResult
      { autoRefactApplied = not (null appliedChangedFiles),
        autoRefactOriginalContents = appliedOriginalContents
      }

rollbackAutoRefactEdits :: (MonadLore m) => Map.Map FilePath Text -> m ()
rollbackAutoRefactEdits =
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
