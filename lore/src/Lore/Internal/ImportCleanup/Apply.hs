{-# LANGUAGE NamedFieldPuns #-}

module Lore.Internal.ImportCleanup.Apply
  ( ImportCleanupApplyResult (..),
    applyImportCleanup,
    applyImportCleanupWithDiagnostics,
    applyImportCleanupFromDiagnostics,
    traverseImportCleanupFiles,
    cleanupOneFile,
    parseFileImports,
    planFileImportCleanup,
  )
where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.RWS (asks)
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (maybeToList)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import qualified GHC
import Lore.Diagnostics (Diagnostic)
import Lore.Internal.ImportCleanup.Diagnostics (classifyRedundantImportIssues)
import Lore.Internal.ImportCleanup.Edit (renderImportCleanupEdits)
import qualified Lore.Internal.ImportCleanup.Parse as ImportCleanupParse
import Lore.Internal.ImportCleanup.Resolve (resolveImportCleanupGroups)
import Lore.Internal.ImportCleanup.Types
  ( AutoRefactorIssue (..),
    ImportCleanupFileReport (..),
    ImportCleanupWarning (..),
    ParsedImport,
    PlannedFileEdit (..),
    RedundantImportIssue,
    mapRedundantImportIssueSpan,
    redundantImportIssueSpan,
  )
import Lore.Internal.Session (SessionContext (..))
import Lore.Internal.SourceEdit (AppliedFileEdits (..), FileEdit, applyFileEdits, editFilePath)
import qualified Lore.Internal.SourceEdit as SourceEdit
import Lore.Internal.SourcePath (normalizeSourceFilePathM)
import Lore.Internal.SourceSpan.Types (Span (..))
import qualified Lore.Logger as Log
import Lore.Monad (MonadLore)
import UnliftIO.Exception (tryAny)

data ImportCleanupApplyResult = ImportCleanupApplyResult
  { importCleanupApplied :: Bool,
    importCleanupChangedFiles :: [FilePath],
    importCleanupSummaryByFile :: Map.Map FilePath [String],
    importCleanupSignature :: Map.Map FilePath [FileEdit]
  }

applyImportCleanup ::
  (MonadLore m) =>
  Map.Map FilePath GHC.ModSummary ->
  NonEmpty AutoRefactorIssue ->
  m ImportCleanupApplyResult
applyImportCleanup modSummariesByFile issues =
  applyImportCleanupFromIssues
    modSummariesByFile
    (NE.map (.autoRefactorIssueRedundantImport) issues)

applyImportCleanupWithDiagnostics ::
  (MonadLore m) =>
  Map.Map FilePath GHC.ModSummary ->
  [Diagnostic] ->
  NonEmpty AutoRefactorIssue ->
  m ImportCleanupApplyResult
applyImportCleanupWithDiagnostics modSummariesByFile diagnostics issues =
  case classifyRedundantImportIssues diagnostics of
    Just diagnosticIssues ->
      applyImportCleanupFromIssues modSummariesByFile diagnosticIssues
    Nothing ->
      applyImportCleanup modSummariesByFile issues

applyImportCleanupFromDiagnostics ::
  (MonadLore m) =>
  Map.Map FilePath GHC.ModSummary ->
  [Diagnostic] ->
  m ImportCleanupApplyResult
applyImportCleanupFromDiagnostics modSummariesByFile diagnostics =
  case classifyRedundantImportIssues diagnostics of
    Nothing -> do
      Log.debug "Auto-refactor: no redundant import diagnostics found; skipping."
      pure emptyImportCleanupApplyResult
    Just diagnosticIssues ->
      applyImportCleanupFromIssues modSummariesByFile diagnosticIssues

applyImportCleanupFromIssues ::
  (MonadLore m) =>
  Map.Map FilePath GHC.ModSummary ->
  NonEmpty RedundantImportIssue ->
  m ImportCleanupApplyResult
applyImportCleanupFromIssues modSummariesByFile issues = do
  canonicalIssues <- canonicalizeIssues issues
  reports <- traverseImportCleanupFiles modSummariesByFile canonicalIssues

  let plannedEdits = concatMap (.importCleanupFileEdits) reports
      plannedEditsByFile =
        Map.fromListWith
          (++)
          [ (editFilePath (plannedFileEdit plannedEdit), [plannedEdit])
          | plannedEdit <- plannedEdits
          ]

  logImportCleanupReports reports

  AppliedFileEdits {appliedChangedFiles, appliedEditsByFile, appliedWarnings} <-
    applyFileEdits (map plannedFileEdit plannedEdits)

  let staleWarnings =
        [ (filePath, fileEdit)
        | SourceEdit.StaleFileEditSpan filePath fileEdit <- appliedWarnings
        ]
      staleByFile =
        Map.fromListWith
          (++)
          [ (filePath, [StaleFileEditSpan filePath fileEdit])
          | (filePath, fileEdit) <- staleWarnings
          ]
      appliedSummariesByFile =
        Map.fromList
          [ (filePath, appliedSummariesForFile plannedFileEdits fileAppliedEdits)
          | (filePath, fileAppliedEdits) <- Map.toList appliedEditsByFile,
            plannedFileEdits <- maybeToList (Map.lookup filePath plannedEditsByFile)
          ]

  forM_ (Map.toList appliedSummariesByFile) \(_, summaries) ->
    forM_ summaries Log.info

  forM_ (Map.toList staleByFile) \(filePath, fileWarnings) ->
    forM_ fileWarnings $ \warning ->
      Log.warn ("Import-cleanup: " <> filePath <> ": " <> show warning)

  pure
    ImportCleanupApplyResult
      { importCleanupApplied = not (null appliedChangedFiles),
        importCleanupChangedFiles = appliedChangedFiles,
        importCleanupSummaryByFile = appliedSummariesByFile,
        importCleanupSignature = Map.map (map plannedFileEdit) plannedEditsByFile
      }

traverseImportCleanupFiles ::
  (MonadLore m) =>
  Map.Map FilePath GHC.ModSummary ->
  NonEmpty RedundantImportIssue ->
  m [ImportCleanupFileReport]
traverseImportCleanupFiles modSummariesByFile issues =
  mapM
    (cleanupOneFile modSummariesByFile)
    (Map.toList (groupIssuesByFile issues))

cleanupOneFile ::
  (MonadLore m) =>
  Map.Map FilePath GHC.ModSummary ->
  (FilePath, NonEmpty RedundantImportIssue) ->
  m ImportCleanupFileReport
cleanupOneFile modSummariesByFile (filePath, fileIssues) =
  case Map.lookup filePath modSummariesByFile of
    Nothing ->
      pure
        ImportCleanupFileReport
          { importCleanupFilePath = filePath,
            importCleanupFileEdits = [],
            importCleanupFileWarnings = [MissingModuleSummary filePath]
          }
    Just summary -> do
      parseResult <- parseFileImports filePath summary
      case parseResult of
        Left warning ->
          pure
            ImportCleanupFileReport
              { importCleanupFilePath = filePath,
                importCleanupFileEdits = [],
                importCleanupFileWarnings = [warning]
              }
        Right (source, parsedImports, parseWarnings) ->
          if null parseWarnings
            then pure (planFileImportCleanup filePath source parsedImports fileIssues)
            else
              pure
                ImportCleanupFileReport
                  { importCleanupFilePath = filePath,
                    importCleanupFileEdits = [],
                    importCleanupFileWarnings = parseWarnings
                  }

parseFileImports ::
  (MonadLore m) =>
  FilePath ->
  GHC.ModSummary ->
  m (Either ImportCleanupWarning (Text, [ParsedImport], [ImportCleanupWarning]))
parseFileImports filePath summary = do
  projectRoot <- asks projectRoot
  sourceResult <- tryAny (liftIO $ TIO.readFile filePath)
  case sourceResult of
    Left _ ->
      pure (Left (SourceReadFailed filePath))
    Right source -> do
      parseResult <-
        GHC.handleSourceError
          (const (pure Nothing))
          (Just <$> GHC.parseModule summary)
      case parseResult of
        Nothing ->
          pure (Left (SourceParseFailed filePath))
        Just parsedModule ->
          let (parsedImports, parseWarnings) = ImportCleanupParse.parseImports projectRoot filePath parsedModule
           in pure (Right (source, parsedImports, parseWarnings))

planFileImportCleanup ::
  FilePath ->
  Text ->
  [ParsedImport] ->
  NonEmpty RedundantImportIssue ->
  ImportCleanupFileReport
planFileImportCleanup filePath source parsedImports fileIssues =
  let (actions, resolveWarnings) =
        resolveImportCleanupGroups parsedImports fileIssues
      (plannedEdits, renderWarnings) =
        renderImportCleanupEdits filePath source actions
      warnings = resolveWarnings <> renderWarnings
   in if null warnings
        then
          ImportCleanupFileReport
            { importCleanupFilePath = filePath,
              importCleanupFileEdits = plannedEdits,
              importCleanupFileWarnings = []
            }
        else
          ImportCleanupFileReport
            { importCleanupFilePath = filePath,
              importCleanupFileEdits = [],
              importCleanupFileWarnings = warnings
            }

canonicalizeIssues :: (MonadLore m) => NonEmpty RedundantImportIssue -> m (NonEmpty RedundantImportIssue)
canonicalizeIssues =
  traverse canonicalizeIssue

canonicalizeIssue :: (MonadLore m) => RedundantImportIssue -> m RedundantImportIssue
canonicalizeIssue issue = do
  canonicalFilePath <- normalizeSourceFilePathM (redundantImportIssueSpan issue).spanFile
  pure (mapRedundantImportIssueSpan (\span' -> span' {spanFile = canonicalFilePath}) issue)

groupIssuesByFile :: NonEmpty RedundantImportIssue -> Map.Map FilePath (NonEmpty RedundantImportIssue)
groupIssuesByFile =
  foldl' insertIssue Map.empty . NE.toList
  where
    insertIssue grouped issue =
      Map.insertWith
        (\newIssues existingIssues -> existingIssues <> newIssues)
        (redundantImportIssueSpan issue).spanFile
        (issue :| [])
        grouped

logImportCleanupReports :: (MonadLore m) => [ImportCleanupFileReport] -> m ()
logImportCleanupReports reports =
  forM_ reports \report ->
    forM_ report.importCleanupFileWarnings \warning ->
      case warning of
        SourceReadFailed {} ->
          Log.warn (renderWarning report warning)
        SourceParseFailed {} ->
          Log.warn (renderWarning report warning)
        _ ->
          Log.debug (renderWarning report warning)
  where
    renderWarning report warning =
      "Import-cleanup: "
        <> report.importCleanupFilePath
        <> ": "
        <> show warning

appliedSummariesForFile :: [PlannedFileEdit] -> [FileEdit] -> [String]
appliedSummariesForFile plannedEdits appliedEdits =
  [ plannedFileEditSummary
  | PlannedFileEdit {plannedFileEdit, plannedFileEditSummary} <- plannedEdits,
    plannedFileEdit `elem` appliedEdits
  ]

emptyImportCleanupApplyResult :: ImportCleanupApplyResult
emptyImportCleanupApplyResult =
  ImportCleanupApplyResult
    { importCleanupApplied = False,
      importCleanupChangedFiles = [],
      importCleanupSummaryByFile = Map.empty,
      importCleanupSignature = Map.empty
    }
