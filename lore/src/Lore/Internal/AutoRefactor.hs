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
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (maybeToList)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import qualified GHC
import Lore.Internal.AutoRefactor.Edit (AppliedFileEdits (..), applyFileEdits, restoreFileContents)
import Lore.Internal.AutoRefactor.ImportDecl (parseImports)
import Lore.Internal.AutoRefactor.ImportRewrite (ImportRewriteResult (..), rewriteImportsInFile)
import Lore.Internal.AutoRefactor.Issue (AutoRefactorIssue (..), AutoRefactorPayload (..))
import Lore.Internal.AutoRefactor.MissingImports (suggestMissingImportOperations)
import Lore.Internal.AutoRefactor.RedundantImports (suggestRedundantImportOperations)
import Lore.Internal.Lookup.ModSummaries (prepareFreshModSummariesByFile)
import Lore.Internal.Lookup.SymbolsMap (getCachedSymbolsMap)
import Lore.Internal.Lookup.Types (SymbolsMap)
import qualified Lore.Logger as Log
import Lore.Monad (MonadLore)

data AutoRefactorResult = AutoRefactorResult
  { autoRefactorApplied :: Bool,
    autoRefactorOriginalContents :: Map.Map FilePath Text,
    autoRefactorSummaryByFile :: Map.Map FilePath [String]
  }

applyAutoRefactor :: (MonadLore m) => NonEmpty AutoRefactorIssue -> m AutoRefactorResult
applyAutoRefactor issues = do
  symbolsMap <- getCachedSymbolsMap
  modSummariesByFile <- prepareFreshModSummariesByFile
  let groupedIssues = Map.toList (groupIssuesByFile issues)
  rewriteResults <- mapM (rewriteIssuesInFile symbolsMap modSummariesByFile) groupedIssues
  let edits = concatMap rewriteEdits rewriteResults
      logs = concatMap rewriteLogs rewriteResults
      rewriteLogsByFile =
        Map.fromList
          [ (filePath, rewriteLogs result)
          | ((filePath, _), result) <- zip groupedIssues rewriteResults
          ]
  forM_ logs Log.info
  AppliedFileEdits {appliedChangedFiles, appliedOriginalContents} <- applyFileEdits edits
  pure
    AutoRefactorResult
      { autoRefactorApplied = not (null appliedChangedFiles),
        autoRefactorOriginalContents = appliedOriginalContents,
        autoRefactorSummaryByFile =
          Map.fromList
            [ (filePath, fileLogs)
            | filePath <- appliedChangedFiles,
              fileLogs <- maybeToList (Map.lookup filePath rewriteLogsByFile)
            ]
      }

rollbackAutoRefactorEdits :: (MonadLore m) => Map.Map FilePath Text -> m ()
rollbackAutoRefactorEdits =
  restoreFileContents

rewriteIssuesInFile ::
  (MonadLore m) =>
  SymbolsMap ->
  Map.Map FilePath GHC.ModSummary ->
  (FilePath, NonEmpty AutoRefactorIssue) ->
  m ImportRewriteResult
rewriteIssuesInFile symbolsMap modSummariesByFile (filePath, fileIssues) =
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
              missingRequests =
                NE.nonEmpty
                  [ request
                  | issue <- NE.toList fileIssues,
                    MissingImportPayload request <- [issue.autoRefactorIssuePayload]
                  ]
              redundantRequests =
                NE.nonEmpty
                  [ request
                  | issue <- NE.toList fileIssues,
                    RedundantImportPayload request <- [issue.autoRefactorIssuePayload]
                  ]
              redundantOperations =
                maybe [] (suggestRedundantImportOperations parsedImports) redundantRequests
          missingOperations <-
            maybe
              (pure [])
              (suggestMissingImportOperations parsedImports symbolsMap)
              missingRequests
          pure $
            rewriteImportsInFile
              filePath
              parsedModule
              source
              (missingOperations <> redundantOperations)

groupIssuesByFile :: NonEmpty AutoRefactorIssue -> Map.Map FilePath (NonEmpty AutoRefactorIssue)
groupIssuesByFile =
  Map.fromListWith
    (<>)
    . map (\issue -> (issue.autoRefactorIssueFilePath, issue :| []))
    . NE.toList
