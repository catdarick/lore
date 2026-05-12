{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Lore.Internal.AutoRefactor
  ( AutoRefactorResult (..),
    applyAutoRefactor,
  )
where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (maybeToList)
import qualified Data.Text.IO as TIO
import qualified GHC
import Lore.Internal.AutoRefactor.Edit (AppliedFileEdits (..), applyFileEdits)
import Lore.Internal.AutoRefactor.ImportDecl (parseImports)
import Lore.Internal.AutoRefactor.ImportRewrite (ImportRewriteResult (..), rewriteImportsInFile)
import Lore.Internal.AutoRefactor.Issue (AutoRefactorIssue (..))
import Lore.Internal.AutoRefactor.RedundantImports (suggestRedundantImportOperations)
import Lore.Internal.Lookup.ModSummaries (prepareFreshModSummariesByFile)
import qualified Lore.Logger as Log
import Lore.Monad (MonadLore)

data AutoRefactorResult = AutoRefactorResult
  { autoRefactorApplied :: Bool,
    autoRefactorChangedFiles :: [FilePath],
    autoRefactorSummaryByFile :: Map.Map FilePath [String]
  }

applyAutoRefactor :: (MonadLore m) => NonEmpty AutoRefactorIssue -> m AutoRefactorResult
applyAutoRefactor issues = do
  modSummariesByFile <- prepareFreshModSummariesByFile
  let groupedIssues = Map.toList (groupIssuesByFile issues)
  rewriteResults <- mapM (rewriteIssuesInFile modSummariesByFile) groupedIssues
  let edits = concatMap rewriteEdits rewriteResults
      logs = concatMap rewriteLogs rewriteResults
      rewriteLogsByFile =
        Map.fromList
          [ (filePath, rewriteLogs result)
          | ((filePath, _), result) <- zip groupedIssues rewriteResults
          ]
  forM_ logs Log.info
  AppliedFileEdits {appliedChangedFiles} <- applyFileEdits edits
  pure
    AutoRefactorResult
      { autoRefactorApplied = not (null appliedChangedFiles),
        autoRefactorChangedFiles = appliedChangedFiles,
        autoRefactorSummaryByFile =
          Map.fromList
            [ (filePath, fileLogs)
            | filePath <- appliedChangedFiles,
              fileLogs <- maybeToList (Map.lookup filePath rewriteLogsByFile)
            ]
      }

rewriteIssuesInFile ::
  (MonadLore m) =>
  Map.Map FilePath GHC.ModSummary ->
  (FilePath, NonEmpty AutoRefactorIssue) ->
  m ImportRewriteResult
rewriteIssuesInFile modSummariesByFile (filePath, fileIssues) =
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
              redundantRequests =
                fmap (.autoRefactorIssueRequest) fileIssues
              operations =
                suggestRedundantImportOperations parsedImports redundantRequests
          pure $
            rewriteImportsInFile
              filePath
              parsedModule
              source
              operations

groupIssuesByFile :: NonEmpty AutoRefactorIssue -> Map.Map FilePath (NonEmpty AutoRefactorIssue)
groupIssuesByFile =
  Map.fromListWith
    (<>)
    . map (\issue -> (issue.autoRefactorIssueFilePath, issue :| []))
    . NE.toList
