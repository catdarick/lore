module Lore.Internal.AutoRefactor.Issue
  ( AutoRefactorIssue (..),
    classifyAutoRefactorIssues,
  )
where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (mapMaybe)
import Lore.Diagnostics (Diagnostic (..), DiagnosticSpan (..))
import Lore.Internal.ImportCleanup.Diagnostics (redundantImportIssueFromDiagnostic)
import Lore.Internal.ImportCleanup.Types
  ( AutoRefactorIssue (..),
  )
import Lore.Internal.SourceSpan.Types (Span (..))
import System.FilePath (normalise)

classifyAutoRefactorIssues :: [Diagnostic] -> Maybe (NonEmpty AutoRefactorIssue)
classifyAutoRefactorIssues =
  NE.nonEmpty . mapMaybe autoRefactorIssueFromDiagnostic

autoRefactorIssueFromDiagnostic :: Diagnostic -> Maybe AutoRefactorIssue
autoRefactorIssueFromDiagnostic diagnostic@Diagnostic {diagnosticSpan = RealDiagnosticSpan Span {spanFile}} = do
  let autoRefactorIssueFilePath = normalise spanFile
  autoRefactorIssueRedundantImport <- redundantImportIssueFromDiagnostic diagnostic
  pure AutoRefactorIssue {autoRefactorIssueFilePath, autoRefactorIssueRedundantImport}
autoRefactorIssueFromDiagnostic Diagnostic {diagnosticSpan = UnhelpfulDiagnosticSpan {}} =
  Nothing
