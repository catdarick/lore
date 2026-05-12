module Lore.Internal.AutoRefactor.Issue
  ( AutoRefactorIssue (..),
    classifyAutoRefactorIssues,
  )
where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Lore.Diagnostics (Diagnostic (..), DiagnosticSpan (..), Span (..))
import Lore.Internal.AutoRefactor.RedundantImports (RedundantImportRequest, redundantImportRequestFromDiagnostic)
import System.FilePath (normalise)

data AutoRefactorIssue = AutoRefactorIssue
  { autoRefactorIssueFilePath :: FilePath,
    autoRefactorIssueRequest :: RedundantImportRequest
  }
  deriving (Eq, Show)

classifyAutoRefactorIssues :: [Diagnostic] -> Maybe (NonEmpty AutoRefactorIssue)
classifyAutoRefactorIssues =
  NE.nonEmpty . mapMaybeToList autoRefactorIssueFromDiagnostic

autoRefactorIssueFromDiagnostic :: Diagnostic -> Maybe AutoRefactorIssue
autoRefactorIssueFromDiagnostic diagnostic@Diagnostic {diagnosticSpan = RealDiagnosticSpan Span {spanFile}} = do
  let autoRefactorIssueFilePath = normalise spanFile
  autoRefactorIssueRequest <- redundantImportRequestFromDiagnostic diagnostic
  pure AutoRefactorIssue {autoRefactorIssueFilePath, autoRefactorIssueRequest}
autoRefactorIssueFromDiagnostic Diagnostic {diagnosticSpan = UnhelpfulDiagnosticSpan {}} =
  Nothing

mapMaybeToList :: (a -> Maybe b) -> [a] -> [b]
mapMaybeToList f =
  foldr
    (\value acc -> maybe acc (: acc) (f value))
    []
