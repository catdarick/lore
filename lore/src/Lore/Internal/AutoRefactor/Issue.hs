module Lore.Internal.AutoRefactor.Issue
  ( AutoRefactorIssue (..),
    AutoRefactorPayload (..),
    classifyAutoRefactorIssues,
  )
where

import Control.Applicative ((<|>))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Lore.Diagnostics (Diagnostic (..), DiagnosticSpan (..), Span (..))
import Lore.Internal.AutoRefactor.MissingImports.Diagnostic (MissingImportRequest, missingImportRequestFromDiagnostic)
import Lore.Internal.AutoRefactor.RedundantImports (RedundantImportRequest, redundantImportRequestFromDiagnostic)
import System.FilePath (normalise)

data AutoRefactorIssue = AutoRefactorIssue
  { autoRefactorIssueFilePath :: FilePath,
    autoRefactorIssuePayload :: AutoRefactorPayload
  }
  deriving (Eq, Show)

data AutoRefactorPayload
  = MissingImportPayload MissingImportRequest
  | RedundantImportPayload RedundantImportRequest
  deriving (Eq, Show)

classifyAutoRefactorIssues :: [Diagnostic] -> Maybe (NonEmpty AutoRefactorIssue)
classifyAutoRefactorIssues =
  NE.nonEmpty . mapMaybeToList autoRefactorIssueFromDiagnostic

autoRefactorIssueFromDiagnostic :: Diagnostic -> Maybe AutoRefactorIssue
autoRefactorIssueFromDiagnostic diagnostic@Diagnostic {diagnosticSpan = RealDiagnosticSpan Span {spanFile}} = do
  let autoRefactorIssueFilePath = normalise spanFile
  autoRefactorIssuePayload <-
    RedundantImportPayload <$> redundantImportRequestFromDiagnostic diagnostic
      <|> MissingImportPayload <$> missingImportRequestFromDiagnostic diagnostic
  pure AutoRefactorIssue {autoRefactorIssueFilePath, autoRefactorIssuePayload}
autoRefactorIssueFromDiagnostic Diagnostic {diagnosticSpan = UnhelpfulDiagnosticSpan {}} =
  Nothing

mapMaybeToList :: (a -> Maybe b) -> [a] -> [b]
mapMaybeToList f =
  foldr
    (\value acc -> maybe acc (: acc) (f value))
    []
