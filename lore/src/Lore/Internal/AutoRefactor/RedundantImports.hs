module Lore.Internal.AutoRefactor.RedundantImports
  ( ImportNamespace (..),
    RedundantImportedOccurrence (..),
    RedundantImportIssue (..),
    redundantImportIssueFromDiagnostic,
  )
where

import Lore.Internal.ImportCleanup.Diagnostics (redundantImportIssueFromDiagnostic)
import Lore.Internal.ImportCleanup.Types
  ( ImportNamespace (..),
    RedundantImportIssue (..),
    RedundantImportedOccurrence (..),
  )
