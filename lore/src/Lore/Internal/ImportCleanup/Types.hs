{-# LANGUAGE NamedFieldPuns #-}

module Lore.Internal.ImportCleanup.Types
  ( ImportNamespace (..),
    RedundantImportedOccurrence (..),
    RedundantImportIssue (..),
    redundantImportIssueSpan,
    mapRedundantImportIssueSpan,
    ImportId (..),
    ParsedImportListKind (..),
    ParsedImport (..),
    SourceRange (..),
    WithRange (..),
    SepList (..),
    SepItem (..),
    ImportList,
    ChildList,
    ImportName (..),
    ImportItem (..),
    ImportItemChildren (..),
    WildcardImportChildren (..),
    ImportCleanupAction (..),
    ImportCleanupWarning (..),
    PlannedFileEdit (..),
    ImportCleanupFileReport (..),
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Lore.Internal.SourceEdit (FileEdit)
import Lore.Internal.SourceSpan.Types (Span)

data ImportNamespace
  = TypeNamespace
  | PatternNamespace
  deriving (Eq, Ord, Show)

data RedundantImportedOccurrence = RedundantImportedOccurrence
  { redundantOccurrenceText :: Text,
    redundantOccurrenceNamespace :: Maybe ImportNamespace
  }
  deriving (Eq, Ord, Show)

data RedundantImportIssue
  = RedundantWholeImportIssue Span
  | RedundantImportOccurrencesIssue Span (NonEmpty RedundantImportedOccurrence)
  deriving (Eq, Show)

redundantImportIssueSpan :: RedundantImportIssue -> Span
redundantImportIssueSpan issue =
  case issue of
    RedundantWholeImportIssue span' -> span'
    RedundantImportOccurrencesIssue span' _ -> span'

mapRedundantImportIssueSpan :: (Span -> Span) -> RedundantImportIssue -> RedundantImportIssue
mapRedundantImportIssueSpan mapSpan issue =
  case issue of
    RedundantWholeImportIssue span' ->
      RedundantWholeImportIssue (mapSpan span')
    RedundantImportOccurrencesIssue span' occurrences ->
      RedundantImportOccurrencesIssue (mapSpan span') occurrences

newtype ImportId = ImportId Int
  deriving (Eq, Ord, Show)

data ParsedImportListKind
  = ParsedOpenImport
  | ParsedExplicitImport
  | ParsedHidingImport
  deriving (Eq, Show)

data ParsedImport = ParsedImport
  { parsedImportId :: ImportId,
    parsedImportSpan :: Span,
    parsedImportModuleName :: Text,
    parsedImportListKind :: ParsedImportListKind
  }
  deriving (Eq, Show)

data SourceRange = SourceRange
  { rangeStart :: Int,
    rangeEnd :: Int
  }
  deriving (Eq, Ord, Show)

data WithRange a = WithRange
  { wrRange :: SourceRange,
    wrValue :: a
  }
  deriving (Eq, Ord, Show)

data SepList a = SepList
  { sepListPayloadRange :: SourceRange,
    sepListItems :: [SepItem a],
    sepListTrailingSeparator :: Maybe SourceRange
  }
  deriving (Eq, Ord, Show)

data SepItem a = SepItem
  { sepItemValue :: a,
    sepItemCoreRange :: SourceRange,
    sepItemOuterRange :: SourceRange,
    sepItemSeparatorAfter :: Maybe SourceRange
  }
  deriving (Eq, Ord, Show)

type ImportList = SepList ImportItem

type ChildList = SepList ImportName

newtype ImportName = ImportName
  { unImportName :: Text
  }
  deriving (Eq, Ord, Show)

data ImportItem = ImportItem
  { importItemHead :: WithRange ImportName,
    importItemNamespace :: Maybe ImportNamespace,
    importItemChildren :: ImportItemChildren,
    importItemOriginalText :: Text
  }
  deriving (Eq, Ord, Show)

data ImportItemChildren
  = NoImportChildren
  | WildcardChildren WildcardImportChildren
  | ExplicitChildren ChildList
  deriving (Eq, Ord, Show)

data WildcardImportChildren = WildcardImportChildren
  { wildcardChildrenFullRange :: SourceRange,
    wildcardChildrenRange :: SourceRange
  }
  deriving (Eq, Ord, Show)

data ImportCleanupAction
  = DeleteImport ParsedImport
  | RemoveImportOccurrences ParsedImport (NonEmpty RedundantImportedOccurrence)
  deriving (Eq, Show)

data ImportCleanupWarning
  = MissingModuleSummary FilePath
  | SourceReadFailed FilePath
  | SourceParseFailed FilePath
  | NoMatchingImportForDiagnostic Span
  | AmbiguousDiagnosticImportMatch Span
  | ImportSpanFileMismatch ImportId FilePath FilePath
  | ImportDeclarationContainsComments ImportId
  | ImportDeclarationUnsafeForWholeDeletion ImportId
  | ImportListRequiredForItemCleanup ImportId
  | HidingImportItemCleanupUnsupported ImportId
  | ImportListParseFailed ImportId Text
  | NoMatchingImportBinding ImportId Text
  | AmbiguousImportBinding ImportId Text
  | ImportRewriteProducedInvalidSource ImportId
  | StaleFileEditSpan FilePath FileEdit
  deriving (Eq, Show)

data PlannedFileEdit = PlannedFileEdit
  { plannedFileEdit :: FileEdit,
    plannedFileEditSummary :: String
  }
  deriving (Eq, Show)

data ImportCleanupFileReport = ImportCleanupFileReport
  { importCleanupFilePath :: FilePath,
    importCleanupFileEdits :: [PlannedFileEdit],
    importCleanupFileWarnings :: [ImportCleanupWarning]
  }
  deriving (Eq, Show)
