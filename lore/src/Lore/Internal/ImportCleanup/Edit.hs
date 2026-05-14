{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Lore.Internal.ImportCleanup.Edit
  ( renderImportCleanupEdits,
  )
where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Lore.Internal.ImportCleanup.Rewrite (cleanupImportListPayloadOccurrences)
import Lore.Internal.ImportCleanup.SourceSlice
  ( SourceSlice (..),
    findFirstBalancedParensRange,
    includeTrailingNewline,
    lineEndOffsetFrom,
    lineStartOffsetAt,
    rangeToSpan,
    replaceRange,
    sliceRange,
    spanToRange,
  )
import Lore.Internal.ImportCleanup.Types
  ( ImportCleanupAction (..),
    ImportCleanupWarning (..),
    ImportId,
    ParsedImport (..),
    ParsedImportListKind (..),
    PlannedFileEdit (..),
    RedundantImportedOccurrence,
    SourceRange (..),
  )
import Lore.Internal.SourceEdit (FileEdit (ReplaceSpanEditExpected), positionToOffset)
import Lore.Internal.SourceSpan.Types (Span (..))
import Lore.Internal.SourceText (offsetToPosition, spanTextMaybe)

data ExplicitImportListSlice = ExplicitImportListSlice
  { explicitListPayloadRange :: SourceRange,
    explicitListPayloadText :: Text
  }
  deriving (Eq, Show)

data ImportDeclView = ImportDeclView
  { importDeclParsedImport :: ParsedImport,
    importDeclSpan :: Span,
    importDeclText :: Text,
    importDeclLineEndOffset :: Int,
    importDeclBeforeTextOnLine :: Text,
    importDeclAfterTextOnLine :: Text,
    importDeclLineText :: Text
  }
  deriving (Eq, Show)

data ImportDeclViewPurpose
  = ForWholeImportDeletion
  | ForImportListRewrite

renderImportCleanupEdits ::
  FilePath ->
  Text ->
  Map.Map ImportId ImportCleanupAction ->
  ([PlannedFileEdit], [ImportCleanupWarning])
renderImportCleanupEdits filePath source actions =
  foldMap (renderAction filePath source) (Map.elems actions)

renderAction ::
  FilePath ->
  Text ->
  ImportCleanupAction ->
  ([PlannedFileEdit], [ImportCleanupWarning])
renderAction filePath source action =
  case action of
    DeleteImport parsedImport ->
      case buildImportDeclView source parsedImport ForWholeImportDeletion of
        Left warning ->
          ([], [warning])
        Right importDeclView ->
          case deleteWholeImportEdit filePath source importDeclView of
            Left warning ->
              ([], [warning])
            Right edit ->
              ([edit], [])
    RemoveImportOccurrences parsedImport occurrences ->
      case buildImportDeclView source parsedImport ForImportListRewrite of
        Left warning ->
          ([], [warning])
        Right importDeclView ->
          case rewriteImportDeclarationEdit filePath importDeclView occurrences of
            Left warning ->
              ([], [warning])
            Right edit ->
              ([edit], [])

rewriteImportDeclarationEdit ::
  FilePath ->
  ImportDeclView ->
  NonEmpty RedundantImportedOccurrence ->
  Either ImportCleanupWarning PlannedFileEdit
rewriteImportDeclarationEdit filePath importDeclView occurrences = do
  let parsedImport = importDeclView.importDeclParsedImport
      importId = parsedImport.parsedImportId
  ensureOr
    (not (hasCommentToken importDeclView.importDeclText || hasCommentToken importDeclView.importDeclAfterTextOnLine))
    (ImportDeclarationContainsComments importId)
  explicitListSlice <-
    extractExplicitImportListSlice importDeclView.importDeclText parsedImport
  rewrittenPayload <-
    cleanupImportListPayloadOccurrences
      importId
      explicitListSlice.explicitListPayloadText
      (NE.toList occurrences)
  newImportDeclSource <-
    maybe
      (Left (ImportRewriteProducedInvalidSource importId))
      Right
      ( replaceRange
          importDeclView.importDeclText
          explicitListSlice.explicitListPayloadRange
          rewrittenPayload
      )
  Right
    PlannedFileEdit
      { plannedFileEdit =
          ReplaceSpanEditExpected
            filePath
            importDeclView.importDeclSpan
            importDeclView.importDeclText
            newImportDeclSource,
        plannedFileEditSummary =
          "Auto-refactor: cleaned redundant import occurrences from "
            <> show parsedImport.parsedImportModuleName
      }

extractExplicitImportListSlice ::
  Text ->
  ParsedImport ->
  Either ImportCleanupWarning ExplicitImportListSlice
extractExplicitImportListSlice importDeclSource parsedImport =
  case parsedImportListKind parsedImport of
    ParsedExplicitImport ->
      case findFirstBalancedParensRange importDeclSource of
        Nothing ->
          Left (ImportListParseFailed (parsedImportId parsedImport) "failed to locate explicit import list")
        Just fullRange ->
          let payloadRange =
                SourceRange
                  { rangeStart = fullRange.rangeStart + 1,
                    rangeEnd = fullRange.rangeEnd - 1
                  }
           in case sliceRange importDeclSource payloadRange of
                Nothing ->
                  Left (ImportListParseFailed (parsedImportId parsedImport) "failed to slice explicit import-list payload")
                Just payloadSlice ->
                  Right
                    ExplicitImportListSlice
                      { explicitListPayloadRange = payloadRange,
                        explicitListPayloadText = sourceSliceText payloadSlice
                      }
    ParsedOpenImport ->
      Left (ImportListRequiredForItemCleanup (parsedImportId parsedImport))
    ParsedHidingImport ->
      Left (HidingImportItemCleanupUnsupported (parsedImportId parsedImport))

deleteWholeImportEdit ::
  FilePath ->
  Text ->
  ImportDeclView ->
  Either ImportCleanupWarning PlannedFileEdit
deleteWholeImportEdit filePath source importDeclView = do
  let parsedImport = importDeclView.importDeclParsedImport
      importId = parsedImport.parsedImportId
      importHeadText = T.stripStart importDeclView.importDeclText
      startsWithImportKeyword = T.isPrefixOf "import " importHeadText
      deleteEndOffset = includeTrailingNewline source importDeclView.importDeclLineEndOffset
  ensureOr startsWithImportKeyword (ImportDeclarationUnsafeForWholeDeletion importId)
  ensureOr
    (T.all isHorizontalSpace importDeclView.importDeclBeforeTextOnLine)
    (ImportDeclarationUnsafeForWholeDeletion importId)
  ensureOr
    (not (hasCommentToken importDeclView.importDeclLineText))
    (ImportDeclarationContainsComments importId)
  ensureOr
    (not (";" `T.isInfixOf` importDeclView.importDeclText))
    (ImportDeclarationUnsafeForWholeDeletion importId)
  ensureOr
    (T.all isHorizontalSpace importDeclView.importDeclAfterTextOnLine)
    (ImportDeclarationUnsafeForWholeDeletion importId)

  (endLine, endCol) <-
    maybe
      (Left (ImportDeclarationUnsafeForWholeDeletion importId))
      Right
      (offsetToPosition source deleteEndOffset)

  let deleteSpan =
        Span
          { spanFile = spanFile importDeclView.importDeclSpan,
            spanStartLine = spanStartLine importDeclView.importDeclSpan,
            spanStartCol = 1,
            spanEndLine = endLine,
            spanEndCol = endCol
          }

  expectedText <-
    maybe
      (Left (ImportDeclarationUnsafeForWholeDeletion importId))
      Right
      (spanTextMaybe source deleteSpan)

  Right
    PlannedFileEdit
      { plannedFileEdit = ReplaceSpanEditExpected filePath deleteSpan expectedText "",
        plannedFileEditSummary =
          "Auto-refactor: removed redundant import " <> show parsedImport.parsedImportModuleName
      }

buildImportDeclView ::
  Text ->
  ParsedImport ->
  ImportDeclViewPurpose ->
  Either ImportCleanupWarning ImportDeclView
buildImportDeclView source parsedImport purpose = do
  importDeclRange <-
    extractImportDeclarationRange source parsedImport (mkBuildWarning purpose parsedImport "invalid import declaration range")
  importDeclSlice <-
    maybe
      (Left (mkBuildWarning purpose parsedImport "missing import declaration source"))
      Right
      (sliceRange source importDeclRange)
  importDeclSpan <-
    maybe
      (Left (mkBuildWarning purpose parsedImport "invalid import declaration span"))
      Right
      (rangeToSpan source (spanFile parsedImport.parsedImportSpan) importDeclRange)
  importDeclLineStartOffset <-
    maybe
      (Left (mkBuildWarning purpose parsedImport "invalid import declaration line offset"))
      Right
      (lineStartOffsetAt source (spanStartLine parsedImport.parsedImportSpan))
  let importDeclLineEndOffset =
        lineEndOffsetFrom source importDeclRange.rangeEnd
      importDeclBeforeTextOnLine =
        T.take
          (importDeclRange.rangeStart - importDeclLineStartOffset)
          (T.drop importDeclLineStartOffset source)
      importDeclAfterTextOnLine =
        T.take
          (importDeclLineEndOffset - importDeclRange.rangeEnd)
          (T.drop importDeclRange.rangeEnd source)
      importDeclLineText =
        T.take
          (importDeclLineEndOffset - importDeclLineStartOffset)
          (T.drop importDeclLineStartOffset source)
  pure
    ImportDeclView
      { importDeclParsedImport = parsedImport,
        importDeclSpan,
        importDeclText = sourceSliceText importDeclSlice,
        importDeclLineEndOffset,
        importDeclBeforeTextOnLine,
        importDeclAfterTextOnLine,
        importDeclLineText
      }

extractImportDeclarationRange ::
  Text ->
  ParsedImport ->
  ImportCleanupWarning ->
  Either ImportCleanupWarning SourceRange
extractImportDeclarationRange source parsedImport warning = do
  importHintRange <-
    maybe
      (Left warning)
      Right
      (spanToRange source (parsedImportSpan parsedImport))
  importStartOffset <-
    maybe
      (Left warning)
      Right
      (positionToOffset source (spanStartLine (parsedImportSpan parsedImport), 1))
  let importEndOffset =
        case parsedImportListKind parsedImport of
          ParsedOpenImport ->
            rangeEnd importHintRange
          ParsedExplicitImport ->
            scanImportDeclarationEnd source importStartOffset
          ParsedHidingImport ->
            scanImportDeclarationEnd source importStartOffset
  Right
    SourceRange
      { rangeStart = importStartOffset,
        rangeEnd = importEndOffset
      }

scanImportDeclarationEnd :: Text -> Int -> Int
scanImportDeclarationEnd source startOffset =
  go startOffset 0 False
  where
    sourceLength = T.length source

    go :: Int -> Int -> Bool -> Int
    go offset depth seenOpenParen
      | offset >= sourceLength =
          sourceLength
      | otherwise =
          case T.index source offset of
            '(' ->
              go (offset + 1) (depth + 1) True
            ')' ->
              go (offset + 1) (max 0 (depth - 1)) seenOpenParen
            '\n'
              | depth == 0 && seenOpenParen ->
                  offset
              | otherwise ->
                  go (offset + 1) depth seenOpenParen
            _ ->
              go (offset + 1) depth seenOpenParen

hasCommentToken :: Text -> Bool
hasCommentToken text =
  "--" `T.isInfixOf` text || "{-" `T.isInfixOf` text

isHorizontalSpace :: Char -> Bool
isHorizontalSpace char =
  char == ' ' || char == '\t'

mkBuildWarning :: ImportDeclViewPurpose -> ParsedImport -> Text -> ImportCleanupWarning
mkBuildWarning purpose parsedImport message =
  case purpose of
    ForWholeImportDeletion ->
      ImportDeclarationUnsafeForWholeDeletion parsedImport.parsedImportId
    ForImportListRewrite ->
      ImportListParseFailed parsedImport.parsedImportId message

ensureOr :: Bool -> ImportCleanupWarning -> Either ImportCleanupWarning ()
ensureOr condition warning =
  if condition
    then Right ()
    else Left warning
