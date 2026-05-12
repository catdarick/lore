{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Lore.Internal.AutoRefactor.ImportCleanupEdit
  ( planImportCleanupEdits,
  )
where

import Control.Applicative ((<|>))
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import Lore.Diagnostics (Span (..))
import Lore.Internal.AutoRefactor.Edit (FileEdit (ReplaceSpanEdit))
import Lore.Internal.AutoRefactor.ImportDecl (ImportId, ImportItem (..), ImportList (..), ParsedImport (..))
import Lore.Internal.AutoRefactor.ImportItemRemoval (applyRemovalTargets)
import Lore.Internal.AutoRefactor.ImportOps (ImportOperation (..))
import Lore.Internal.SourceText (offsetToPosition, spanText, spanToOffsets)

planImportCleanupEdits ::
  FilePath ->
  Text ->
  [ParsedImport] ->
  [ImportOperation] ->
  ([FileEdit], [String])
planImportCleanupEdits filePath source parsedImports operations =
  foldl' planForImport ([], []) parsedImports
  where
    operationsByImportId = groupOperationsByImportId operations

    planForImport (accEdits, accLogs) parsedImport =
      let importOperations =
            Map.findWithDefault [] parsedImport.parsedImportId operationsByImportId
          (newEdits, newLogs) =
            planParsedImportCleanup filePath source parsedImport importOperations
       in (accEdits <> newEdits, accLogs <> newLogs)

groupOperationsByImportId :: [ImportOperation] -> Map.Map ImportId [ImportOperation]
groupOperationsByImportId =
  foldl' insertOperation Map.empty
  where
    insertOperation operationsByImport = \case
      RemoveImportItem importId target ->
        Map.insertWith (<>) importId [RemoveImportItem importId target] operationsByImport
      RemoveWholeImport importId ->
        Map.insertWith (<>) importId [RemoveWholeImport importId] operationsByImport

planParsedImportCleanup ::
  FilePath ->
  Text ->
  ParsedImport ->
  [ImportOperation] ->
  ([FileEdit], [String])
planParsedImportCleanup filePath source parsedImport importOperations
  | null importOperations =
      ([], [])
  | hasRemoveWholeImport importOperations =
      removeWholeImportEdit filePath source parsedImport
  | otherwise =
      case parsedImport.parsedImportList of
        ExplicitImport originalItems ->
          let removalTargets =
                [ itemText
                | RemoveImportItem _ itemText <- importOperations
                ]
              finalItems =
                catMaybes (map (applyRemovalTargets removalTargets) originalItems)
           in if finalItems == originalItems
                then ([], [])
                else case finalItems of
                  [] ->
                    removeWholeImportEdit filePath source parsedImport
                  _ ->
                    rewriteExplicitImportItems filePath source parsedImport originalItems finalItems
        _ ->
          ([], [])

hasRemoveWholeImport :: [ImportOperation] -> Bool
hasRemoveWholeImport =
  any isRemoveWholeImport
  where
    isRemoveWholeImport = \case
      RemoveWholeImport {} -> True
      _ -> False

removeWholeImportEdit ::
  FilePath ->
  Text ->
  ParsedImport ->
  ([FileEdit], [String])
removeWholeImportEdit filePath source parsedImport =
  let deleteSpan =
        extendToEndOfLineIncludingNewline
          source
          parsedImport.parsedImportSpan {spanStartCol = 1}
   in ( [ReplaceSpanEdit filePath deleteSpan ""],
        ["Auto-refact: removed redundant import " <> show parsedImport.parsedImportModuleName]
      )

rewriteExplicitImportItems ::
  FilePath ->
  Text ->
  ParsedImport ->
  [ImportItem] ->
  [ImportItem] ->
  ([FileEdit], [String])
rewriteExplicitImportItems filePath source parsedImport originalItems finalItems =
  if not (allImportItemsHaveSpans originalItems)
    then
      ( [],
        [ "Auto-refact: skipped redundant import cleanup for "
            <> show parsedImport.parsedImportModuleName
            <> " because not all import item spans were available."
        ]
      )
    else case importItemsPayloadSpan source originalItems of
      Nothing ->
        ([], ["Auto-refact: skipped redundant import cleanup because import item span was unavailable."])
      Just payloadSpan ->
        let originalPayload = spanText source payloadSpan
         in if importPayloadHasComments originalPayload
              then
                ( [],
                  [ "Auto-refact: skipped redundant import cleanup for "
                      <> show parsedImport.parsedImportModuleName
                      <> " because the import list contains comments."
                  ]
                )
              else
                let replacementText = renderReplacementImportItems source payloadSpan finalItems
                 in if replacementText == originalPayload
                      then ([], [])
                      else
                        ( [ReplaceSpanEdit filePath payloadSpan replacementText],
                          [ "Auto-refact: removed redundant bindings from "
                              <> show parsedImport.parsedImportModuleName
                          ]
                        )

allImportItemsHaveSpans :: [ImportItem] -> Bool
allImportItemsHaveSpans =
  all hasSpan
  where
    hasSpan = \case
      ImportItem {importItemSpan = Just _} -> True
      ImportItem {importItemSpan = Nothing} -> False

importItemsPayloadSpan :: Text -> [ImportItem] -> Maybe Span
importItemsPayloadSpan source importItems = do
  firstItem <- firstWithSpan importItems
  lastItem <- lastWithSpan importItems
  let baseSpan =
        Span
          { spanFile = firstItem.spanFile,
            spanStartLine = firstItem.spanStartLine,
            spanStartCol = firstItem.spanStartCol,
            spanEndLine = lastItem.spanEndLine,
            spanEndCol = lastItem.spanEndCol
          }
  pure (extendSpanEndOverTrailingComma source baseSpan)

firstWithSpan :: [ImportItem] -> Maybe Span
firstWithSpan =
  foldr
    (\item acc -> item.importItemSpan <|> acc)
    Nothing

lastWithSpan :: [ImportItem] -> Maybe Span
lastWithSpan =
  foldl'
    (\acc item -> item.importItemSpan <|> acc)
    Nothing

extendSpanEndOverTrailingComma :: Text -> Span -> Span
extendSpanEndOverTrailingComma source span' =
  case spanToOffsets source span' of
    Nothing ->
      span'
    Just (_, endOffset) ->
      let after = T.drop endOffset source
          horizontalSpaces = T.takeWhile (\ch -> ch == ' ' || ch == '\t') after
          afterSpaces = T.drop (T.length horizontalSpaces) after
       in if T.isPrefixOf "," afterSpaces
            then case offsetToPosition source (endOffset + T.length horizontalSpaces + 1) of
              Just (newEndLine, newEndCol) ->
                span' {spanEndLine = newEndLine, spanEndCol = newEndCol}
              Nothing ->
                span'
            else span'

renderReplacementImportItems :: Text -> Span -> [ImportItem] -> Text
renderReplacementImportItems _ payloadSpan importItems =
  let renderedItems = map (.importItemText) importItems
   in if payloadSpan.spanStartLine == payloadSpan.spanEndLine
        then T.intercalate ", " renderedItems
        else
          let continuationIndent = T.replicate (max 0 (payloadSpan.spanStartCol - 1)) " "
           in T.intercalate (",\n" <> continuationIndent) renderedItems

importPayloadHasComments :: Text -> Bool
importPayloadHasComments payload =
  "--" `T.isInfixOf` payload || "{-" `T.isInfixOf` payload

extendToEndOfLineIncludingNewline :: Text -> Span -> Span
extendToEndOfLineIncludingNewline source span' =
  case spanToOffsets source span' of
    Nothing ->
      span'
    Just (_, endOffset) ->
      let after = T.drop endOffset source
          extraLength =
            case T.findIndex (== '\n') after of
              Nothing -> T.length (T.takeWhile (\ch -> ch == ' ' || ch == '\t') after)
              Just newlineIndex -> newlineIndex + 1
       in case offsetToPosition source (endOffset + extraLength) of
            Just (newEndLine, newEndCol) ->
              span' {spanEndLine = newEndLine, spanEndCol = newEndCol}
            Nothing ->
              span'
