{-# LANGUAGE OverloadedStrings #-}

module Lore.Internal.AutoRefactor.RedundantImports
  ( RedundantImportRequest (..),
    redundantImportRequestFromDiagnostic,
    suggestRedundantImportOperations,
  )
where

import Control.Applicative ((<|>))
import Data.List (find, foldl', nub)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Lore.Diagnostics (Diagnostic (..), DiagnosticSpan (..))
import Lore.Internal.AutoRefactor.ImportDecl (ImportItem (..), ImportList (..), ParsedImport (..), parsedImportContainsSpan)
import Lore.Internal.AutoRefactor.ImportItemRemoval (diagnosticBindingTextToRemovalTargets)
import Lore.Internal.AutoRefactor.ImportOps (ImportOperation (..))
import Lore.Internal.SourceSpan (spansOverlap)
import Lore.Internal.SourceSpan.Types (Span (..))

data RedundantImportRequest = RedundantImportRequest
  { redundantImportDiagnosticSpan :: Span,
    redundantImportBindings :: Maybe (NonEmpty T.Text)
  }
  deriving (Eq, Show)

redundantImportRequestFromDiagnostic :: Diagnostic -> Maybe RedundantImportRequest
redundantImportRequestFromDiagnostic Diagnostic {diagnosticSpan = RealDiagnosticSpan diagnosticSpan, diagnosticMessage} =
  case parseRedundantImportDiagnostic diagnosticMessage of
    Just RemoveWholeImportDiagnostic ->
      Just
        RedundantImportRequest
          { redundantImportDiagnosticSpan = diagnosticSpan,
            redundantImportBindings = Nothing
          }
    Just (RemoveBindings bindings) ->
      Just
        RedundantImportRequest
          { redundantImportDiagnosticSpan = diagnosticSpan,
            redundantImportBindings = Just bindings
          }
    Nothing ->
      Nothing
redundantImportRequestFromDiagnostic Diagnostic {diagnosticSpan = UnhelpfulDiagnosticSpan {}} =
  Nothing

suggestRedundantImportOperations :: [ParsedImport] -> NonEmpty RedundantImportRequest -> [ImportOperation]
suggestRedundantImportOperations parsedImports requests =
  concatMap suggestForImport groupedRequests
  where
    groupedRequests =
      groupRequestsByImportId
        [ (findParsedImport request, request)
        | request <- NE.toList requests
        ]

    findParsedImport request =
      find (`parsedImportContainsSpan` request.redundantImportDiagnosticSpan) parsedImports

    suggestForImport (Nothing, _) =
      []
    suggestForImport (Just parsedImport, importRequests) =
      if any requestRemovesWholeImport importRequests
        then [RemoveWholeImport parsedImport.parsedImportId]
        else case NE.nonEmpty (nub (concatMap (requestRemovalTargets parsedImport) importRequests)) of
          Just targets ->
            [RemoveImportItems parsedImport.parsedImportId targets]
          Nothing ->
            []

    requestRemovalTargets parsedImport request =
      case request.redundantImportBindings of
        Just bindings ->
          concatMap (NE.toList . diagnosticBindingTextToRemovalTargets) (NE.toList bindings)
        Nothing ->
          case findImportItemBySpan request.redundantImportDiagnosticSpan parsedImport of
            Just importItem ->
              NE.toList (diagnosticBindingTextToRemovalTargets importItem.importItemText)
            Nothing -> []

    requestRemovesWholeImport RedundantImportRequest {redundantImportBindings} =
      case redundantImportBindings of
        Nothing -> True
        Just _ -> False

groupRequestsByImportId :: [(Maybe ParsedImport, RedundantImportRequest)] -> [(Maybe ParsedImport, [RedundantImportRequest])]
groupRequestsByImportId =
  map (\(maybeParsedImport, grouped) -> (maybeParsedImport, reverse grouped))
    . Map.elems
    . foldl' step Map.empty
  where
    step groupedRequestsByImport (maybeParsedImport, request) =
      Map.insertWith
        mergeRequests
        (fmap (.parsedImportId) maybeParsedImport)
        (maybeParsedImport, [request])
        groupedRequestsByImport

    mergeRequests (newImport, newRequests) (existingImport, existingRequests) =
      (existingImport <|> newImport, newRequests <> existingRequests)

findImportItemBySpan :: Span -> ParsedImport -> Maybe ImportItem
findImportItemBySpan diagnosticSpan parsedImport =
  find (itemSpanMatches diagnosticSpan) (importItems parsedImport.parsedImportList)

importItems :: ImportList -> [ImportItem]
importItems = \case
  OpenImport -> []
  ExplicitImport items -> items
  HidingImport items -> items

itemSpanMatches :: Span -> ImportItem -> Bool
itemSpanMatches diagnosticSpan importItem =
  case importItem.importItemSpan of
    Nothing -> False
    Just importItemSpan ->
      spansOverlap importItemSpan diagnosticSpan

data ParsedRedundantImportDiagnostic
  = RemoveBindings (NonEmpty T.Text)
  | RemoveWholeImportDiagnostic
  deriving (Eq, Show)

parseRedundantImportDiagnostic :: T.Text -> Maybe ParsedRedundantImportDiagnostic
parseRedundantImportDiagnostic rawMessage
  | " is redundant" `T.isInfixOf` message =
      parseSpecificRedundantImport message <|> parseWholeImport message
  | otherwise =
      Nothing
  where
    message = unifySpaces rawMessage

parseSpecificRedundantImport :: T.Text -> Maybe ParsedRedundantImportDiagnostic
parseSpecificRedundantImport message = do
  suffix <- T.stripPrefix "The import of " message <|> T.stripPrefix "The qualified import of " message
  quoted <- extractQuoted suffix
  afterQuoted <- snd <$> nonEmptyBreak " from module " suffix
  if " is redundant" `T.isInfixOf` afterQuoted
    then RemoveBindings <$> splitBindings quoted
    else Nothing

parseWholeImport :: T.Text -> Maybe ParsedRedundantImportDiagnostic
parseWholeImport message
  | " from module " `T.isInfixOf` message = Nothing
  | "The import of " `T.isInfixOf` message = Just RemoveWholeImportDiagnostic
  | "The qualified import of " `T.isInfixOf` message = Just RemoveWholeImportDiagnostic
  | otherwise = Nothing

extractQuoted :: T.Text -> Maybe T.Text
extractQuoted text =
  extractBetween "‘" "’" text
    <|> extractBetween "`" "'" text

extractBetween :: T.Text -> T.Text -> T.Text -> Maybe T.Text
extractBetween openQuote closeQuote text =
  case T.breakOn openQuote text of
    (_, "") -> Nothing
    (_, withOpenQuote) ->
      let afterOpenQuote = T.drop (T.length openQuote) withOpenQuote
          (quoted, afterCloseQuote) = T.breakOn closeQuote afterOpenQuote
       in if T.null afterCloseQuote
            then Nothing
            else Just quoted

nonEmptyBreak :: T.Text -> T.Text -> Maybe (T.Text, T.Text)
nonEmptyBreak needle haystack =
  let pair@(_, suffix) = T.breakOn needle haystack
   in if T.null suffix then Nothing else Just pair

unifySpaces :: T.Text -> T.Text
unifySpaces =
  T.unwords . T.words

splitBindings :: T.Text -> Maybe (NonEmpty T.Text)
splitBindings =
  NE.nonEmpty . map T.strip . splitTopLevelCommaSeparated

splitTopLevelCommaSeparated :: T.Text -> [T.Text]
splitTopLevelCommaSeparated =
  finalize . T.foldl' step ([], "", 0 :: Int)
  where
    step (chunks, currentChunk, depth) char =
      case char of
        '(' ->
          (chunks, T.snoc currentChunk char, depth + 1)
        ')' ->
          (chunks, T.snoc currentChunk char, max 0 (depth - 1))
        ','
          | depth == 0 ->
              (chunks <> [currentChunk], "", depth)
        _ ->
          (chunks, T.snoc currentChunk char, depth)

    finalize (chunks, currentChunk, _) =
      chunks <> [currentChunk]
