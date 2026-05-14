{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Lore.Internal.ImportCleanup.Rewrite
  ( cleanupImportListPayloadOccurrences,
    normalizeImportName,
  )
where

import Control.Monad (foldM)
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import Lore.Internal.ImportCleanup.ImportListParser (parseImportListPayload)
import Lore.Internal.ImportCleanup.SourceSlice (replaceRange)
import Lore.Internal.ImportCleanup.Types
  ( ImportCleanupWarning (..),
    ImportId,
    ImportItem (..),
    ImportItemChildren (..),
    ImportList,
    ImportName (..),
    ImportNamespace (..),
    RedundantImportedOccurrence (..),
    SepItem (..),
    SepList (..),
    SourceRange (..),
    WithRange (..),
  )

data RemovalCandidate
  = RemoveTopLevelItem Int
  | RemoveChildItem Int Int
  deriving (Eq, Show)

cleanupImportListPayloadOccurrences ::
  ImportId ->
  Text ->
  [RedundantImportedOccurrence] ->
  Either ImportCleanupWarning Text
cleanupImportListPayloadOccurrences importId =
  foldM (cleanupOneOccurrence importId)

cleanupOneOccurrence ::
  ImportId ->
  Text ->
  RedundantImportedOccurrence ->
  Either ImportCleanupWarning Text
cleanupOneOccurrence importId payload occurrence = do
  parsedList <- parsePayload importId payload
  candidate <- resolveUniqueOccurrence importId parsedList occurrence
  payload' <- removeCandidate importId parsedList payload candidate
  _ <- parsePayload importId payload'
  pure payload'

parsePayload ::
  ImportId ->
  Text ->
  Either ImportCleanupWarning ImportList
parsePayload importId payload =
  either
    (\err -> Left (ImportListParseFailed importId err))
    Right
    (parseImportListPayload payload)

resolveUniqueOccurrence ::
  ImportId ->
  ImportList ->
  RedundantImportedOccurrence ->
  Either ImportCleanupWarning RemovalCandidate
resolveUniqueOccurrence importId parsedList occurrence =
  case candidates of
    [] ->
      Left (NoMatchingImportBinding importId (renderOccurrence occurrence))
    [one] ->
      Right one
    _ ->
      Left (AmbiguousImportBinding importId (renderOccurrence occurrence))
  where
    candidates = nub (collectCandidates parsedList occurrence)

collectCandidates :: ImportList -> RedundantImportedOccurrence -> [RemovalCandidate]
collectCandidates parsedList occurrence =
  concatMap (uncurry (itemCandidates occurrence)) (zip [0 ..] parsedList.sepListItems)

itemCandidates :: RedundantImportedOccurrence -> Int -> SepItem ImportItem -> [RemovalCandidate]
itemCandidates occurrence parentIndex item =
  let occurrenceName = normalizeImportName occurrence.redundantOccurrenceText
      itemValue = item.sepItemValue
      headName = normalizeImportName (unImportName itemValue.importItemHead.wrValue)
      headMatches =
        matchesNamespace occurrence.redundantOccurrenceNamespace itemValue.importItemNamespace
          && occurrenceName == headName
      explicitChildMatches =
        case itemValue.importItemChildren of
          ExplicitChildren children ->
            [ RemoveChildItem parentIndex childIndex
            | (childIndex, child) <- zip [0 ..] children.sepListItems,
              occurrence.redundantOccurrenceNamespace == Nothing,
              occurrenceName == normalizeImportName (unImportName child.sepItemValue)
            ]
          _ ->
            []
   in case itemValue.importItemChildren of
        NoImportChildren
          | headMatches ->
              [RemoveTopLevelItem parentIndex]
        NoImportChildren ->
          []
        ExplicitChildren _ ->
          explicitChildMatches
        WildcardChildren _
          | headMatches ->
              [RemoveTopLevelItem parentIndex]
        WildcardChildren _ ->
          []

removeCandidate ::
  ImportId ->
  ImportList ->
  Text ->
  RemovalCandidate ->
  Either ImportCleanupWarning Text
removeCandidate importId parsedList payload candidate =
  case candidate of
    RemoveTopLevelItem itemIndex ->
      maybe
        (Left (ImportRewriteProducedInvalidSource importId))
        Right
        (removeSepItemAt parsedList itemIndex payload)
    RemoveChildItem candidateParentIndex candidateChildIndex ->
      case parsedList.sepListItems !!? candidateParentIndex of
        Nothing ->
          Left (ImportRewriteProducedInvalidSource importId)
        Just parentItem ->
          removeChildCandidate importId payload parentItem candidateChildIndex

removeChildCandidate ::
  ImportId ->
  Text ->
  SepItem ImportItem ->
  Int ->
  Either ImportCleanupWarning Text
removeChildCandidate importId payload parentItem childIndex =
  case parentItem.sepItemValue.importItemChildren of
    ExplicitChildren children ->
      let childCount = length children.sepListItems
       in if childCount <= 1
            then
              replacePayloadRange
                importId
                parentItem.sepItemCoreRange
                (renderItemHeadOnly parentItem.sepItemValue)
                payload
            else
              maybe
                (Left (ImportRewriteProducedInvalidSource importId))
                Right
                (removeSepItemAt children childIndex payload)
    _ ->
      Right payload

renderItemHeadOnly :: ImportItem -> Text
renderItemHeadOnly item =
  let namespacePrefix =
        case item.importItemNamespace of
          Just TypeNamespace -> "type "
          Just PatternNamespace -> "pattern "
          Nothing -> ""
   in namespacePrefix <> unImportName item.importItemHead.wrValue

replacePayloadRange ::
  ImportId ->
  SourceRange ->
  Text ->
  Text ->
  Either ImportCleanupWarning Text
replacePayloadRange importId range replacement payload =
  maybe
    (Left (ImportRewriteProducedInvalidSource importId))
    Right
    (replaceRange payload range replacement)

resolveSepItemDeletionRange ::
  SepList a ->
  Int ->
  SourceRange
resolveSepItemDeletionRange sepList index =
  case sepList.sepListItems !!? index of
    Nothing ->
      sepList.sepListPayloadRange
    Just currentItem ->
      case currentItem.sepItemSeparatorAfter of
        Just separatorAfter ->
          SourceRange
            { rangeStart = currentItem.sepItemOuterRange.rangeStart,
              rangeEnd = separatorAfter.rangeEnd
            }
        Nothing ->
          case previousItemSeparator of
            Just separatorBefore ->
              SourceRange
                { rangeStart = separatorBefore.rangeStart,
                  rangeEnd = deletionEnd
                }
            Nothing ->
              SourceRange
                { rangeStart = currentItem.sepItemOuterRange.rangeStart,
                  rangeEnd = deletionEnd
                }
  where
    isLastItem = index == length sepList.sepListItems - 1

    deletionEnd =
      case sepList.sepListTrailingSeparator of
        Just trailingSeparator
          | isLastItem ->
              trailingSeparator.rangeStart + 1
        _ ->
          case sepList.sepListItems !!? index of
            Just currentItem -> currentItem.sepItemOuterRange.rangeEnd
            Nothing -> sepList.sepListPayloadRange.rangeEnd

    previousItemSeparator =
      case sepList.sepListItems !!? (index - 1) of
        Just previousItem -> previousItem.sepItemSeparatorAfter
        Nothing -> Nothing

removeSepItemAt ::
  SepList a ->
  Int ->
  Text ->
  Maybe Text
removeSepItemAt sepList index source = do
  _ <- sepList.sepListItems !!? index
  replaceRange source (resolveSepItemDeletionRange sepList index) ""

renderOccurrence :: RedundantImportedOccurrence -> Text
renderOccurrence RedundantImportedOccurrence {redundantOccurrenceText, redundantOccurrenceNamespace} =
  case redundantOccurrenceNamespace of
    Just TypeNamespace -> "type " <> redundantOccurrenceText
    Just PatternNamespace -> "pattern " <> redundantOccurrenceText
    Nothing -> redundantOccurrenceText

matchesNamespace :: Maybe ImportNamespace -> Maybe ImportNamespace -> Bool
matchesNamespace Nothing _ =
  True
matchesNamespace (Just expected) (Just actual) =
  expected == actual
matchesNamespace (Just _) Nothing =
  False

(!!?) :: [a] -> Int -> Maybe a
(!!?) values index
  | index < 0 = Nothing
  | otherwise =
      case drop index values of
        value : _ -> Just value
        [] -> Nothing

normalizeImportName :: Text -> Text
normalizeImportName rawText =
  unwrapOperatorParens (T.strip rawText)
  where
    unwrapOperatorParens text
      | T.length text >= 2,
        T.head text == '(',
        T.last text == ')',
        let inner = T.init (T.tail text),
        not (T.null inner),
        T.all (`notElem` [' ', '(', ')', ',']) inner =
          inner
      | otherwise =
          text
