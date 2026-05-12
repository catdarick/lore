{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Lore.Internal.AutoRefactor.ImportItemRemoval
  ( removeTargetFromImportItem,
    applyRemovalTargets,
    parseParentImportItem,
    normalizedFlatBindingText,
  )
where

import Data.List (foldl')
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Lore.Diagnostics (Span)
import Lore.Internal.AutoRefactor.ImportDecl (ImportItem (..))

removeTargetFromImportItem :: Text -> ImportItem -> Maybe ImportItem
removeTargetFromImportItem targetText item
  | normalizedFlatBindingText targetText == normalizedFlatBindingText item.importItemText =
      Nothing
  | otherwise =
      case parseParentImportItem item.importItemText of
        Just (ParentImportItem itemParent itemCoverage) ->
          removeFromParentImportItem targetText item itemParent itemCoverage
        Nothing ->
          removeTargetChildFromFlatImportItem targetText item

applyRemovalTargets :: [Text] -> ImportItem -> Maybe ImportItem
applyRemovalTargets targets item =
  foldl' applyTarget (Just item) targets
  where
    applyTarget maybeCurrentItem target =
      maybeCurrentItem >>= removeTargetFromImportItem target

removeTargetChildFromFlatImportItem :: Text -> ImportItem -> Maybe ImportItem
removeTargetChildFromFlatImportItem targetText item =
  case parseParentImportItem targetText of
    Just (ParentImportItem _ (ParentChildren targetChildren))
      | normalizedFlatBindingText item.importItemText `Set.member` Set.map normalizedFlatBindingText targetChildren ->
          Nothing
    _ ->
      Just item

removeFromParentImportItem :: Text -> ImportItem -> Text -> ParentImportCoverage -> Maybe ImportItem
removeFromParentImportItem targetText item itemParent = \case
  ParentOnly ->
    Just item
  ParentAllChildren
    | normalizedFlatBindingText targetText == normalizedFlatBindingText itemParent ->
        Nothing
    | otherwise ->
        Just item
  ParentChildren itemChildren ->
    let normalizedTarget = normalizedFlatBindingText targetText
        normalizedChildren = Set.map normalizedFlatBindingText itemChildren
        remainingChildren = Set.filter ((/= normalizedTarget) . normalizedFlatBindingText) itemChildren
     in if normalizedTarget `Set.member` normalizedChildren
          then
            if Set.null remainingChildren
              then Nothing
              else Just (renderParentImportItem itemParent (ParentChildren remainingChildren) item.importItemSpan)
          else Just item

normalizedFlatBindingText :: Text -> Text
normalizedFlatBindingText rawText =
  unwrapOperatorParens . stripPatternKeyword . T.strip $ rawText
  where
    stripPatternKeyword text =
      maybe text T.strip (T.stripPrefix "pattern " text)

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

data ParentImportItem
  = ParentImportItem Text ParentImportCoverage

data ParentImportCoverage
  = ParentOnly
  | ParentAllChildren
  | ParentChildren (Set.Set Text)

parseParentImportItem :: Text -> Maybe ParentImportItem
parseParentImportItem itemText
  | T.isPrefixOf "pattern " itemText = Nothing
  | T.isPrefixOf "(" itemText = Nothing
  | otherwise =
      case T.breakOn "(" itemText of
        (parent, "")
          | isParentImportName parent ->
              Just (ParentImportItem parent ParentOnly)
        (parent, suffix)
          | isParentImportName parent,
            Just membersText <- T.stripSuffix ")" (T.drop 1 suffix) ->
              Just
                ( ParentImportItem
                    parent
                    (parseParentMembers membersText)
                )
        _ ->
          Nothing

parseParentMembers :: Text -> ParentImportCoverage
parseParentMembers membersText
  | T.strip membersText == ".." =
      ParentAllChildren
  | otherwise =
      ParentChildren (Set.fromList (map T.strip (T.splitOn "," membersText)))

isParentImportName :: Text -> Bool
isParentImportName parent =
  case T.uncons (T.strip parent) of
    Just (firstChar, _) -> firstChar >= 'A' && firstChar <= 'Z'
    Nothing -> False

renderParentImportItem :: Text -> ParentImportCoverage -> Maybe Span -> ImportItem
renderParentImportItem parent coverage importItemSpan =
  ImportItem
    { importItemText =
        case coverage of
          ParentOnly ->
            parent
          ParentAllChildren ->
            parent <> "(..)"
          ParentChildren children ->
            parent <> "(" <> T.intercalate ", " (Set.toAscList children) <> ")",
      importItemSpan
    }
