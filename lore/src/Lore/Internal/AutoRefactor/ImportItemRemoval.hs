{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Lore.Internal.AutoRefactor.ImportItemRemoval
  ( removeTargetFromImportItem,
    applyRemovalTargets,
    diagnosticBindingTextToRemovalTargets,
    parseParentImportItem,
    normalizeImportItemText,
    normalizedFlatBindingText,
  )
where

import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Lore.Internal.AutoRefactor.ImportDecl (ImportItem (..))
import Lore.Internal.AutoRefactor.ImportOps
  ( ImportRemovalTarget (..),
    NormalizedImportItem,
    mkFlatRemovalTarget,
    mkNormalizedImportItem,
    mkWholeImportItemTarget,
    unNormalizedImportItem,
  )
import Lore.Internal.SourceSpan.Types (Span)

removeTargetFromImportItem :: ImportRemovalTarget -> ImportItem -> Maybe ImportItem
removeTargetFromImportItem target item =
  case target of
    RemoveFlatBinding targetBinding
      | targetBinding == normalizeImportItemText item.importItemText ->
          Nothing
      | otherwise ->
          case parseParentImportItem item.importItemText of
            Just (ParentImportItem itemParent itemCoverage) ->
              removeFromParentByFlatBinding targetBinding item itemParent itemCoverage
            Nothing ->
              keepUnchangedFlatImportItem target item
    RemoveWholeImportItem targetItem ->
      if targetItem == normalizeImportItemText item.importItemText
        then Nothing
        else Just item
    RemoveParentChild targetParent targetBinding ->
      case parseParentImportItem item.importItemText of
        Just (ParentImportItem itemParent itemCoverage) ->
          removeFromParentByScopedTarget targetParent targetBinding item itemParent itemCoverage
        Nothing ->
          Just item

applyRemovalTargets :: NonEmpty ImportRemovalTarget -> ImportItem -> Maybe ImportItem
applyRemovalTargets targets item =
  foldl' applyTarget (Just item) (NE.toList targets)
  where
    applyTarget maybeCurrentItem target =
      maybeCurrentItem >>= removeTargetFromImportItem target

diagnosticBindingTextToRemovalTargets :: Text -> NonEmpty ImportRemovalTarget
diagnosticBindingTextToRemovalTargets bindingText =
  case parseParentImportItem bindingText of
    Just (ParentImportItem parent (ParentChildren children))
      | not (Set.null children) ->
          case Set.toAscList children of
            [singleChild] ->
              mkScopedTarget parent singleChild
            _ ->
              mkWholeImportItemTarget bindingText :| []
    Just (ParentImportItem parent ParentAllChildren) ->
      mkWholeImportItemTarget parent :| []
    Just (ParentImportItem parent ParentOnly) ->
      mkWholeImportItemTarget parent :| []
    _ ->
      flatTarget bindingText

flatTarget :: Text -> NonEmpty ImportRemovalTarget
flatTarget bindingText =
  mkFlatRemovalTarget bindingText :| []

mkScopedTarget :: Text -> Text -> NonEmpty ImportRemovalTarget
mkScopedTarget parent child =
  RemoveParentChild
    (normalizeImportItemText parent)
    (normalizeImportItemText child)
    :| []

keepUnchangedFlatImportItem :: ImportRemovalTarget -> ImportItem -> Maybe ImportItem
keepUnchangedFlatImportItem _target item =
  Just item

removeFromParentByFlatBinding :: NormalizedImportItem -> ImportItem -> Text -> ParentImportCoverage -> Maybe ImportItem
removeFromParentByFlatBinding targetBinding item itemParent = \case
  ParentOnly ->
    if targetBinding == normalizeImportItemText itemParent
      then Nothing
      else Just item
  ParentAllChildren ->
    if targetBinding == normalizeImportItemText itemParent
      then Nothing
      else Just item
  ParentChildren itemChildren ->
    if targetBinding == normalizeImportItemText itemParent
      then Nothing
      else
        removeMatchingChild targetBinding item itemParent itemChildren

removeFromParentByScopedTarget :: NormalizedImportItem -> NormalizedImportItem -> ImportItem -> Text -> ParentImportCoverage -> Maybe ImportItem
removeFromParentByScopedTarget targetParent targetBinding item itemParent = \case
  ParentOnly ->
    Just item
  ParentAllChildren ->
    Just item
  ParentChildren itemChildren ->
    if targetParent == normalizeImportItemText itemParent
      then removeMatchingChild targetBinding item itemParent itemChildren
      else Just item

removeMatchingChild :: NormalizedImportItem -> ImportItem -> Text -> Set.Set Text -> Maybe ImportItem
removeMatchingChild targetBinding item itemParent itemChildren =
  let normalizedChildren = Set.map normalizeImportItemText itemChildren
      remainingChildren =
        Set.filter ((/= targetBinding) . normalizeImportItemText) itemChildren
   in if targetBinding `Set.member` normalizedChildren
        then
          if Set.null remainingChildren
            then Nothing
            else Just (renderParentImportItem itemParent (ParentChildren remainingChildren) item.importItemSpan)
        else Just item

normalizedFlatBindingText :: Text -> Text
normalizedFlatBindingText =
  unNormalizedImportItem . mkNormalizedImportItem

normalizeImportItemText :: Text -> NormalizedImportItem
normalizeImportItemText =
  mkNormalizedImportItem

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
