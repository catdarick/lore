{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Lore.Internal.AutoRefactor.ImportNormalize
  ( applyImportOperations,
    normalizeImports,
  )
where

import Data.List (find, foldl', groupBy, partition, sortOn)
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Lore.Diagnostics (Span)
import Lore.Internal.AutoRefactor.ImportDecl
  ( ImportId,
    ImportItem (..),
    ImportList (..),
    NormalizedImport (..),
    QualifiedImportStyle (..),
  )
import Lore.Internal.AutoRefactor.ImportOps (ImportOperation (..))

applyImportOperations :: [NormalizedImport] -> [ImportOperation] -> ([NormalizedImport], [String])
applyImportOperations imports operations =
  normalizeImports $
    foldl' applyOne (imports, []) operations
  where
    applyOne (currentImports, logs) operation =
      let (updatedImports, newLogs) = applyOperation currentImports operation
       in (updatedImports, logs <> newLogs)

applyOperation :: [NormalizedImport] -> ImportOperation -> ([NormalizedImport], [String])
applyOperation imports = \case
  AddUnqualifiedItem moduleName itemText ->
    ensureUnqualifiedItem moduleName itemText imports
  EnsureUnqualifiedOpenImport moduleName ->
    ensureUnqualifiedOpenImport moduleName imports
  AddUnqualifiedItemToExistingImport moduleName itemText ->
    ensureExistingUnqualifiedItem moduleName itemText imports
  EnsureQualifiedImport moduleName qualifier ->
    ensureQualifiedImport moduleName qualifier imports
  RemoveImportItem importId itemText ->
    removeImportItem importId itemText imports
  RemoveWholeImport importId ->
    removeWholeImport importId imports

ensureUnqualifiedItem :: Text -> Text -> [NormalizedImport] -> ([NormalizedImport], [String])
ensureUnqualifiedItem moduleName itemText imports =
  case find isCompatible imports of
    Just normalizedImport ->
      case normalizedImport.normalizedImportList of
        OpenImport ->
          (imports, [])
        ExplicitImport items ->
          let updatedItems = appendUniqueImportItems items [ImportItem itemText Nothing]
           in if updatedItems == items
                then (imports, [])
                else
                  ( replaceImport normalizedImport normalizedImport {normalizedImportList = ExplicitImport updatedItems} imports,
                    ["Auto-refact: extended existing import list for " <> show moduleName]
                  )
        HidingImport {} ->
          insertNewImport (newExplicitImport moduleName itemText) imports
    Nothing ->
      insertNewImport (newExplicitImport moduleName itemText) imports
  where
    isCompatible normalizedImport =
      normalizedImport.normalizedImportModuleName == moduleName
        && normalizedImport.normalizedImportQualifiedStyle == ImportUnqualified
        && not (isHidingImport normalizedImport)

ensureExistingUnqualifiedItem :: Text -> Text -> [NormalizedImport] -> ([NormalizedImport], [String])
ensureExistingUnqualifiedItem moduleName itemText imports =
  case find isCompatible imports of
    Just normalizedImport ->
      case normalizedImport.normalizedImportList of
        OpenImport ->
          (imports, [])
        ExplicitImport items ->
          let updatedItems = appendUniqueImportItems items [ImportItem itemText Nothing]
           in if updatedItems == items
                then (imports, [])
                else
                  ( replaceImport normalizedImport normalizedImport {normalizedImportList = ExplicitImport updatedItems} imports,
                    ["Auto-refact: extended existing import list for " <> show moduleName]
                  )
        HidingImport {} ->
          (imports, [])
    Nothing ->
      (imports, [])
  where
    isCompatible normalizedImport =
      normalizedImport.normalizedImportModuleName == moduleName
        && normalizedImport.normalizedImportQualifiedStyle == ImportUnqualified
        && not (isHidingImport normalizedImport)

ensureUnqualifiedOpenImport :: Text -> [NormalizedImport] -> ([NormalizedImport], [String])
ensureUnqualifiedOpenImport moduleName imports =
  case find isCompatible imports of
    Just normalizedImport ->
      case normalizedImport.normalizedImportList of
        OpenImport ->
          (imports, [])
        ExplicitImport _ ->
          ( replaceImport normalizedImport normalizedImport {normalizedImportList = OpenImport} imports,
            ["Auto-refact: opened unqualified import for " <> show moduleName]
          )
        HidingImport {} ->
          insertNewImport (newOpenUnqualifiedImport moduleName) imports
    Nothing ->
      insertNewImport (newOpenUnqualifiedImport moduleName) imports
  where
    isCompatible normalizedImport =
      normalizedImport.normalizedImportModuleName == moduleName
        && normalizedImport.normalizedImportQualifiedStyle == ImportUnqualified
        && not (isHidingImport normalizedImport)

ensureQualifiedImport :: Text -> Text -> [NormalizedImport] -> ([NormalizedImport], [String])
ensureQualifiedImport moduleName qualifier imports =
  case find isCompatible imports of
    Just normalizedImport ->
      case normalizedImport.normalizedImportList of
        OpenImport ->
          (imports, [])
        ExplicitImport _ ->
          ( replaceImport normalizedImport normalizedImport {normalizedImportList = OpenImport} imports,
            ["Auto-refact: opened qualified import for " <> show moduleName]
          )
        HidingImport {} ->
          insertNewImport (newQualifiedImport moduleName qualifier) imports
    Nothing ->
      insertNewImport (newQualifiedImport moduleName qualifier) imports
  where
    isCompatible normalizedImport =
      normalizedImport.normalizedImportModuleName == moduleName
        && normalizedImport.normalizedImportQualifiedStyle /= ImportUnqualified
        && normalizedImport.normalizedImportAlias == Just qualifier
        && not (isHidingImport normalizedImport)

removeImportItem :: ImportId -> Text -> [NormalizedImport] -> ([NormalizedImport], [String])
removeImportItem importId itemText imports =
  case find ((== Just importId) . normalizedImportId) imports of
    Nothing ->
      (imports, [])
    Just normalizedImport ->
      case normalizedImport.normalizedImportList of
        ExplicitImport items ->
          let updatedItems =
                concatMap (updateImportItem itemText) items
           in if updatedItems == items
                then (imports, [])
                else case updatedItems of
                  [] ->
                    ( filter ((/= Just importId) . normalizedImportId) imports,
                      [ "Auto-refact: removed redundant import "
                          <> show normalizedImport.normalizedImportModuleName
                      ]
                    )
                  _ ->
                    ( replaceImport normalizedImport normalizedImport {normalizedImportList = ExplicitImport updatedItems} imports,
                      [ "Auto-refact: removed redundant binding "
                          <> show itemText
                          <> " from "
                          <> show normalizedImport.normalizedImportModuleName
                      ]
                    )
        _ ->
          (imports, [])

updateImportItem :: Text -> ImportItem -> [ImportItem]
updateImportItem targetText item =
  case removeTargetFromImportItem targetText item of
    Nothing -> []
    Just updatedItem -> [updatedItem]

removeTargetFromImportItem :: Text -> ImportItem -> Maybe ImportItem
removeTargetFromImportItem targetText item
  | normalizedFlatBindingText targetText == normalizedFlatBindingText item.importItemText =
      Nothing
  | otherwise =
      case parseParentImportItem item.importItemText of
        Just (ParentImportItem itemParent itemCoverage) ->
          removeFromParentImportItem targetText item itemParent itemCoverage
        Just PlainImportItem ->
          removeTargetChildFromFlatImportItem targetText item
        Nothing ->
          removeTargetChildFromFlatImportItem targetText item

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

removeWholeImport :: ImportId -> [NormalizedImport] -> ([NormalizedImport], [String])
removeWholeImport importId imports =
  case find ((== Just importId) . normalizedImportId) imports of
    Nothing ->
      (imports, [])
    Just normalizedImport ->
      ( filter ((/= Just importId) . normalizedImportId) imports,
        ["Auto-refact: removed redundant import " <> show normalizedImport.normalizedImportModuleName]
      )

normalizeImports :: ([NormalizedImport], [String]) -> ([NormalizedImport], [String])
normalizeImports (imports, logs) =
  let (hidingImports, mergeableImports) = partition isHidingImport imports
      (mergedImports, mergeLogs) = mergeImports mergeableImports
   in (sortOn normalizedImportOrder (mergedImports <> hidingImports), logs <> mergeLogs)

mergeImports :: [NormalizedImport] -> ([NormalizedImport], [String])
mergeImports imports =
  foldl' mergeGroup ([], []) groupedImports
  where
    groupedImports =
      groupBy sameMergeKey $
        sortOn mergeSortKey imports

    mergeSortKey normalizedImport =
      ( mergeKey normalizedImport,
        normalizedImport.normalizedImportOrder
      )

    sameMergeKey left right =
      mergeKey left == mergeKey right

mergeGroup :: ([NormalizedImport], [String]) -> [NormalizedImport] -> ([NormalizedImport], [String])
mergeGroup (accImports, accLogs) = \case
  [] ->
    (accImports, accLogs)
  [normalizedImport] ->
    (normalizedImport : accImports, accLogs)
  groupedImports ->
    let representative = minimumByOrder groupedImports
        mergedImport =
          case find ((== OpenImport) . normalizedImportList) groupedImports of
            Just openImport ->
              representative {normalizedImportList = openImport.normalizedImportList}
            Nothing ->
              representative
                { normalizedImportList =
                    ExplicitImport $
                      foldl'
                        appendUniqueImportItems
                        []
                        [ items
                        | ExplicitImport items <- map (.normalizedImportList) groupedImports
                        ]
                }
        mergeLog =
          "Auto-refact: merged duplicate imports for " <> show representative.normalizedImportModuleName
     in (mergedImport : accImports, mergeLog : accLogs)

mergeKey ::
  NormalizedImport ->
  ( Text,
    QualifiedImportStyle,
    Maybe Text,
    Bool,
    Bool,
    Maybe Text
  )
mergeKey normalizedImport =
  ( normalizedImport.normalizedImportModuleName,
    normalizedImport.normalizedImportQualifiedStyle,
    normalizedImport.normalizedImportAlias,
    normalizedImport.normalizedImportSource,
    normalizedImport.normalizedImportSafe,
    normalizedImport.normalizedImportPackageQualifier
  )

minimumByOrder :: [NormalizedImport] -> NormalizedImport
minimumByOrder =
  foldl1 pickEarlier
  where
    pickEarlier left right
      | left.normalizedImportOrder <= right.normalizedImportOrder = left
      | otherwise = right

replaceImport :: NormalizedImport -> NormalizedImport -> [NormalizedImport] -> [NormalizedImport]
replaceImport original updated =
  map \normalizedImport ->
    if normalizedImport.normalizedImportId == original.normalizedImportId
      then updated
      else normalizedImport

insertNewImport :: NormalizedImport -> [NormalizedImport] -> ([NormalizedImport], [String])
insertNewImport newImport imports =
  (imports <> [newImport], ["Auto-refact: inserted import " <> show newImport.normalizedImportModuleName])

newExplicitImport :: Text -> Text -> NormalizedImport
newExplicitImport moduleName itemText =
  NormalizedImport
    { normalizedImportId = Nothing,
      normalizedImportOrder = 1_000_000,
      normalizedImportSpan = Nothing,
      normalizedImportModuleName = moduleName,
      normalizedImportQualifiedStyle = ImportUnqualified,
      normalizedImportAlias = Nothing,
      normalizedImportSource = False,
      normalizedImportSafe = False,
      normalizedImportPackageQualifier = Nothing,
      normalizedImportList = ExplicitImport [ImportItem itemText Nothing]
    }

newOpenUnqualifiedImport :: Text -> NormalizedImport
newOpenUnqualifiedImport moduleName =
  NormalizedImport
    { normalizedImportId = Nothing,
      normalizedImportOrder = 1_000_000,
      normalizedImportSpan = Nothing,
      normalizedImportModuleName = moduleName,
      normalizedImportQualifiedStyle = ImportUnqualified,
      normalizedImportAlias = Nothing,
      normalizedImportSource = False,
      normalizedImportSafe = False,
      normalizedImportPackageQualifier = Nothing,
      normalizedImportList = OpenImport
    }

newQualifiedImport :: Text -> Text -> NormalizedImport
newQualifiedImport moduleName qualifier =
  NormalizedImport
    { normalizedImportId = Nothing,
      normalizedImportOrder = 1_000_000,
      normalizedImportSpan = Nothing,
      normalizedImportModuleName = moduleName,
      normalizedImportQualifiedStyle = ImportQualifiedPrefix,
      normalizedImportAlias = Just qualifier,
      normalizedImportSource = False,
      normalizedImportSafe = False,
      normalizedImportPackageQualifier = Nothing,
      normalizedImportList = OpenImport
    }

appendUniqueImportItems :: [ImportItem] -> [ImportItem] -> [ImportItem]
appendUniqueImportItems existing additions =
  foldl' appendItem existing additions
  where
    appendItem acc item
      | any ((== item.importItemText) . importItemText) acc = acc
      | otherwise =
          case classifyImportItem item of
            PlainImportItem ->
              acc <> [item]
            ParentImportItem parent newCoverage ->
              mergeParentImportItem parent newCoverage item acc

mergeParentImportItem :: Text -> ParentImportCoverage -> ImportItem -> [ImportItem] -> [ImportItem]
mergeParentImportItem parent newCoverage newItem items =
  let (sameParentItems, otherItems) =
        partition
          (\item -> itemParentMatches parent item)
          items
   in case foldl' mergeCoverage Nothing (mapMaybe parentImportCoverage sameParentItems) of
        Just existingCoverage
          | existingCoverage `coversCoverage` newCoverage ->
              items
          | otherwise ->
              let mergedCoverage = mergeCoverages existingCoverage newCoverage
               in otherItems <> [renderParentImportItem parent mergedCoverage newItem.importItemSpan]
        Nothing ->
          otherItems <> [newItem]

data ClassifiedImportItem
  = PlainImportItem
  | ParentImportItem Text ParentImportCoverage

data ParentImportCoverage
  = ParentOnly
  | ParentAllChildren
  | ParentChildren (Set.Set Text)

classifyImportItem :: ImportItem -> ClassifiedImportItem
classifyImportItem item =
  case parseParentImportItem item.importItemText of
    Just classifiedItem -> classifiedItem
    Nothing -> PlainImportItem

parseParentImportItem :: Text -> Maybe ClassifiedImportItem
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

itemParentMatches :: Text -> ImportItem -> Bool
itemParentMatches parent item =
  case parseParentImportItem item.importItemText of
    Just (ParentImportItem itemParent _) -> itemParent == parent
    _ -> False

parentImportCoverage :: ImportItem -> Maybe ParentImportCoverage
parentImportCoverage item =
  case parseParentImportItem item.importItemText of
    Just (ParentImportItem _ coverage) -> Just coverage
    _ -> Nothing

mergeCoverage :: Maybe ParentImportCoverage -> ParentImportCoverage -> Maybe ParentImportCoverage
mergeCoverage Nothing coverage =
  Just coverage
mergeCoverage (Just left) right =
  Just (mergeCoverages left right)

mergeCoverages :: ParentImportCoverage -> ParentImportCoverage -> ParentImportCoverage
mergeCoverages left right =
  case (left, right) of
    (ParentAllChildren, _) ->
      ParentAllChildren
    (_, ParentAllChildren) ->
      ParentAllChildren
    (ParentOnly, coverage) ->
      coverage
    (coverage, ParentOnly) ->
      coverage
    (ParentChildren leftChildren, ParentChildren rightChildren) ->
      ParentChildren (leftChildren `Set.union` rightChildren)

coversCoverage :: ParentImportCoverage -> ParentImportCoverage -> Bool
coversCoverage left right =
  case (left, right) of
    (ParentAllChildren, _) ->
      True
    (ParentChildren _, ParentOnly) ->
      True
    (ParentChildren leftChildren, ParentChildren rightChildren) ->
      rightChildren `Set.isSubsetOf` leftChildren
    (ParentOnly, ParentOnly) ->
      True
    _ ->
      False

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

isHidingImport :: NormalizedImport -> Bool
isHidingImport normalizedImport =
  case normalizedImport.normalizedImportList of
    HidingImport {} -> True
    _ -> False
