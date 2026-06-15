{-# LANGUAGE CPP #-}

module Lore.Internal.Definition.SourceTree
  ( collectModuleSourceRegionCandidates,
    buildDefinitionSourceTree,
    chooseBestReferenceContext,
    nestSourceRegions,
    flattenSourceRegions,
  )
where

import qualified Data.IntMap.Strict as IntMap
import Data.List (minimumBy, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified GHC
import qualified GHC.Plugins as GHC
import Lore.Internal.Definition.Analysis.Common (collectTyped)
import Lore.Internal.Definition.Types
  ( DeclarationSpans (..),
    DefinitionSourceTree (..),
    SourceRegion (..),
    SourceRegionCandidate (..),
    SourceRegionKind (..),
  )
import Lore.Internal.SourceSpan (srcSpanSize, srcSpanSortKey)

collectModuleSourceRegionCandidates :: GHC.ParsedSource -> [SourceRegionCandidate]
collectModuleSourceRegionCandidates parsedSource =
  dedupeRegionCandidates $
    map (SourceRegionCandidate MatchRegion . locatedASpan) (collectTyped parsedSource :: [GHC.LMatch GHC.GhcPs (GHC.LHsExpr GHC.GhcPs)])
      <> map (SourceRegionCandidate GuardRegion . grhsSpan) (collectTyped parsedSource :: [GHC.LGRHS GHC.GhcPs (GHC.LHsExpr GHC.GhcPs)])
      <> map (SourceRegionCandidate StatementRegion . locatedASpan) (collectTyped parsedSource :: [GHC.LStmt GHC.GhcPs (GHC.LHsExpr GHC.GhcPs)])
      <> map (SourceRegionCandidate BindingRegion . locatedASpan) (collectTyped parsedSource :: [GHC.LHsBind GHC.GhcPs])
      <> mapMaybe expressionRegionCandidate (collectTyped parsedSource :: [GHC.LocatedA (GHC.HsExpr GHC.GhcPs)])
  where
    expressionRegionCandidate expression@(GHC.L _ expr) = do
      regionKind <- sourceRegionKindForExpression expr
      pure (SourceRegionCandidate regionKind (locatedASpan expression))

    dedupeRegionCandidates =
      Map.elems
        . Map.fromList
        . map (\candidate -> ((candidate.candidateRegionKind, srcSpanSortKey candidate.candidateRegionSpan), candidate))
        . filter hasRealSpan

    hasRealSpan candidate =
      case GHC.srcSpanToRealSrcSpan candidate.candidateRegionSpan of
        Nothing -> False
        Just _ -> True

buildDefinitionSourceTree :: DeclarationSpans -> [SourceRegionCandidate] -> SourceRegion
buildDefinitionSourceTree spans moduleRegionCandidates =
  SourceRegion
    { sourceRegionKind = DefinitionRegion,
      sourceRegionSpan = spans.declarationSpan,
      sourceRegionChildren = nestSourceRegions regionCandidates
    }
  where
    targetSpans =
      [spans.declarationSpan]

    regionCandidates =
      filter usefulCandidate moduleRegionCandidates

    usefulCandidate candidate =
      spanWithin targetSpans candidate.candidateRegionSpan
        && candidate.candidateRegionSpan /= spans.declarationSpan

chooseBestReferenceContext :: DefinitionSourceTree -> GHC.SrcSpan -> Maybe GHC.SrcSpan
chooseBestReferenceContext sourceTree referenceExactSpan =
  sourceRegionSpan
    <$> case sortOn regionRank containingRegions of
      bestRegion : _ -> Just bestRegion
      [] -> Nothing
  where
    containingRegions =
      [ region
      | region <- flattenSourceRegions sourceTree.sourceTreeRoot,
        referenceExactSpan `GHC.isSubspanOf` region.sourceRegionSpan,
        show referenceExactSpan /= show region.sourceRegionSpan
      ]

    regionRank region =
      ( sourceRegionContextRank region,
        sourceRegionPreferredLineSpan region,
        sourceRegionPreferredColumnSpan region,
        regionKindRank region.sourceRegionKind,
        show region.sourceRegionSpan
      )

flattenSourceRegions :: SourceRegion -> [SourceRegion]
flattenSourceRegions region =
  region : concatMap flattenSourceRegions region.sourceRegionChildren

regionKindRank :: SourceRegionKind -> Int
regionKindRank = \case
  ApplicationRegion -> 0
  RecordRegion -> 0
  StatementRegion -> 1
  GuardRegion -> 2
  MatchRegion -> 3
  BindingRegion -> 4
  DefinitionRegion -> 5

sourceRegionLineSpan :: SourceRegion -> Int
sourceRegionLineSpan region =
  case GHC.srcSpanToRealSrcSpan region.sourceRegionSpan of
    Nothing -> maxBound
    Just realSpan ->
      GHC.srcSpanEndLine realSpan - GHC.srcSpanStartLine realSpan

sourceRegionColumnSpan :: SourceRegion -> Int
sourceRegionColumnSpan region =
  case GHC.srcSpanToRealSrcSpan region.sourceRegionSpan of
    Nothing -> maxBound
    Just realSpan ->
      GHC.srcSpanEndCol realSpan - GHC.srcSpanStartCol realSpan

sourceRegionPreferredLineSpan :: SourceRegion -> Int
sourceRegionPreferredLineSpan region
  | sourceRegionContextRank region == 0 =
      negate (sourceRegionLineSpan region)
  | otherwise =
      sourceRegionLineSpan region

sourceRegionPreferredColumnSpan :: SourceRegion -> Int
sourceRegionPreferredColumnSpan region
  | sourceRegionContextRank region == 0 =
      negate (sourceRegionColumnSpan region)
  | otherwise =
      sourceRegionColumnSpan region

sourceRegionContextRank :: SourceRegion -> Int
sourceRegionContextRank region =
  case region.sourceRegionKind of
    ApplicationRegion | sourceRegionLineSpan region > 0 -> 0
    RecordRegion | sourceRegionLineSpan region > 0 -> 0
    DefinitionRegion -> 2
    BindingRegion -> 1
    _ -> 1

{- ORMOLU_DISABLE -}
sourceRegionKindForExpression :: GHC.HsExpr GHC.GhcPs -> Maybe SourceRegionKind
sourceRegionKindForExpression = \case
  GHC.RecordCon {} -> Just RecordRegion
  GHC.RecordUpd {} -> Just RecordRegion
  GHC.HsApp {} -> Just ApplicationRegion
  GHC.HsAppType {} -> Just ApplicationRegion
  GHC.OpApp {} -> Just ApplicationRegion
  GHC.NegApp {} -> Just ApplicationRegion
#if MIN_VERSION_ghc(9,10,0)
  GHC.HsPar _ expression -> sourceRegionKindForExpression (GHC.unLoc expression)
#else
  GHC.HsPar _ _ expression _ -> sourceRegionKindForExpression (GHC.unLoc expression)
#endif
  _ -> Nothing
{- ORMOLU_ENABLE -}

nestSourceRegions :: [SourceRegionCandidate] -> [SourceRegion]
nestSourceRegions candidates =
  mapMaybe toRegion (IntMap.findWithDefault [] rootParentId childrenByParent)
  where
    rootParentId =
      -1

    sortedCandidates =
      zip [0 :: Int ..] (sortOn candidateSortKey candidates)

    candidatesById =
      IntMap.fromList sortedCandidates

    parentById =
      IntMap.fromList
        [ (candidateId, maybe rootParentId id (parentOf candidateId candidate))
        | (candidateId, candidate) <- sortedCandidates
        ]

    childrenByParent =
      IntMap.fromListWith
        (<>)
        [ (parentId, [candidateId])
        | (candidateId, parentId) <- IntMap.toList parentById
        ]

    toRegion candidateId =
      do
        candidate <- IntMap.lookup candidateId candidatesById
        pure
          SourceRegion
            { sourceRegionKind = candidate.candidateRegionKind,
              sourceRegionSpan = candidate.candidateRegionSpan,
              sourceRegionChildren = mapMaybe toRegion (IntMap.findWithDefault [] candidateId childrenByParent)
            }

    parentOf candidateId candidate =
      case containingParents of
        [] -> Nothing
        parents ->
          Just . fst $
            minimumBy
              (\(_, left) (_, right) -> compare (candidateSpanSize left) (candidateSpanSize right))
              parents
      where
        containingParents =
          [ (parentId, parent)
          | (parentId, parent) <- sortedCandidates,
            parentId /= candidateId,
            candidate.candidateRegionSpan `properlyContainedBy` parent.candidateRegionSpan
          ]

candidateSortKey :: SourceRegionCandidate -> ((String, Int, Int, Int, Int), Int, SourceRegionKind)
candidateSortKey candidate =
  (srcSpanSortKey candidate.candidateRegionSpan, candidateSpanSize candidate, candidate.candidateRegionKind)

candidateSpanSize :: SourceRegionCandidate -> Int
candidateSpanSize =
  srcSpanSize . candidateRegionSpan

properlyContainedBy :: GHC.SrcSpan -> GHC.SrcSpan -> Bool
properlyContainedBy child parent =
  child `GHC.isSubspanOf` parent && show child /= show parent

spanWithin :: [GHC.SrcSpan] -> GHC.SrcSpan -> Bool
spanWithin targetSpans span' =
  any (span' `GHC.isSubspanOf`) targetSpans

locatedASpan :: GHC.LocatedA a -> GHC.SrcSpan
locatedASpan = GHC.getLocA

grhsSpan :: GHC.LGRHS GHC.GhcPs (GHC.LHsExpr GHC.GhcPs) -> GHC.SrcSpan
grhsSpan = GHC.locA . GHC.getLoc
