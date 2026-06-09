module Lore.Internal.Definition.Reachability
  ( walkReachable,
    reachableNamedTargets,
    reachableDeclarationIds,
  )
where

import Data.List (foldl')
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Lore.Internal.Definition.ProjectIndex
  ( DefinitionTarget,
    ProjectDefinitionIndex,
    dependenciesForDeclaration,
    dependenciesForNamedTarget,
  )
import Lore.Internal.Definition.Types (DefinitionId)

walkReachable ::
  (Ord node) =>
  Maybe Int ->
  (node -> [node]) ->
  [node] ->
  [(Int, node)]
walkReachable maybeMaxDepth neighbours roots =
  go initialVisited initialQueue []
  where
    initialRoots =
      dedupePreservingOrder roots
    initialQueue =
      Seq.fromList [(0, root) | root <- initialRoots]
    initialVisited =
      Set.fromList initialRoots

    go visited queue reached =
      case Seq.viewl queue of
        Seq.EmptyL ->
          reverse reached
        (depth, node) Seq.:< remainingQueue ->
          let shouldExpand =
                maybe True (depth <) maybeMaxDepth
              nextNodes =
                if shouldExpand
                  then filter (`Set.notMember` visited) (neighbours node)
                  else []
              visited' =
                foldr Set.insert visited nextNodes
              nextQueueItems =
                Seq.fromList [(depth + 1, nextNode) | nextNode <- nextNodes]
              queue' =
                remainingQueue Seq.>< nextQueueItems
           in go visited' queue' ((depth, node) : reached)

dedupePreservingOrder :: (Ord a) => [a] -> [a]
dedupePreservingOrder =
  reverse . snd . foldl' step (Set.empty, [])
  where
    step (seen, deduped) item
      | item `Set.member` seen =
          (seen, deduped)
      | otherwise =
          (Set.insert item seen, item : deduped)

reachableNamedTargets ::
  Int ->
  ProjectDefinitionIndex ->
  [DefinitionTarget] ->
  [(Int, DefinitionTarget)]
reachableNamedTargets maxDepth projectIndex =
  walkReachable
    (Just (max 0 maxDepth))
    (Set.toList . dependenciesForNamedTarget projectIndex)

reachableDeclarationIds ::
  ProjectDefinitionIndex ->
  Set.Set DefinitionId ->
  Set.Set DefinitionId
reachableDeclarationIds projectIndex roots =
  Set.fromList $
    map snd $
      walkReachable
        Nothing
        (Set.toList . dependenciesForDeclaration projectIndex)
        (Set.toList roots)
