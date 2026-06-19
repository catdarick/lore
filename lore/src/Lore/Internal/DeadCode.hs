module Lore.Internal.DeadCode
  ( DeadCodeOptions (..),
    DeadDefinition (..),
    DeadDefinitionKind (..),
    DeadCodeResult (..),
    findDeadCode,
  )
where

import Control.DeepSeq (NFData (..))
import Control.Monad (filterM)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC
import GHC.Generics (Generic)
import qualified GHC.Plugins as GHC
import qualified GHC.Types.TyThing as GHC
import Lore.Internal.Definition.ProjectIndex
  ( ProjectDefinitionIndex (..),
    dependenciesForDeclaration,
    loadProjectDefinitionIndex,
  )
import Lore.Internal.Definition.Reachability (reachableDeclarationIds)
import Lore.Internal.Definition.Types (DeclarationSpans (..), DefinitionCatalog (..), DefinitionId, DefinitionSource (..), definitionSourceModule)
import Lore.Internal.HomeModules.EntryModules
  ( ComponentEntryModule (..),
    collectLoadedComponentModuleInfoWithDiagnostics,
  )
import Lore.Internal.Package (ComponentKind (..))
import Lore.Monad (MonadLore)
import Lore.SourceSpan (realSrcSpanFromSrcSpan)
import UnliftIO (evaluateDeep)

data DeadCodeOptions = DeadCodeOptions
  { deadCodeTargetModules :: Maybe (Set.Set GHC.Module),
    deadCodeAliveModules :: Set.Set GHC.Module,
    deadCodeAliveNames :: Set.Set GHC.Name
  }

data DeadDefinitionKind
  = SafeDeleteDeadDefinition
  | TestOnlyDeadDefinition
  deriving stock (Eq, Generic)

data DeadDefinition = DeadDefinition
  { deadDefinitionKind :: DeadDefinitionKind,
    deadDefinitionSource :: DefinitionSource,
    deadDefinitionNames :: Set.Set GHC.Name
  }
  deriving stock (Eq, Generic)

data DeadCodeResult = DeadCodeResult
  { deadCodeTotalDefinitions :: Int,
    deadCodeAliveDefinitions :: Int,
    deadCodeDeadDefinitions :: [DeadDefinition],
    deadCodeWarnings :: [Text]
  }
  deriving stock (Eq, Generic)

instance NFData DeadDefinitionKind

instance NFData DeadDefinition where
  rnf DeadDefinition {deadDefinitionKind, deadDefinitionSource, deadDefinitionNames} =
    rnf deadDefinitionKind `seq`
      rnf deadDefinitionSource `seq`
        rnf deadDefinitionNames

instance NFData DeadCodeResult where
  rnf DeadCodeResult {deadCodeTotalDefinitions, deadCodeAliveDefinitions, deadCodeDeadDefinitions, deadCodeWarnings} =
    rnf deadCodeTotalDefinitions `seq`
      rnf deadCodeAliveDefinitions `seq`
        rnf deadCodeDeadDefinitions `seq`
          rnf deadCodeWarnings

findDeadCode :: (MonadLore m) => DeadCodeOptions -> m DeadCodeResult
findDeadCode options = do
  projectIndex <- loadProjectDefinitionIndex
  (moduleKindsByModule, componentEntries, entryModuleDiagnostics) <-
    collectLoadedComponentModuleInfoWithDiagnostics
  let (nonTestMainRoots, testMainRoots) =
        collectMainRootsByKind projectIndex componentEntries
      definitionSourcesById =
        projectIndex.projectDefinitionCatalog.definitionSourcesById
      aliveOptionRoots =
        aliveModuleRoots options projectIndex <> aliveNameRoots options projectIndex
      reachableFromNonTestRoots =
        reachableDeclarationIds projectIndex (nonTestMainRoots <> aliveOptionRoots)
      reachableFromTestRoots =
        reachableDeclarationIds projectIndex (testMainRoots <> aliveOptionRoots)
      testOnlyModules =
        Set.fromList
          [ module_
          | (module_, componentKinds) <- Map.toList moduleKindsByModule,
            isTestOnlyComponentModule componentKinds
          ]
      aliveByReachability =
        Set.fromList
          [ definitionId
          | (definitionId, source) <- Map.toList definitionSourcesById,
            definitionIsAlive testOnlyModules source definitionId reachableFromNonTestRoots reachableFromTestRoots
          ]
      aliveInstanceDefinitionIds =
        aliveInstanceDefinitionsByHeadTypes
          projectIndex.projectInstanceHeadTypeDefinitionIdsByInstance
          aliveByReachability
      instanceDefinitionIds =
        Map.keysSet projectIndex.projectInstanceHeadTypeDefinitionIdsByInstance
      aliveDefinitionIds =
        (aliveByReachability `Set.difference` instanceDefinitionIds)
          <> aliveInstanceDefinitionIds
      allDefinitionIds =
        Map.keysSet definitionSourcesById
  deadDefinitions <-
    collectDeadDefinitions
      options
      projectIndex
      aliveDefinitionIds
      reachableFromNonTestRoots
      reachableFromTestRoots
  let result =
        DeadCodeResult
          { deadCodeTotalDefinitions = Set.size allDefinitionIds,
            deadCodeAliveDefinitions = Set.size aliveDefinitionIds,
            deadCodeDeadDefinitions = deadDefinitions,
            deadCodeWarnings = map T.pack entryModuleDiagnostics
          }
  evaluateDeep result

aliveInstanceDefinitionsByHeadTypes ::
  Map.Map DefinitionId (Set.Set DefinitionId) ->
  Set.Set DefinitionId ->
  Set.Set DefinitionId
aliveInstanceDefinitionsByHeadTypes instanceHeadTypeDefinitionIdsByInstance aliveDefinitionIds =
  Set.fromList
    [ instanceDefinitionId
    | (instanceDefinitionId, headTypeDefinitionIds) <- Map.toList instanceHeadTypeDefinitionIdsByInstance,
      Set.null headTypeDefinitionIds
        || not (Set.null (Set.intersection aliveDefinitionIds headTypeDefinitionIds))
    ]

collectMainRootsByKind ::
  ProjectDefinitionIndex ->
  [ComponentEntryModule] ->
  (Set.Set DefinitionId, Set.Set DefinitionId)
collectMainRootsByKind projectIndex componentEntries =
  foldr collectRoots (Set.empty, Set.empty) componentEntries
  where
    mainDefinitionIdsByModule =
      buildMainDefinitionIdsByModule projectIndex

    collectRoots ComponentEntryModule {entryComponentKind, entryModule} (nonTestRoots, testRoots) =
      let mainRootIds =
            Map.findWithDefault Set.empty entryModule mainDefinitionIdsByModule
       in case entryComponentKind of
            ComponentKindTest ->
              (nonTestRoots, testRoots <> mainRootIds)
            ComponentKindExecutable ->
              (nonTestRoots <> mainRootIds, testRoots)
            ComponentKindBenchmark ->
              (nonTestRoots <> mainRootIds, testRoots)
            ComponentKindLibrary ->
              (nonTestRoots, testRoots)
            ComponentKindInternalLibrary ->
              (nonTestRoots, testRoots)

buildMainDefinitionIdsByModule ::
  ProjectDefinitionIndex ->
  Map.Map GHC.Module (Set.Set DefinitionId)
buildMainDefinitionIdsByModule projectIndex =
  Map.fromListWith
    Set.union
    [ (definitionSourceModule source, Set.singleton definitionId)
    | (definitionId, source) <- Map.toList projectIndex.projectDefinitionCatalog.definitionSourcesById,
      any ((== "main") . GHC.getOccString) (Set.toList source.definitionSourceNames)
    ]

isTestOnlyComponentModule :: Set.Set ComponentKind -> Bool
isTestOnlyComponentModule componentKinds =
  Set.member ComponentKindTest componentKinds
    && Set.null (Set.delete ComponentKindTest componentKinds)

definitionIsAlive ::
  Set.Set GHC.Module ->
  DefinitionSource ->
  DefinitionId ->
  Set.Set DefinitionId ->
  Set.Set DefinitionId ->
  Bool
-- Hybrid test semantics:
-- - test-only modules are considered alive when reachable from test roots
-- - non-test modules are considered alive only when reachable from non-test roots
definitionIsAlive testOnlyModules definitionSource definitionId reachableFromNonTestRoots reachableFromTestRoots =
  if definitionSourceModule definitionSource `Set.member` testOnlyModules
    then definitionId `Set.member` reachableFromTestRoots
    else definitionId `Set.member` reachableFromNonTestRoots

aliveModuleRoots ::
  DeadCodeOptions ->
  ProjectDefinitionIndex ->
  Set.Set DefinitionId
aliveModuleRoots options projectIndex =
  Set.fromList
    [ definitionId
    | (definitionId, source) <- Map.toList projectIndex.projectDefinitionCatalog.definitionSourcesById,
      definitionSourceModule source `Set.member` options.deadCodeAliveModules
    ]

aliveNameRoots ::
  DeadCodeOptions ->
  ProjectDefinitionIndex ->
  Set.Set DefinitionId
aliveNameRoots options projectIndex =
  Set.fromList
    [ definitionId
    | name <- Set.toList options.deadCodeAliveNames,
      Just definitionId <- [Map.lookup name projectIndex.projectDefinitionCatalog.definitionIdsByName]
    ]

collectDeadDefinitions ::
  (MonadLore m) =>
  DeadCodeOptions ->
  ProjectDefinitionIndex ->
  Set.Set DefinitionId ->
  Set.Set DefinitionId ->
  Set.Set DefinitionId ->
  m [DeadDefinition]
collectDeadDefinitions options projectIndex aliveDefinitionIds reachableFromNonTestRoots reachableFromTestRoots = do
  safeDeleteDeadDefinitions <-
    orderedDeadDefinitions SafeDeleteDeadDefinition safeDeleteDeadDefinitionIds
  testOnlyDeadDefinitions <-
    orderedDeadDefinitions TestOnlyDeadDefinition testOnlyDeadDefinitionIds
  pure (safeDeleteDeadDefinitions <> testOnlyDeadDefinitions)
  where
    (safeDeleteDeadDefinitionIds, testOnlyDeadDefinitionIds) =
      Map.foldlWithKey' collectDefinition (Set.empty, Set.empty) projectIndex.projectDefinitionCatalog.definitionSourcesById

    collectDefinition (safeIds, testOnlyIds) definitionId source
      | Set.member definitionId aliveDefinitionIds =
          (safeIds, testOnlyIds)
      | not (isReportableDefinition source) =
          (safeIds, testOnlyIds)
      | definitionIsReachableOnlyFromTests definitionId =
          (safeIds, Set.insert definitionId testOnlyIds)
      | otherwise =
          (Set.insert definitionId safeIds, testOnlyIds)

    isReportableDefinition source =
      case options.deadCodeTargetModules of
        Nothing ->
          True
        Just targetModules ->
          definitionSourceModule source `Set.member` targetModules

    definitionIsReachableOnlyFromTests definitionId =
      Set.member definitionId reachableFromTestRoots
        && Set.notMember definitionId reachableFromNonTestRoots

    orderedDeadDefinitions kind definitionIds =
      mapM
        (mkDeadDefinition kind)
        [ source
        | definitionId <- safeDeleteOrderedDefinitionIds projectIndex definitionIds,
          Just source <- [Map.lookup definitionId projectIndex.projectDefinitionCatalog.definitionSourcesById]
        ]

    mkDeadDefinition kind source = do
      deadDefinitionNames <- definitionRootNames source
      pure
        DeadDefinition
          { deadDefinitionKind = kind,
            deadDefinitionSource = source,
            deadDefinitionNames
          }

definitionRootNames ::
  (MonadLore m) =>
  DefinitionSource ->
  m (Set.Set GHC.Name)
definitionRootNames source
  | Set.size source.definitionSourceNames <= 1 =
      pure source.definitionSourceNames
  | otherwise = do
      rootNames <- filterM isRootName (Set.toList source.definitionSourceNames)
      pure (Set.fromList rootNames)
  where
    definitionNames =
      source.definitionSourceNames

    isRootName name = do
      parentNames <- definitionNameParentNames name
      pure (Set.null (Set.intersection definitionNames parentNames))

definitionNameParentNames ::
  (MonadLore m) =>
  GHC.Name ->
  m (Set.Set GHC.Name)
definitionNameParentNames name = do
  maybeTyThing <- GHC.lookupName name
  pure $
    case maybeTyThing of
      Nothing ->
        Set.empty
      Just tyThing ->
        Set.fromList (map GHC.getName (tyThingParentChain tyThing))
  where
    tyThingParentChain tyThing =
      case GHC.tyThingParent_maybe tyThing of
        Nothing ->
          []
        Just parentTyThing ->
          parentTyThing : tyThingParentChain parentTyThing

safeDeleteOrderedDefinitionIds ::
  ProjectDefinitionIndex ->
  Set.Set DefinitionId ->
  [DefinitionId]
safeDeleteOrderedDefinitionIds projectIndex definitionIds =
  go initialRemaining initialReady initialDependentCounts []
  where
    sourceOrderedDefinitionIds =
      sortOn (definitionIdSortKey projectIndex) (Set.toList definitionIds)
    rankByDefinitionId =
      Map.fromList (zip sourceOrderedDefinitionIds [0 :: Int ..])
    rankedDefinitionId definitionId =
      (Map.findWithDefault maxBound definitionId rankByDefinitionId, definitionId)
    dependenciesById =
      Map.fromSet dependenciesWithinDeadDefinitions definitionIds
    dependenciesWithinDeadDefinitions definitionId =
      dependenciesForDeclaration projectIndex definitionId `Set.intersection` definitionIds
    initialDependentCounts =
      Map.foldl' countDependents (Map.fromSet (const (0 :: Int)) definitionIds) dependenciesById
    countDependents dependentCounts dependencies =
      Set.foldl'
        (\counts dependencyId -> Map.adjust (+ 1) dependencyId counts)
        dependentCounts
        dependencies
    initialRemaining =
      Set.fromList (map rankedDefinitionId sourceOrderedDefinitionIds)
    initialReady =
      Set.filter
        (\(_, definitionId) -> Map.findWithDefault 0 definitionId initialDependentCounts == 0)
        initialRemaining

    go remaining ready dependentCounts ordered
      | Set.null remaining =
          reverse ordered
      | otherwise =
          let selected@(_, selectedDefinitionId) =
                case Set.minView ready of
                  Just (readyDefinitionId, _) ->
                    readyDefinitionId
                  Nothing ->
                    case Set.minView remaining of
                      Just (cycleDefinitionId, _) ->
                        cycleDefinitionId
                      Nothing ->
                        error "safeDeleteOrderedDefinitionIds: impossible empty remaining set"
              remainingWithoutSelected =
                Set.delete selected remaining
              readyWithoutSelected =
                Set.delete selected ready
              (dependentCounts', ready') =
                unblockDependencies
                  selectedDefinitionId
                  remainingWithoutSelected
                  dependentCounts
                  readyWithoutSelected
           in go remainingWithoutSelected ready' dependentCounts' (selectedDefinitionId : ordered)

    unblockDependencies selectedDefinitionId remaining dependentCounts ready =
      Set.foldl' unblockOne (dependentCounts, ready) (Map.findWithDefault Set.empty selectedDefinitionId dependenciesById)
      where
        unblockOne (counts, readyDefinitions) dependencyId =
          let nextCount =
                max (0 :: Int) (Map.findWithDefault 0 dependencyId counts - 1)
              counts' =
                Map.insert dependencyId nextCount counts
              rankedDependencyId =
                rankedDefinitionId dependencyId
              readyDefinitions' =
                if nextCount == 0 && Set.member rankedDependencyId remaining
                  then Set.insert rankedDependencyId readyDefinitions
                  else readyDefinitions
           in (counts', readyDefinitions')

definitionIdSortKey ::
  ProjectDefinitionIndex ->
  DefinitionId ->
  (String, String, Int, Int, [String])
definitionIdSortKey projectIndex definitionId =
  case Map.lookup definitionId projectIndex.projectDefinitionCatalog.definitionSourcesById of
    Just source ->
      definitionSourceSortKey source source.definitionSourceNames
    Nothing ->
      ("", "", maxBound, maxBound, [])

definitionSourceSortKey ::
  DefinitionSource ->
  Set.Set GHC.Name ->
  (String, String, Int, Int, [String])
definitionSourceSortKey source names =
  ( moduleNameKey,
    sourceFileKey,
    sourceLineKey,
    sourceColumnKey,
    nameKeys
  )
  where
    moduleNameKey =
      GHC.moduleNameString (GHC.moduleName (definitionSourceModule source))
    nameKeys =
      map GHC.getOccString (Set.toAscList names)
    (sourceFileKey, sourceLineKey, sourceColumnKey) =
      case realSrcSpanFromSrcSpan source.definitionSourceSpans.declarationSpan of
        Just realSrcSpan ->
          ( GHC.unpackFS (GHC.srcSpanFile realSrcSpan),
            GHC.srcSpanStartLine realSrcSpan,
            GHC.srcSpanStartCol realSrcSpan
          )
        Nothing ->
          ("", maxBound, maxBound)
