module Lore.Internal.DeadCode
  ( DeadCodeOptions (..),
    DeadDefinition (..),
    DeadCodeResult (..),
    findDeadCode,
  )
where

import Control.DeepSeq (NFData (..))
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified GHC.Plugins as GHC
import Lore.Internal.Definition.Graph
  ( ProjectDefinitionIndex (..),
    buildDependencyGraph,
    loadProjectDefinitionIndex,
    reachableDefinitions,
  )
import Lore.Internal.Definition.Types (DeclarationSpans (..), DefinitionId, DefinitionSource (..))
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

data DeadDefinition = DeadDefinition
  { deadDefinitionSource :: DefinitionSource,
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

instance NFData DeadDefinition where
  rnf DeadDefinition {deadDefinitionSource, deadDefinitionNames} =
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
  dependencyGraph <- buildDependencyGraph projectIndex
  let (nonTestMainRoots, testMainRoots) =
        collectMainRootsByKind projectIndex componentEntries
      aliveOptionRoots =
        aliveModuleRoots options projectIndex <> aliveNameRoots options projectIndex
      reachableFromNonTestRoots =
        reachableDefinitions dependencyGraph (nonTestMainRoots <> aliveOptionRoots)
      reachableFromTestRoots =
        reachableDefinitions dependencyGraph (testMainRoots <> aliveOptionRoots)
      testOnlyModules =
        Set.fromList
          [ module_
          | (module_, componentKinds) <- Map.toList moduleKindsByModule,
            isTestOnlyComponentModule componentKinds
          ]
      aliveByReachability =
        Set.fromList
          [ definitionId
          | (definitionId, source) <- Map.toList projectIndex.projectDefinitionsById,
            definitionIsAlive testOnlyModules source definitionId reachableFromNonTestRoots reachableFromTestRoots
          ]
      aliveInstanceDefinitionIds =
        aliveInstanceDefinitionsByHeadTypes
          projectIndex.projectInstanceHeadTypeDefinitionIdsByInstance
          aliveByReachability
      aliveDefinitionIds =
        (aliveByReachability `Set.difference` projectIndex.projectInstanceDefinitionIds)
          <> aliveInstanceDefinitionIds
      allDefinitionIds =
        Map.keysSet projectIndex.projectDefinitionsById
      deadDefinitions =
        collectDeadDefinitions options projectIndex aliveDefinitionIds
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
    [ (source.definitionSourceModule, Set.singleton definitionId)
    | (definitionId, source) <- Map.toList projectIndex.projectDefinitionsById,
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
  if definitionSource.definitionSourceModule `Set.member` testOnlyModules
    then definitionId `Set.member` reachableFromTestRoots
    else definitionId `Set.member` reachableFromNonTestRoots

aliveModuleRoots ::
  DeadCodeOptions ->
  ProjectDefinitionIndex ->
  Set.Set DefinitionId
aliveModuleRoots options projectIndex =
  Set.fromList
    [ definitionId
    | (definitionId, source) <- Map.toList projectIndex.projectDefinitionsById,
      source.definitionSourceModule `Set.member` options.deadCodeAliveModules
    ]

aliveNameRoots ::
  DeadCodeOptions ->
  ProjectDefinitionIndex ->
  Set.Set DefinitionId
aliveNameRoots options projectIndex =
  Set.fromList
    [ definitionId
    | name <- Set.toList options.deadCodeAliveNames,
      Just definitionId <- [Map.lookup name projectIndex.projectDefinitionIdByName]
    ]

collectDeadDefinitions ::
  DeadCodeOptions ->
  ProjectDefinitionIndex ->
  Set.Set DefinitionId ->
  [DeadDefinition]
collectDeadDefinitions options projectIndex aliveDefinitionIds =
  sortOn deadDefinitionSortKey $
    [ DeadDefinition
        { deadDefinitionSource = source,
          deadDefinitionNames = source.definitionSourceNames
        }
    | (definitionId, source) <- Map.toList projectIndex.projectDefinitionsById,
      Set.notMember definitionId aliveDefinitionIds,
      isReportableDefinition source
    ]
  where
    isReportableDefinition source =
      case options.deadCodeTargetModules of
        Nothing ->
          True
        Just targetModules ->
          source.definitionSourceModule `Set.member` targetModules

deadDefinitionSortKey ::
  DeadDefinition ->
  (String, String, Int, Int, [String])
deadDefinitionSortKey deadDefinition =
  ( moduleNameKey,
    sourceFileKey,
    sourceLineKey,
    sourceColumnKey,
    nameKeys
  )
  where
    source =
      deadDefinition.deadDefinitionSource
    moduleNameKey =
      GHC.moduleNameString (GHC.moduleName source.definitionSourceModule)
    nameKeys =
      map GHC.getOccString (Set.toAscList deadDefinition.deadDefinitionNames)
    (sourceFileKey, sourceLineKey, sourceColumnKey) =
      case realSrcSpanFromSrcSpan source.definitionSourceSpans.declarationSpan of
        Just realSrcSpan ->
          ( GHC.unpackFS (GHC.srcSpanFile realSrcSpan),
            GHC.srcSpanStartLine realSrcSpan,
            GHC.srcSpanStartCol realSrcSpan
          )
        Nothing ->
          ("", maxBound, maxBound)
