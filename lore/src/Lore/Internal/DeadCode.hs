module Lore.Internal.DeadCode
  ( DeadCodeOptions (..),
    DeadDefinition (..),
    DeadCodeResult (..),
    findDeadCode,
  )
where

import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC.Plugins as GHC
import Lore.Internal.Definition.Cache.DefinitionModuleIndex (getCachedDefinitionModuleIndexes)
import Lore.Internal.Definition.Types
  ( DeclarationSpans (..),
    DefinitionDependencies (..),
    DefinitionId,
    DefinitionModuleIndex (..),
    DefinitionSource (..),
  )
import Lore.Internal.HomeModules.EntryModules
  ( ComponentEntryModule (..),
    collectLoadedComponentEntryModulesWithDiagnostics,
    collectLoadedComponentModuleKinds,
  )
import Lore.Internal.Lookup.ModSummaries (getCachedModSummaries, modSummariesToMap)
import Lore.Internal.Package (ComponentKind (..))
import Lore.Monad (MonadLore)
import Lore.SourceSpan (realSrcSpanFromSrcSpan)

data DeadCodeOptions = DeadCodeOptions
  { deadCodeTargetModules :: Maybe (Set.Set GHC.Module),
    deadCodeAliveModules :: Set.Set GHC.Module,
    deadCodeAliveNames :: Set.Set GHC.Name
  }

data DeadDefinition = DeadDefinition
  { deadDefinitionSource :: DefinitionSource,
    deadDefinitionNames :: Set.Set GHC.Name
  }

data DeadCodeResult = DeadCodeResult
  { deadCodeTotalDefinitions :: Int,
    deadCodeAliveDefinitions :: Int,
    deadCodeDeadDefinitions :: [DeadDefinition],
    deadCodeWarnings :: [Text]
  }

data ProjectDefinitionIndex = ProjectDefinitionIndex
  { projectDefinitionsById :: Map.Map DefinitionId DefinitionSource,
    projectDefinitionIdByName :: Map.Map GHC.Name DefinitionId,
    projectDependenciesById :: Map.Map DefinitionId DefinitionDependencies
  }

findDeadCode :: (MonadLore m) => DeadCodeOptions -> m DeadCodeResult
findDeadCode options = do
  projectIndex <- loadProjectDefinitionIndex
  moduleKindsByModule <- collectLoadedComponentModuleKinds
  (componentEntries, entryModuleDiagnostics) <- collectLoadedComponentEntryModulesWithDiagnostics
  let dependencyGraph =
        buildDependencyGraph projectIndex
      nonTestMainRoots =
        collectMainRootsForKind projectIndex componentEntries componentIsNonTestRoot
      testMainRoots =
        collectMainRootsForKind projectIndex componentEntries componentIsTestRoot
      aliveOptionRoots =
        aliveModuleRoots options projectIndex
          <> aliveNameRoots options projectIndex
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
      aliveDefinitionIds =
        Set.fromList
          [ definitionId
          | (definitionId, source) <- Map.toList projectIndex.projectDefinitionsById,
            definitionIsAlive testOnlyModules source definitionId reachableFromNonTestRoots reachableFromTestRoots
          ]
      allDefinitionIds =
        Map.keysSet projectIndex.projectDefinitionsById
      deadDefinitionIds =
        allDefinitionIds `Set.difference` aliveDefinitionIds
      deadDefinitions =
        sortOn deadDefinitionSortKey $
          [ DeadDefinition
              { deadDefinitionSource = source,
                deadDefinitionNames = source.definitionSourceNames
              }
          | definitionId <- Set.toList deadDefinitionIds,
            Just source <- [Map.lookup definitionId projectIndex.projectDefinitionsById],
            isReportableDefinition source
          ]
  pure
    DeadCodeResult
      { deadCodeTotalDefinitions = Set.size allDefinitionIds,
        deadCodeAliveDefinitions = Set.size aliveDefinitionIds,
        deadCodeDeadDefinitions = deadDefinitions,
        deadCodeWarnings = map T.pack entryModuleDiagnostics
      }
  where
    isReportableDefinition source =
      case options.deadCodeTargetModules of
        Nothing ->
          True
        Just targetModules ->
          source.definitionSourceModule `Set.member` targetModules

loadProjectDefinitionIndex :: (MonadLore m) => m ProjectDefinitionIndex
loadProjectDefinitionIndex = do
  modSummaries <- getCachedModSummaries
  let modSummariesByModule =
        modSummariesToMap modSummaries
      homeModules =
        Map.keys modSummariesByModule
  moduleIndexes <-
    getCachedDefinitionModuleIndexes
      modSummariesByModule
      homeModules
  pure
    ProjectDefinitionIndex
      { projectDefinitionsById =
          Map.unions (map (.definitionsById) moduleIndexes),
        projectDefinitionIdByName =
          Map.unions (map (.definitionIdByName) moduleIndexes),
        projectDependenciesById =
          Map.unions (map (.dependenciesById) moduleIndexes)
      }

buildDependencyGraph ::
  ProjectDefinitionIndex ->
  Map.Map DefinitionId (Set.Set DefinitionId)
buildDependencyGraph projectIndex =
  Map.unionWith Set.union emptyGraph resolvedGraph
  where
    emptyGraph =
      Map.fromSet (const Set.empty) (Map.keysSet projectIndex.projectDefinitionsById)

    resolvedGraph =
      Map.map resolveDependencies projectIndex.projectDependenciesById

    resolveDependencies dependencies =
      Set.fromList
        [ dependencyId
        | dependencyName <- Set.toList dependencyNames,
          Just dependencyId <- [Map.lookup dependencyName projectIndex.projectDefinitionIdByName]
        ]
      where
        dependencyNames =
          dependencies.dependencyDirectReferenceNames
            <> dependencies.dependencyUsedInstanceNames

collectMainRootsForKind ::
  ProjectDefinitionIndex ->
  [ComponentEntryModule] ->
  (ComponentKind -> Bool) ->
  Set.Set DefinitionId
collectMainRootsForKind projectIndex componentEntries isRootComponentKind =
  Set.unions
    [ definitionIdsForModuleMain projectIndex entryModule
    | ComponentEntryModule {entryComponentKind, entryModule} <- componentEntries,
      isRootComponentKind entryComponentKind
    ]

componentIsNonTestRoot :: ComponentKind -> Bool
componentIsNonTestRoot componentKind =
  case componentKind of
    ComponentKindExecutable -> True
    ComponentKindBenchmark -> True
    ComponentKindTest -> False
    ComponentKindLibrary -> False
    ComponentKindInternalLibrary -> False

componentIsTestRoot :: ComponentKind -> Bool
componentIsTestRoot componentKind =
  case componentKind of
    ComponentKindTest -> True
    ComponentKindExecutable -> False
    ComponentKindBenchmark -> False
    ComponentKindLibrary -> False
    ComponentKindInternalLibrary -> False

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

definitionIdsForModuleMain ::
  ProjectDefinitionIndex ->
  GHC.Module ->
  Set.Set DefinitionId
definitionIdsForModuleMain projectIndex module_ =
  Set.fromList
    [ definitionId
    | (definitionId, source) <- Map.toList projectIndex.projectDefinitionsById,
      source.definitionSourceModule == module_,
      any ((== "main") . GHC.getOccString) (Set.toList source.definitionSourceNames)
    ]

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

reachableDefinitions ::
  Map.Map DefinitionId (Set.Set DefinitionId) ->
  Set.Set DefinitionId ->
  Set.Set DefinitionId
reachableDefinitions graph roots =
  go Set.empty (Set.toList roots)
  where
    go seen [] =
      seen
    go seen (definitionId : queue)
      | definitionId `Set.member` seen =
          go seen queue
      | otherwise =
          let nextDependencies =
                Set.toList (Map.findWithDefault Set.empty definitionId graph)
           in go
                (Set.insert definitionId seen)
                (nextDependencies <> queue)

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
