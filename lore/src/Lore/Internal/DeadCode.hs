module Lore.Internal.DeadCode
  ( DeadCodeOptions (..),
    DeadDefinition (..),
    DeadCodeResult (..),
    findDeadCode,
  )
where

import Control.DeepSeq (NFData (..))
import Control.Exception (evaluate)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (foldl')
import qualified Data.IntMap.Strict as IntMap
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (maybeToList)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Conc (getNumCapabilities)
import GHC.Generics (Generic)
import qualified GHC.Plugins as GHC
import qualified GHC.Types.Unique as GHCUnique
import Lore.Internal.Definition.Analysis
  ( buildDefinitionBindings,
  )
import Lore.Internal.Definition.Cache.ModuleArtifacts
  ( DefinitionModuleArtifacts (..),
    lookupDefinitionModuleArtifactsForModules,
  )
import Lore.Internal.Definition.Types
  ( DeclarationSpans (..),
    DefinitionBindings (..),
    DefinitionId,
    DefinitionSource (..),
    MinimalCoreModuleFacts (..),
    MinimalTypedModuleFacts (..),
    MinimalTypedOccurrence (..),
  )
import Lore.Internal.HomeModules.EntryModules
  ( ComponentEntryModule (..),
    collectLoadedComponentModuleInfoWithDiagnostics,
  )
import Lore.Internal.Lookup.ModSummaries (getCachedModSummaries, modSummariesToMap)
import Lore.Internal.Package (ComponentKind (..))
import Lore.Monad (MonadLore)
import Lore.SourceSpan (realSrcSpanFromSrcSpan)
import UnliftIO (evaluateDeep)
import qualified UnliftIO.Async as Async

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

data ProjectDefinitionIndex = ProjectDefinitionIndex
  { projectDefinitionsById :: Map.Map DefinitionId DefinitionSource,
    projectDefinitionIdByName :: Map.Map GHC.Name DefinitionId,
    projectDependenciesById :: Map.Map DefinitionId ProjectDependencyNames,
    projectInstanceHeadTypeDefinitionIdsByInstance :: Map.Map DefinitionId (Set.Set DefinitionId),
    projectInstanceDefinitionIds :: Set.Set DefinitionId
  }
  deriving stock (Eq, Generic)

data ProjectDependencyNames = ProjectDependencyNames
  { projectDependencyDirectReferenceNames :: !(Set.Set GHC.Name),
    projectDependencyCoreSemanticNames :: ![GHC.Name]
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

instance NFData ProjectDefinitionIndex where
  rnf ProjectDefinitionIndex {projectDefinitionsById, projectDefinitionIdByName, projectDependenciesById, projectInstanceHeadTypeDefinitionIdsByInstance, projectInstanceDefinitionIds} =
    Map.size projectDefinitionsById `seq`
      Map.size projectDefinitionIdByName `seq`
        Map.size projectDependenciesById `seq`
          Map.size projectInstanceHeadTypeDefinitionIdsByInstance `seq`
            Set.size projectInstanceDefinitionIds `seq`
              ()

instance NFData ProjectDependencyNames where
  rnf dependencies =
    projectDependencyWeight dependencies `seq` ()

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

forceProjectDefinitionIndex :: (MonadIO m) => ProjectDefinitionIndex -> m ProjectDefinitionIndex
forceProjectDefinitionIndex projectIndex = do
  forceDefinitionSourceMap projectIndex.projectDefinitionsById
  forceDefinitionIdMap projectIndex.projectDefinitionIdByName
  _ <- forceDependenciesById projectIndex.projectDependenciesById
  _ <- liftIO (evaluate (Map.size projectIndex.projectInstanceHeadTypeDefinitionIdsByInstance))
  _ <- liftIO (evaluate (Set.size projectIndex.projectInstanceDefinitionIds))
  pure projectIndex

forceDefinitionBindings :: (MonadIO m) => DefinitionBindings -> m DefinitionBindings
forceDefinitionBindings bindings = do
  forceDefinitionSourceMap bindings.bindingDefinitionsById
  forceDefinitionIdMap bindings.bindingDefinitionIdByName
  pure bindings

forceDefinitionSourceMap :: (MonadIO m) => Map.Map DefinitionId DefinitionSource -> m ()
forceDefinitionSourceMap definitionsById = do
  _ <-
    liftIO $
      evaluate $
        Map.size definitionsById
          + foldl'
            ( \total source ->
                total
                  + Set.size source.definitionSourceNames
            )
            0
            (Map.elems definitionsById)
  pure ()

forceDefinitionIdMap :: (MonadIO m) => Map.Map GHC.Name DefinitionId -> m ()
forceDefinitionIdMap definitionIdByName =
  liftIO (evaluate (Map.size definitionIdByName)) >> pure ()

forceDependenciesById :: (MonadIO m) => Map.Map DefinitionId ProjectDependencyNames -> m (Map.Map DefinitionId ProjectDependencyNames)
forceDependenciesById dependenciesById = do
  _ <-
    liftIO $
      evaluate $
        Map.size dependenciesById
          + foldl'
            ( \total dependencies ->
                total + projectDependencyWeight dependencies
            )
            0
            (Map.elems dependenciesById)
  pure dependenciesById

projectDependencyWeight :: ProjectDependencyNames -> Int
projectDependencyWeight dependencies =
  Set.size dependencies.projectDependencyDirectReferenceNames
    + length dependencies.projectDependencyCoreSemanticNames

loadProjectDefinitionIndex :: (MonadLore m) => m ProjectDefinitionIndex
loadProjectDefinitionIndex = do
  modSummaries <- getCachedModSummaries
  capabilityCount <- liftIO getNumCapabilities
  let homeModules =
        Map.keys (modSummariesToMap modSummaries)
      workerCount =
        max 1 capabilityCount
  artifactsByModule <- lookupDefinitionModuleArtifactsForModules homeModules
  moduleIndexes <-
    Async.pooledMapConcurrentlyN
      workerCount
      (uncurry buildProjectIndexForModule)
      (Map.toList artifactsByModule)
  let mergedIndex =
        ProjectDefinitionIndex
          { projectDefinitionsById =
              Map.unions (map projectDefinitionsById moduleIndexes),
            projectDefinitionIdByName =
              Map.unions (map projectDefinitionIdByName moduleIndexes),
            projectDependenciesById =
              Map.unions (map projectDependenciesById moduleIndexes),
            projectInstanceHeadTypeDefinitionIdsByInstance =
              Map.unions (map projectInstanceHeadTypeDefinitionIdsByInstance moduleIndexes),
            projectInstanceDefinitionIds =
              Set.unions (map projectInstanceDefinitionIds moduleIndexes)
          }
  forceProjectDefinitionIndex mergedIndex

buildProjectIndexForModule ::
  (MonadLore m) =>
  GHC.Module ->
  DefinitionModuleArtifacts ->
  m ProjectDefinitionIndex
buildProjectIndexForModule homeModule DefinitionModuleArtifacts {definitionArtifactParsedFacts, definitionArtifactTypedFacts, definitionArtifactCoreFacts} = do
  let bindings =
        buildDefinitionBindings homeModule definitionArtifactParsedFacts definitionArtifactTypedFacts
  forcedBindings <- forceDefinitionBindings bindings
  let directReferencesById =
        collectDirectReferencesByDefinitionId
          forcedBindings
          forcedBindings.bindingDefinitionsById
          definitionArtifactTypedFacts.typedOccurrences
  let dependenciesById =
        buildDeadCodeDependenciesFromDirectRefs
          forcedBindings
          definitionArtifactCoreFacts
          directReferencesById
      instanceDefinitionIds =
        collectInstanceDefinitionIds
          forcedBindings.bindingDefinitionIdByName
          definitionArtifactTypedFacts
      instanceHeadTypeDefinitionIdsByInstance =
        collectInstanceHeadTypeDefinitionIdsByInstance
          forcedBindings.bindingDefinitionIdByName
          definitionArtifactTypedFacts
  let projectIndex =
        ProjectDefinitionIndex
          { projectDefinitionsById = forcedBindings.bindingDefinitionsById,
            projectDefinitionIdByName = forcedBindings.bindingDefinitionIdByName,
            projectDependenciesById = dependenciesById,
            projectInstanceHeadTypeDefinitionIdsByInstance = instanceHeadTypeDefinitionIdsByInstance,
            projectInstanceDefinitionIds = instanceDefinitionIds
          }
  forceProjectDefinitionIndex projectIndex

collectInstanceDefinitionIds ::
  Map.Map GHC.Name DefinitionId ->
  MinimalTypedModuleFacts ->
  Set.Set DefinitionId
collectInstanceDefinitionIds definitionIdByName typedFacts =
  Set.fromList
    [ definitionId
    | instanceName <- typedFacts.typedInstanceNames,
      Just definitionId <- [Map.lookup instanceName definitionIdByName]
    ]

collectInstanceHeadTypeDefinitionIdsByInstance ::
  Map.Map GHC.Name DefinitionId ->
  MinimalTypedModuleFacts ->
  Map.Map DefinitionId (Set.Set DefinitionId)
collectInstanceHeadTypeDefinitionIdsByInstance definitionIdByName typedFacts =
  Map.fromList
    [ (instanceDefinitionId, headTypeDefinitionIds)
    | (instanceName, headTypeNames) <- Map.toList typedFacts.typedInstanceHeadTypeNamesByInstance,
      Just instanceDefinitionId <- [Map.lookup instanceName definitionIdByName],
      let headTypeDefinitionIds =
            Set.fromList
              [ definitionId
              | headTypeName <- Set.toList headTypeNames,
                Just definitionId <- [Map.lookup headTypeName definitionIdByName]
              ]
    ]

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

buildDeadCodeDependenciesFromDirectRefs ::
  DefinitionBindings ->
  Maybe MinimalCoreModuleFacts ->
  Map.Map DefinitionId (Set.Set GHC.Name) ->
  Map.Map DefinitionId ProjectDependencyNames
buildDeadCodeDependenciesFromDirectRefs bindings maybeCoreFacts directReferencesById =
  Map.fromList
    [ (definitionId, dependenciesForDefinition definitionId source)
    | (definitionId, source) <- Map.toList bindings.bindingDefinitionsById
    ]
  where
    coreSemanticDependenciesByBinder =
      IntMap.fromListWith
        (<>)
        [ (nameUniqueKey binderName, semanticNames)
        | (binderName, semanticNames) <-
            Map.toList (maybe Map.empty (.coreSemanticDependenciesByBinder) maybeCoreFacts)
        ]

    dependenciesForDefinition definitionId source =
      ProjectDependencyNames
        { projectDependencyDirectReferenceNames =
            Map.findWithDefault Set.empty definitionId directReferencesById,
          projectDependencyCoreSemanticNames =
            [ semanticName
            | definitionName <- Set.toList source.definitionSourceNames,
              semanticName <- IntMap.findWithDefault [] (nameUniqueKey definitionName) coreSemanticDependenciesByBinder
            ]
        }

collectDirectReferencesByDefinitionId ::
  DefinitionBindings ->
  Map.Map DefinitionId DefinitionSource ->
  [MinimalTypedOccurrence] ->
  Map.Map DefinitionId (Set.Set GHC.Name)
collectDirectReferencesByDefinitionId bindings definitionsById typedOccurrences =
  foldl'
    addOccurrenceReference
    (Map.fromSet (const Set.empty) (Map.keysSet definitionsById))
    typedOccurrences
  where
    addOccurrenceReference referencesById occurrence =
      case resolveOccurrenceOwnerId occurrence of
        Just definitionId
          | Just source <- Map.lookup definitionId definitionsById,
            isFollowableReference source.definitionSourceNames source.definitionSourceSpans occurrence.typedOccurrenceName ->
              Map.adjust (Set.insert occurrence.typedOccurrenceName) definitionId referencesById
        _ ->
          referencesById

    resolveOccurrenceOwnerId occurrence =
      case occurrence.typedOccurrenceParent >>= (`Map.lookup` bindings.bindingDefinitionIdByName) of
        Just definitionId ->
          Just definitionId
        Nothing ->
          resolveOwnerIdBySpan occurrence.typedOccurrenceSpan

    definitionSpansById =
      [ (definitionId, declarationTargetSpans source.definitionSourceSpans)
      | (definitionId, source) <- Map.toList definitionsById
      ]

    resolveOwnerIdBySpan occurrenceSpan =
      case [ definitionId
           | (definitionId, targetSpans) <- definitionSpansById,
             spanWithin targetSpans occurrenceSpan
           ] of
        (definitionId : _) ->
          Just definitionId
        [] ->
          Nothing

declarationTargetSpans :: DeclarationSpans -> [GHC.SrcSpan]
declarationTargetSpans declarationSpans =
  declarationSpans.declarationSpan
    : maybeToList declarationSpans.signatureSpan

spanWithin :: [GHC.SrcSpan] -> GHC.SrcSpan -> Bool
spanWithin targetSpans span' =
  any (span' `GHC.isSubspanOf`) targetSpans

isFollowableReference :: Set.Set GHC.Name -> DeclarationSpans -> GHC.Name -> Bool
isFollowableReference definitionNames spans name =
  Set.notMember name definitionNames
    && case GHC.nameModule_maybe name of
      Nothing -> False
      Just definingModule ->
        not (definesName spans.declarationSpan definingModule name)

definesName :: GHC.SrcSpan -> GHC.Module -> GHC.Name -> Bool
definesName declarationSpan definingModule name =
  GHC.nameModule_maybe name == Just definingModule
    && GHC.nameSrcSpan name `GHC.isSubspanOf` declarationSpan

buildDependencyGraph ::
  (MonadLore m) =>
  ProjectDefinitionIndex ->
  m (Map.Map DefinitionId (Set.Set DefinitionId))
buildDependencyGraph projectIndex = do
  let dependencyItems =
        Map.toList projectIndex.projectDependenciesById
      dependencyCount =
        length dependencyItems
      definitionIdByUnique =
        IntMap.fromList
          [ (nameUniqueKey name, definitionId)
          | (name, definitionId) <- Map.toList projectIndex.projectDefinitionIdByName
          ]
      workerCount =
        8
      chunkSize =
        max 128 (dependencyCount `div` (workerCount * 8) + 1)
      dependencyChunks =
        chunkList chunkSize dependencyItems
  resolvedChunks <-
    Async.pooledMapConcurrentlyN workerCount (resolveChunk definitionIdByUnique) dependencyChunks
  let graph =
        Map.fromList (concat resolvedChunks)
  evaluateDeep graph

resolveChunk ::
  (MonadLore m) =>
  IntMap.IntMap DefinitionId ->
  [(DefinitionId, ProjectDependencyNames)] ->
  m [(DefinitionId, Set.Set DefinitionId)]
resolveChunk definitionIdByUnique dependencyChunk =
  mapM resolveDependencyItem dependencyChunk
  where
    resolveDependencyItem (definitionId, dependencies) = do
      let resolvedDependencies = resolveDependencies definitionIdByUnique dependencies
          resolvedEntry = (definitionId, resolvedDependencies)
      evaluateDeep resolvedEntry

resolveDependencies ::
  IntMap.IntMap DefinitionId ->
  ProjectDependencyNames ->
  Set.Set DefinitionId
resolveDependencies definitionIdByUnique dependencies =
  foldCoreDependencyNames
    (foldDirectDependencyNames Set.empty dependencies.projectDependencyDirectReferenceNames)
    dependencies.projectDependencyCoreSemanticNames
  where
    foldDirectDependencyNames initial names =
      Set.foldl'
        ( \resolvedDependencyIds dependencyName ->
            case IntMap.lookup (nameUniqueKey dependencyName) definitionIdByUnique of
              Just dependencyId ->
                Set.insert dependencyId resolvedDependencyIds
              Nothing ->
                resolvedDependencyIds
        )
        initial
        names

    foldCoreDependencyNames =
      foldl'
        ( \resolvedDependencyIds dependencyName ->
            case IntMap.lookup (nameUniqueKey dependencyName) definitionIdByUnique of
              Just dependencyId ->
                Set.insert dependencyId resolvedDependencyIds
              Nothing ->
                resolvedDependencyIds
        )

nameUniqueKey :: GHC.Name -> Int
nameUniqueKey =
  GHCUnique.getKey . GHC.getUnique

chunkList :: Int -> [a] -> [[a]]
chunkList chunkSize xs
  | chunkSize <= 0 =
      [xs]
  | otherwise =
      go xs
  where
    go [] =
      []
    go remaining =
      let (chunk, rest) = splitAt chunkSize remaining
       in chunk : go rest

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
