{-# OPTIONS_GHC -Wno-identities #-}

module Lore.Internal.Definition.Graph
  ( DefinitionGraph,
    ProjectDefinitionIndex (..),
    buildDependencyGraph,
    loadProjectDefinitionIndex,
    reachableDefinitions,
  )
where

import Control.DeepSeq (NFData (..))
import Control.Exception (evaluate)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (foldl')
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC.Conc (getNumCapabilities)
import GHC.Generics (Generic)
import qualified GHC.Plugins as GHC
import Lore.Internal.Definition.Analysis (buildDefinitionModuleIndex)
import Lore.Internal.Definition.Analysis.Common (nameUniqueKey)
import Lore.Internal.Definition.Cache.ModuleArtifacts
  ( DefinitionModuleArtifacts (..),
    lookupDefinitionModuleArtifactsForModules,
  )
import Lore.Internal.Definition.Types
  ( DefinitionDependencies (..),
    DefinitionId,
    DefinitionModuleIndex (..),
    DefinitionSource (..),
    MinimalTypedModuleFacts (..),
  )
import Lore.Internal.Lookup.ModSummaries (getCachedModSummaries, modSummariesToMap)
import Lore.Monad (MonadLore)
import UnliftIO (evaluateDeep)
import qualified UnliftIO.Async as Async

newtype DefinitionGraph = DefinitionGraph (Map.Map DefinitionId (Set.Set DefinitionId))

data ProjectDefinitionIndex = ProjectDefinitionIndex
  { projectDefinitionsById :: Map.Map DefinitionId DefinitionSource,
    projectDefinitionIdByName :: Map.Map GHC.Name DefinitionId,
    projectDependenciesById :: Map.Map DefinitionId DefinitionDependencies,
    projectInstanceHeadTypeDefinitionIdsByInstance :: Map.Map DefinitionId (Set.Set DefinitionId),
    projectInstanceDefinitionIds :: Set.Set DefinitionId
  }
  deriving stock (Eq, Generic)

instance NFData DefinitionGraph where
  rnf (DefinitionGraph graph) =
    rnf graph

instance NFData ProjectDefinitionIndex where
  rnf ProjectDefinitionIndex {projectDefinitionsById, projectDefinitionIdByName, projectDependenciesById, projectInstanceHeadTypeDefinitionIdsByInstance, projectInstanceDefinitionIds} =
    Map.size projectDefinitionsById `seq`
      Map.size projectDefinitionIdByName `seq`
        Map.size projectDependenciesById `seq`
          Map.size projectInstanceHeadTypeDefinitionIdsByInstance `seq`
            Set.size projectInstanceDefinitionIds `seq`
              ()

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

buildDependencyGraph ::
  (MonadLore m) =>
  ProjectDefinitionIndex ->
  m DefinitionGraph
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
        DefinitionGraph (Map.fromList (concat resolvedChunks))
  evaluateDeep graph

reachableDefinitions ::
  DefinitionGraph ->
  Set.Set DefinitionId ->
  Set.Set DefinitionId
reachableDefinitions (DefinitionGraph graph) roots =
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

forceProjectDefinitionIndex :: (MonadIO m) => ProjectDefinitionIndex -> m ProjectDefinitionIndex
forceProjectDefinitionIndex projectIndex = do
  forceDefinitionSourceMap projectIndex.projectDefinitionsById
  forceDefinitionIdMap projectIndex.projectDefinitionIdByName
  _ <- forceDependenciesById projectIndex.projectDependenciesById
  _ <- liftIO (evaluate (Map.size projectIndex.projectInstanceHeadTypeDefinitionIdsByInstance))
  _ <- liftIO (evaluate (Set.size projectIndex.projectInstanceDefinitionIds))
  pure projectIndex

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

forceDependenciesById :: (MonadIO m) => Map.Map DefinitionId DefinitionDependencies -> m (Map.Map DefinitionId DefinitionDependencies)
forceDependenciesById dependenciesById = do
  _ <-
    liftIO $
      evaluate $
        Map.size dependenciesById
          + foldl'
            ( \total dependencies ->
                total + dependencyWeight dependencies
            )
            0
            (Map.elems dependenciesById)
  pure dependenciesById

dependencyWeight :: DefinitionDependencies -> Int
dependencyWeight dependencies =
  Set.size dependencies.dependencyDirectReferenceNames
    + Set.size dependencies.dependencyUsedInstanceNames
    + length dependencies.dependencyCoreSemanticNames

buildProjectIndexForModule ::
  (MonadLore m) =>
  GHC.Module ->
  DefinitionModuleArtifacts ->
  m ProjectDefinitionIndex
buildProjectIndexForModule homeModule DefinitionModuleArtifacts {definitionArtifactParsedFacts, definitionArtifactTypedFacts, definitionArtifactCoreFacts} = do
  let moduleIndex =
        buildDefinitionModuleIndex
          homeModule
          definitionArtifactParsedFacts
          definitionArtifactTypedFacts
          definitionArtifactCoreFacts
      instanceDefinitionIds =
        collectInstanceDefinitionIds
          moduleIndex.definitionIdByName
          definitionArtifactTypedFacts
      instanceHeadTypeDefinitionIdsByInstance =
        collectInstanceHeadTypeDefinitionIdsByInstance
          moduleIndex.definitionIdByName
          definitionArtifactTypedFacts
  let projectIndex =
        ProjectDefinitionIndex
          { projectDefinitionsById = moduleIndex.definitionsById,
            projectDefinitionIdByName = moduleIndex.definitionIdByName,
            projectDependenciesById = moduleIndex.dependenciesById,
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

resolveChunk ::
  (MonadLore m) =>
  IntMap.IntMap DefinitionId ->
  [(DefinitionId, DefinitionDependencies)] ->
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
  DefinitionDependencies ->
  Set.Set DefinitionId
resolveDependencies definitionIdByUnique dependencies =
  resolveDependencyNames
    definitionIdByUnique
    (resolveDependencyNames definitionIdByUnique Set.empty dependencies.dependencyDirectReferenceNames)
    dependencies.dependencyCoreSemanticNames

resolveDependencyNames ::
  (Foldable f) =>
  IntMap.IntMap DefinitionId ->
  Set.Set DefinitionId ->
  f GHC.Name ->
  Set.Set DefinitionId
resolveDependencyNames definitionIdByUnique =
  foldl' insertDependencyName
  where
    insertDependencyName resolvedDependencyIds dependencyName =
      case IntMap.lookup (nameUniqueKey dependencyName) definitionIdByUnique of
        Just dependencyId ->
          Set.insert dependencyId resolvedDependencyIds
        Nothing ->
          resolvedDependencyIds

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
