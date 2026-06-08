module Lore.Internal.Definition.ProjectIndex
  ( DefinitionTarget (..),
    ProjectDefinitionIndex (..),
    loadProjectDefinitionIndex,
    buildProjectDefinitionIndex,
    lookupDefinitionTarget,
    lookupDefinitionSource,
    dependenciesForNamedTarget,
    dependenciesForDeclaration,
  )
where

import Control.DeepSeq (NFData (..), deepseq)
import Control.Exception (evaluate)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (foldl')
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC.Conc (getNumCapabilities)
import GHC.Generics (Generic)
import qualified GHC.Plugins as GHC
import Lore.Internal.Definition.Cache.DefinitionModuleIndex
  ( getCachedDefinitionModuleIndexesConcurrently,
  )
import Lore.Internal.Definition.Types
  ( DefinitionCatalog (..),
    DefinitionDependencies (..),
    DefinitionId,
    DefinitionModuleIndex (..),
    DefinitionSource (..),
  )
import Lore.Internal.Lookup.ModSummaries (getCachedModSummaries, modSummariesToMap)
import Lore.Monad (MonadLore)

data DefinitionTarget = DefinitionTarget
  { definitionTargetName :: !GHC.Name,
    definitionTargetId :: !DefinitionId
  }
  deriving stock (Eq, Ord, Generic)

data ResolvedDefinitionDependencies = ResolvedDefinitionDependencies
  { resolvedClosureTargetsByReferenceName :: !(Map.Map GHC.Name (Set.Set DefinitionTarget)),
    resolvedReachabilityTargets :: !(Set.Set DefinitionTarget)
  }
  deriving stock (Eq, Generic)

data ProjectDefinitionIndex = ProjectDefinitionIndex
  { projectDefinitionCatalog :: !DefinitionCatalog,
    projectResolvedDependenciesById :: !(Map.Map DefinitionId ResolvedDefinitionDependencies),
    projectInstanceHeadTypeDefinitionIdsByInstance :: !(Map.Map DefinitionId (Set.Set DefinitionId))
  }
  deriving stock (Eq, Generic)

instance NFData DefinitionTarget where
  rnf DefinitionTarget {definitionTargetName, definitionTargetId} =
    rnf definitionTargetName `seq` rnf definitionTargetId

instance NFData ResolvedDefinitionDependencies where
  rnf ResolvedDefinitionDependencies {resolvedClosureTargetsByReferenceName, resolvedReachabilityTargets} =
    rnf resolvedClosureTargetsByReferenceName `seq`
      rnf resolvedReachabilityTargets

instance NFData ProjectDefinitionIndex where
  rnf ProjectDefinitionIndex {projectDefinitionCatalog, projectResolvedDependenciesById, projectInstanceHeadTypeDefinitionIdsByInstance} =
    rnf projectDefinitionCatalog `seq`
      rnf projectResolvedDependenciesById `seq`
        rnf projectInstanceHeadTypeDefinitionIdsByInstance

loadProjectDefinitionIndex :: (MonadLore m) => m ProjectDefinitionIndex
loadProjectDefinitionIndex = do
  modSummaries <- getCachedModSummaries
  capabilityCount <- liftIO getNumCapabilities
  let modSummariesByModule =
        modSummariesToMap modSummaries
      homeModules =
        Map.keys modSummariesByModule
      workerCount =
        max 1 capabilityCount
  moduleIndexes <-
    getCachedDefinitionModuleIndexesConcurrently
      workerCount
      modSummariesByModule
      homeModules
  forceProjectDefinitionIndex (buildProjectDefinitionIndex moduleIndexes)

buildProjectDefinitionIndex ::
  [DefinitionModuleIndex] ->
  ProjectDefinitionIndex
buildProjectDefinitionIndex moduleIndexes =
  ProjectDefinitionIndex
    { projectDefinitionCatalog = catalog,
      projectResolvedDependenciesById =
        resolveDependenciesById catalog (mergeIdenticalMaps "definition dependencies" (map dependenciesById moduleIndexes)),
      projectInstanceHeadTypeDefinitionIdsByInstance =
        mergeIdenticalMaps "instance-head dependencies" (map instanceHeadTypeDefinitionIdsByInstance moduleIndexes)
    }
  where
    catalog =
      mergeDefinitionCatalogs (map definitionCatalog moduleIndexes)

lookupDefinitionTarget ::
  ProjectDefinitionIndex ->
  GHC.Name ->
  Maybe DefinitionTarget
lookupDefinitionTarget projectIndex name = do
  definitionId <- Map.lookup name projectIndex.projectDefinitionCatalog.definitionIdsByName
  pure
    DefinitionTarget
      { definitionTargetName = name,
        definitionTargetId = definitionId
      }

lookupDefinitionSource ::
  ProjectDefinitionIndex ->
  DefinitionId ->
  Maybe DefinitionSource
lookupDefinitionSource projectIndex definitionId =
  Map.lookup definitionId projectIndex.projectDefinitionCatalog.definitionSourcesById

dependenciesForNamedTarget ::
  ProjectDefinitionIndex ->
  DefinitionTarget ->
  Set.Set DefinitionTarget
dependenciesForNamedTarget projectIndex target =
  case Map.lookup target.definitionTargetId projectIndex.projectResolvedDependenciesById of
    Nothing ->
      Set.empty
    Just dependencies ->
      Map.findWithDefault
        Set.empty
        target.definitionTargetName
        dependencies.resolvedClosureTargetsByReferenceName

dependenciesForDeclaration ::
  ProjectDefinitionIndex ->
  DefinitionId ->
  Set.Set DefinitionId
dependenciesForDeclaration projectIndex definitionId =
  case Map.lookup definitionId projectIndex.projectResolvedDependenciesById of
    Nothing ->
      Set.empty
    Just dependencies ->
      Set.map definitionTargetId dependencies.resolvedReachabilityTargets

mergeDefinitionCatalogs :: [DefinitionCatalog] -> DefinitionCatalog
mergeDefinitionCatalogs catalogs =
  DefinitionCatalog
    { definitionSourcesById =
        mergeIdenticalMaps "definition sources" (map definitionSourcesById catalogs),
      definitionIdsByName =
        mergeIdenticalMaps "definition ids by name" (map definitionIdsByName catalogs)
    }

mergeIdenticalMaps ::
  (Eq value, Ord key) =>
  String ->
  [Map.Map key value] ->
  Map.Map key value
mergeIdenticalMaps label =
  foldl' mergeOne Map.empty
  where
    mergeOne accumulated nextMap =
      Map.foldlWithKey' insertEntry accumulated nextMap

    insertEntry accumulated key value =
      case Map.lookup key accumulated of
        Nothing ->
          Map.insert key value accumulated
        Just existingValue
          | existingValue == value ->
              accumulated
          | otherwise ->
              error ("conflicting " <> label <> " while building project definition index")

resolveDependenciesById ::
  DefinitionCatalog ->
  Map.Map DefinitionId DefinitionDependencies ->
  Map.Map DefinitionId ResolvedDefinitionDependencies
resolveDependenciesById catalog =
  Map.map (resolveDefinitionDependencies catalog)

resolveDefinitionDependencies ::
  DefinitionCatalog ->
  DefinitionDependencies ->
  ResolvedDefinitionDependencies
resolveDefinitionDependencies catalog dependencies =
  ResolvedDefinitionDependencies
    { resolvedClosureTargetsByReferenceName =
        Map.map (resolveDependencyNames catalog) dependencies.dependencyClosureNamesByReferenceName,
      resolvedReachabilityTargets =
        resolveDependencyNames catalog dependencies.dependencyReachabilityNames
    }

resolveDependencyNames ::
  (Foldable f) =>
  DefinitionCatalog ->
  f GHC.Name ->
  Set.Set DefinitionTarget
resolveDependencyNames catalog =
  foldl' insertDependencyName Set.empty
  where
    insertDependencyName resolvedTargets dependencyName =
      case Map.lookup dependencyName catalog.definitionIdsByName of
        Just dependencyId ->
          Set.insert
            DefinitionTarget
              { definitionTargetName = dependencyName,
                definitionTargetId = dependencyId
              }
            resolvedTargets
        Nothing ->
          resolvedTargets

forceProjectDefinitionIndex :: (MonadIO m) => ProjectDefinitionIndex -> m ProjectDefinitionIndex
forceProjectDefinitionIndex projectIndex = do
  _ <- liftIO (evaluate (projectIndex `deepseq` projectIndex))
  pure projectIndex
