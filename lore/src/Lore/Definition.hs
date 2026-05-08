module Lore.Definition
  ( -- Source-first definition API.
    resolveDefinitionSourceNamed,
    resolveDefinitionClosureSourcesNamed,
    getMinifiedImportsForDefinition,
    resolveReferenceMatchesForNames,
    mergeDefinitionSlices,
    DefinitionId (..),
    DefinitionSource (..),
    -- Rendering DTO used by existing renderers.
    DefinitionSlice (..),
    NamedDefinitionSource (..),
    DeclarationSpans (..),
    ReferenceHit (..),
    ReferenceMatch (..),
    RequiredImport (..),
    ImportQualifiedStyle (..),
    RequiredImportItem (..),
  )
where

import Control.DeepSeq (deepseq)
import qualified Control.Exception as Exception
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Containers.ListUtils (nubOrdOn)
import qualified Data.IntMap.Strict as IntMap
import Data.List (foldl')
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Data.Time.Clock (getCurrentTime)
import qualified GHC
import qualified GHC.Plugins as GHC
import Lore.Internal.Definition.Analysis (buildDefinitionModuleIndex, normalizeImportItems)
import qualified Lore.Internal.Definition.Analysis as DefinitionAnalysis
import Lore.Internal.Definition.Cache (cacheDefinitionModuleIndex, getParsedModuleFacts, getParsedOccurrenceModuleIndex, lookupDefinitionModuleIndexCache)
import Lore.Internal.Definition.Types (DeclarationSpans (..), DefinitionDependencies (..), DefinitionId (..), DefinitionModuleIndex (..), DefinitionSlice (..), DefinitionSource (..), ImportQualifiedStyle (..), MinimalCoreModuleFacts, MinimalTypedModuleFacts, NamedDefinitionSource (..), OccKey (..), ParsedModuleCache (..), ParsedModuleFacts (..), ParsedOccurrenceModuleIndex (..), ReferenceHit (..), ReferenceMatch (..), RequiredImport (..), RequiredImportItem (..), TypedModuleCache (..), nameOccKey, srcSpanKey)
import Lore.Internal.Lookup.ModSummaries (getModSummaries)
import Lore.Internal.Lookup.Types (ModSummaries (..))
import Lore.Internal.Session (SessionContext (..))
import qualified Lore.Logger as Log
import Lore.Monad
import UnliftIO (pooledForConcurrently, readMVar)

data ClosureState = ClosureState
  { closureSeen :: Set.Set DefinitionId,
    closureSources :: [NamedDefinitionSource]
  }

data CachedDefinitionModuleArtifacts = CachedDefinitionModuleArtifacts
  { cachedParsedModuleFacts :: ParsedModuleFacts,
    cachedMinimalTypedFacts :: MinimalTypedModuleFacts,
    cachedMinimalCoreFacts :: Maybe MinimalCoreModuleFacts
  }

resolveDefinitionSourceNamed :: (MonadLore m) => GHC.Name -> m (Maybe DefinitionSource)
resolveDefinitionSourceNamed inputName = do
  ModSummaries modSummaries <- getModSummaries
  resolveDefinitionSourceWithSummaries modSummaries inputName

getMinifiedImportsForDefinition :: (MonadLore m) => DefinitionSource -> m [RequiredImport]
getMinifiedImportsForDefinition source = do
  ModSummaries modSummaries <- getModSummaries
  getMinifiedImportsForDefinitionWithSummaries modSummaries source

resolveReferenceMatchesForNames :: (MonadLore m) => [GHC.Name] -> m [ReferenceMatch]
resolveReferenceMatchesForNames targetNames = do
  logTimedSectionStart "findReferences:getModSummaries"
  ModSummaries modSummaries <- getModSummaries
  logTimedSectionEnd "findReferences:getModSummaries"
  let targetSet = Set.fromList targetNames
      targetOccKeys = Set.fromList (map nameOccKey targetNames)
  logTimedSectionStart "findReferences:getParsedOccurrenceModuleIndex"
  ParsedOccurrenceModuleIndex occurrenceIndex <-
    getParsedOccurrenceModuleIndex (buildParsedOccurrenceModuleIndex modSummaries)
  logTimedSectionEnd "findReferences:getParsedOccurrenceModuleIndex"
  let candidateModules = lookupModulesForOccurrenceKeys targetOccKeys occurrenceIndex
  Log.debug $ "Resolved " <> show (Set.size targetOccKeys) <> " target occurrence names to " <> show (length candidateModules) <> " candidate modules."
  logTimedSectionStart "findReferences:prepareCandidateModules"
  preparedModules <-
    prepareCandidateModuleIndexes modSummaries candidateModules
  logTimedSectionEnd "findReferences:prepareCandidateModules"
  logTimedSectionStart "findReferences:analyzePreparedModules"
  resolvedMatches <-
    concat <$> pooledForConcurrently preparedModules (liftIO . Exception.evaluate . matchingReferenceMatches targetSet)
  logTimedSectionEnd "findReferences:analyzePreparedModules"
  logTimedSectionStart "findReferences:forceMatches"
  forcedMatches <- liftIO $ Exception.evaluate (forceReferenceMatchesForRendering resolvedMatches)
  logTimedSectionEnd "findReferences:forceMatches"
  Log.debug $
    "Finished resolving reference matches. Found "
      <> show (length forcedMatches)
      <> " matches in total"
  pure forcedMatches

matchingReferenceMatches :: Set.Set GHC.Name -> DefinitionModuleIndex -> [ReferenceMatch]
matchingReferenceMatches targetSet moduleAnalysis =
  [ ReferenceMatch
      { referenceMatchDefinition = source,
        referenceMatchOccurrences = dedupeReferenceHits occurrences
      }
  | (source, occurrences) <- Map.elems occurrencesByDefinition
  ]
  where
    occurrencesByDefinition =
      Map.fromListWith
        mergeOccurrences
        [ ( reference.referenceHitDefinitionId,
            (source, [reference])
          )
        | targetName <- Set.toList targetSet,
          reference <- Map.findWithDefault [] (nameOccKey targetName) moduleAnalysis.referenceHitsByOccKey,
          reference.referenceHitTargetName == targetName,
          Just source <- [Map.lookup reference.referenceHitDefinitionId moduleAnalysis.definitionsById]
        ]

    mergeOccurrences (source, newOccurrences) (_, oldOccurrences) =
      (source, oldOccurrences <> newOccurrences)

dedupeReferenceHits :: [ReferenceHit] -> [ReferenceHit]
dedupeReferenceHits =
  reverse . snd . foldl' go (Set.empty, [])
  where
    go (seen, occurrences) occurrence
      | occurrenceKey `Set.member` seen =
          (seen, occurrences)
      | otherwise =
          (Set.insert occurrenceKey seen, occurrence : occurrences)
      where
        occurrenceKey =
          ( occurrence.referenceHitTargetName,
            srcSpanKey occurrence.referenceHitExactSpan
          )

lookupModulesForOccurrenceKeys :: Set.Set OccKey -> Map.Map OccKey (Set.Set GHC.Module) -> [GHC.Module]
lookupModulesForOccurrenceKeys targetOccKeys occurrenceIndex =
  Set.toList $
    foldl'
      (\modules occKey -> modules <> Map.findWithDefault Set.empty occKey occurrenceIndex)
      Set.empty
      (Set.toList targetOccKeys)

resolveDefinitionClosureSourcesNamed :: (MonadLore m) => Int -> GHC.Name -> m [NamedDefinitionSource]
resolveDefinitionClosureSourcesNamed maxDepth inputName = do
  ModSummaries modSummaries <- getModSummaries
  resolveDefinitionClosureSourcesWithSummaries modSummaries maxDepth inputName

resolveDefinitionClosureSourcesWithSummaries ::
  (MonadLore m) =>
  Map.Map GHC.Module GHC.ModSummary ->
  Int ->
  GHC.Name ->
  m [NamedDefinitionSource]
resolveDefinitionClosureSourcesWithSummaries modSummaries maxDepth inputName = do
  let depth = max 0 maxDepth
  result <- go depth inputName (ClosureState Set.empty [])
  maybeRootSource <- resolveDefinitionSourceWithSummaries modSummaries inputName
  pure $
    case maybeRootSource of
      Nothing -> result.closureSources
      Just rootSource -> dependenciesFirst rootSource.definitionSourceId result.closureSources
  where
    dependenciesFirst rootDefinitionId closureSources =
      let (rootEntries, dependencyEntries) =
            List.partition ((== rootDefinitionId) . (.definitionSource.definitionSourceId)) closureSources
       in -- Keep dependencies ahead of the queried root in closure output.
          dependencyEntries <> rootEntries

    go depth name state = do
      maybeSource <- resolveDefinitionSourceWithSummaries modSummaries name
      case maybeSource of
        Nothing ->
          pure state
        Just source ->
          let definitionId = source.definitionSourceId
           in if Set.member definitionId state.closureSeen
                then pure state
                else
                  if depth == 0
                    then appendClosureSource name source state
                    else do
                      dependencyNames <- resolveDefinitionDependencyNames modSummaries source
                      result <-
                        foldlM
                          (go (depth - 1))
                          state
                            { closureSeen = Set.insert definitionId state.closureSeen,
                              closureSources = []
                            }
                          dependencyNames
                      let namedSource =
                            NamedDefinitionSource
                              { definitionName = name,
                                definitionSource = source
                              }
                      pure
                        result
                          { closureSources =
                              state.closureSources
                                <> (namedSource : result.closureSources)
                          }

    appendClosureSource name source state =
      pure
        state
          { closureSeen = Set.insert source.definitionSourceId state.closureSeen,
            closureSources =
              state.closureSources
                <> [ NamedDefinitionSource
                       { definitionName = name,
                         definitionSource = source
                       }
                   ]
          }

    foldlM f = go'
      where
        go' acc [] = pure acc
        go' acc (x : xs) = do
          acc' <- f x acc
          go' acc' xs

mergeDefinitionSlices :: [DefinitionSlice] -> Maybe DefinitionSlice
mergeDefinitionSlices [] = Nothing
mergeDefinitionSlices (slice : slices)
  | all ((== slice.definitionModule) . definitionModule) slices =
      Just
        DefinitionSlice
          { definitionModule = slice.definitionModule,
            declarationSpans =
              dedupeDeclarationSpans . sortDeclarationSpans $
                concatMap declarationSpans allSlices,
            requiredImports =
              mergeImports $
                concatMap requiredImports allSlices
          }
  | otherwise =
      Nothing
  where
    allSlices = slice : slices

resolveDefinitionSourceWithSummaries ::
  (MonadLore m) =>
  Map.Map GHC.Module GHC.ModSummary ->
  GHC.Name ->
  m (Maybe DefinitionSource)
resolveDefinitionSourceWithSummaries modSummaries inputName =
  case GHC.nameModule_maybe inputName of
    Nothing ->
      pure Nothing
    Just definingModule -> do
      maybeModuleIndex <- getDefinitionModuleIndex modSummaries definingModule
      pure do
        moduleIndex <- maybeModuleIndex
        definitionId <- Map.lookup inputName moduleIndex.definitionIdByName
        Map.lookup definitionId moduleIndex.definitionsById

resolveDefinitionDependencyNames ::
  (MonadLore m) =>
  Map.Map GHC.Module GHC.ModSummary ->
  DefinitionSource ->
  m [GHC.Name]
resolveDefinitionDependencyNames modSummaries source = do
  maybeModuleIndex <- getDefinitionModuleIndex modSummaries source.definitionSourceModule
  pure case maybeModuleIndex >>= Map.lookup source.definitionSourceId . (.dependenciesById) of
    Nothing -> []
    Just dependencies ->
      Set.toList (dependencies.dependencyDirectReferenceNames <> dependencies.dependencyUsedInstanceNames)

getMinifiedImportsForDefinitionWithSummaries ::
  (MonadLore m) =>
  Map.Map GHC.Module GHC.ModSummary ->
  DefinitionSource ->
  m [RequiredImport]
getMinifiedImportsForDefinitionWithSummaries modSummaries source = do
  maybeModuleIndex <- getDefinitionModuleIndex modSummaries source.definitionSourceModule
  pure $
    maybe
      []
      (`DefinitionAnalysis.getMinifiedImportsForDefinition` source.definitionSourceId)
      maybeModuleIndex

buildParsedOccurrenceModuleIndex ::
  (MonadLore m) =>
  Map.Map GHC.Module GHC.ModSummary ->
  m ParsedOccurrenceModuleIndex
buildParsedOccurrenceModuleIndex modSummaries = do
  parsedModuleCacheVar <- asks referenceParsedModuleCache
  parsedModuleCache <- liftIO (readMVar parsedModuleCacheVar)
  Log.debug $ "Building reference occurrence index for " <> show (Map.size modSummaries) <> " modules."
  let occurrenceIndex =
        foldl' (buildModuleIndex parsedModuleCache) Map.empty (Map.keys modSummaries)
  Log.debug $ "Finished building reference occurrence index. Indexed " <> show (Map.size occurrenceIndex) <> " unique occurrence names."
  pure (ParsedOccurrenceModuleIndex occurrenceIndex)
  where
    buildModuleIndex parsedModuleCache occurrenceIndex homeModule =
      case Map.lookup homeModule parsedModuleCache of
        Nothing -> occurrenceIndex
        Just parsedModule ->
          foldl'
            (\index occName -> Map.insertWith (<>) occName (Set.singleton homeModule) index)
            occurrenceIndex
            (Set.toList (moduleOccurrenceNames parsedModule))

prepareCandidateModuleIndexes ::
  (MonadLore m) =>
  Map.Map GHC.Module GHC.ModSummary ->
  [GHC.Module] ->
  m [DefinitionModuleIndex]
prepareCandidateModuleIndexes modSummaries homeModules =
  catMaybes <$> traverse (getDefinitionModuleIndex modSummaries) homeModules

getDefinitionModuleIndex ::
  (MonadLore m) =>
  Map.Map GHC.Module GHC.ModSummary ->
  GHC.Module ->
  m (Maybe DefinitionModuleIndex)
getDefinitionModuleIndex modSummaries homeModule = do
  cachedModuleIndex <- lookupDefinitionModuleIndexCache homeModule
  case cachedModuleIndex of
    Just moduleIndex ->
      pure moduleIndex
    Nothing ->
      buildCachedDefinitionModuleIndex modSummaries homeModule

buildCachedDefinitionModuleIndex ::
  (MonadLore m) =>
  Map.Map GHC.Module GHC.ModSummary ->
  GHC.Module ->
  m (Maybe DefinitionModuleIndex)
buildCachedDefinitionModuleIndex modSummaries homeModule
  | Map.notMember homeModule modSummaries =
      pure Nothing
  | otherwise = do
      maybeArtifacts <- loadCachedDefinitionModuleArtifacts
      case maybeArtifacts of
        Just CachedDefinitionModuleArtifacts {cachedParsedModuleFacts, cachedMinimalTypedFacts, cachedMinimalCoreFacts} -> do
          let moduleIndex =
                buildDefinitionModuleIndex homeModule cachedParsedModuleFacts cachedMinimalTypedFacts cachedMinimalCoreFacts
          cacheDefinitionModuleIndex homeModule (Just moduleIndex)
          pure (Just moduleIndex)
        Nothing -> do
          Log.debug $ "Cached definition artifacts missing for " <> GHC.moduleNameString (GHC.moduleName homeModule)
          cacheDefinitionModuleIndex homeModule Nothing
          pure Nothing
  where
    loadCachedDefinitionModuleArtifacts = do
      typedModuleCacheVar <- asks referenceTypedModuleCache
      maybeParsedFacts <- getParsedModuleFacts homeModule
      maybeMinimalTypedFacts <- lookupMinimalTypedFacts typedModuleCacheVar
      maybeCoreFacts <- lookupMinimalCoreFacts
      pure do
        cachedParsedModuleFacts <- maybeParsedFacts
        cachedMinimalTypedFacts <- maybeMinimalTypedFacts
        pure CachedDefinitionModuleArtifacts {cachedParsedModuleFacts, cachedMinimalTypedFacts, cachedMinimalCoreFacts = maybeCoreFacts}

    lookupMinimalCoreFacts = do
      coreFactsCacheVar <- asks referenceMinimalCoreModuleFactsCache
      coreFactsByModule <- liftIO (readMVar coreFactsCacheVar)
      pure (Map.lookup homeModule coreFactsByModule)

    lookupMinimalTypedFacts typedModuleCacheVar = do
      typedModuleCache <- liftIO (readMVar typedModuleCacheVar)
      case Map.lookup homeModule typedModuleCache of
        Just (TypedModuleMinimalFacts minimalFacts) -> pure (Just minimalFacts)
        Nothing -> pure Nothing

moduleOccurrenceNames :: ParsedModuleCache -> Set.Set OccKey
moduleOccurrenceNames = \case
  ParsedModuleFactsCache parsedFacts ->
    parsedFacts.parsedOccKeys

logTimedSectionStart :: (MonadLore m) => String -> m ()
logTimedSectionStart label = do
  now <- liftIO getCurrentTime
  Log.debug $ "Starting " <> label <> " at " <> show now

logTimedSectionEnd :: (MonadLore m) => String -> m ()
logTimedSectionEnd label = do
  now <- liftIO getCurrentTime
  Log.debug $ "Finished " <> label <> " at " <> show now

mergeImports :: [RequiredImport] -> [RequiredImport]
mergeImports =
  IntMap.elems . foldl insertImport IntMap.empty
  where
    insertImport acc import' =
      IntMap.insertWith mergeImport import'.importKey import' acc

    mergeImport new old =
      old
        { importOriginallyExplicit = old.importOriginallyExplicit || new.importOriginallyExplicit,
          importItems = normalizeImportItems (old.importItems <> new.importItems)
        }

forceReferenceMatchesForRendering :: [ReferenceMatch] -> [ReferenceMatch]
forceReferenceMatchesForRendering matches =
  forceMatches matches `seq` matches
  where
    forceMatches [] = ()
    forceMatches (referenceMatch : restMatches) =
      referenceMatch.referenceMatchDefinition.definitionSourceModule `deepseq`
        referenceMatch.referenceMatchDefinition.definitionSourceSpans `deepseq`
          referenceMatch.referenceMatchOccurrences `deepseq`
            forceMatches restMatches

sortDeclarationSpans :: [DeclarationSpans] -> [DeclarationSpans]
sortDeclarationSpans =
  List.sortOn (GHC.srcSpanToRealSrcSpan . declarationSpan)

dedupeDeclarationSpans :: [DeclarationSpans] -> [DeclarationSpans]
dedupeDeclarationSpans =
  nubOrdOn (\declarationSpans -> (show declarationSpans.declarationSpan, fmap show declarationSpans.signatureSpan))
