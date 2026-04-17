module Lore.Definition
  ( resolveDefinitionSlice,
    resolveDefinitionSliceNamed,
    resolveReferenceMatchesForNames,
    resolveDefinitionClosure,
    resolveDefinitionClosureNamed,
    mergeDefinitionSlices,
    DefinitionSlice (..),
    NamedDefinitionSlice (..),
    DeclarationSpans (..),
    ReferenceMatch (..),
    RequiredImport (..),
    ImportQualifiedStyle (..),
    RequiredImportItem (..),
    resolveInstanceDefinitions,
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
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import qualified GHC
import qualified GHC.Plugins as GHC
import Lore.Internal.Definition.Analysis (buildParsedModuleSummary, buildProcessedTypedDefinitionFacts, buildReferenceModuleAnalysis, collectParsedOccurrenceNames, mergeReferenceModuleAnalysisWithCoreFacts, normalizeImportItems)
import Lore.Internal.Definition.Cache (cacheReferenceModuleAnalysis, getReferenceOccurrenceIndex, lookupReferenceModuleAnalysisCache)
import Lore.Internal.Definition.Types (DeclarationSpans (..), DefinitionAnalysis (..), DefinitionSlice (..), ImportQualifiedStyle (..), MinimalCoreModuleFacts, NamedDefinitionSlice (..), ParsedModuleCache (..), ParsedModuleSummary (..), ProcessedTypedDefinitionFacts, ReferenceMatch (..), ReferenceModuleAnalysis (..), ReferenceOccurrenceIndex (..), RequiredImport (..), RequiredImportItem (..), TypedModuleCache (..), parsedModuleOccurrenceNames)
import Lore.Internal.Lookup.ModSummaries (getModSummaries)
import Lore.Internal.Lookup.Types (ModSummaries (..))
import Lore.Internal.Session (SessionContext (..))
import qualified Lore.Logger as Log
import Lore.Lookup (Instances (..), listAssociatedInstances)
import Lore.Monad
import UnliftIO (modifyMVar_, pooledForConcurrently, readMVar)

data ResolverCache = ResolverCache
  { cachedAnalyses :: Map.Map GHC.Name (Maybe DefinitionAnalysis)
  }

data DefinitionKey = DefinitionKey
  { definitionKeyModule :: GHC.Module,
    definitionKeySpan :: Maybe GHC.RealSrcSpan
  }
  deriving stock (Eq, Ord)

data ClosureState = ClosureState
  { closureCache :: ResolverCache,
    closureSeen :: Set.Set ClosureEntryKey,
    closureSlices :: [NamedDefinitionSlice]
  }

data ClosureEntryKey = ClosureEntryKey
  { closureEntryDefinitionKey :: DefinitionKey,
    closureEntryName :: GHC.Name
  }
  deriving stock (Eq, Ord)

data CachedReferenceModuleArtifacts = CachedReferenceModuleArtifacts
  { cachedParsedModuleSummary :: ParsedModuleSummary,
    cachedProcessedTypedFacts :: Map.Map GHC.Name ProcessedTypedDefinitionFacts,
    cachedMinimalCoreFacts :: MinimalCoreModuleFacts
  }

resolveDefinitionSlice :: (MonadLore m) => GHC.Name -> m (Maybe DefinitionSlice)
resolveDefinitionSlice inputName = do
  fmap (.definitionSlice) <$> resolveDefinitionSliceNamed inputName

resolveDefinitionSliceNamed :: (MonadLore m) => GHC.Name -> m (Maybe NamedDefinitionSlice)
resolveDefinitionSliceNamed inputName = do
  ModSummaries modSummaries <- getModSummaries
  (_, analysis) <- resolveDefinitionAnalysis modSummaries emptyResolverCache inputName
  pure (fmap (\resolvedAnalysis -> NamedDefinitionSlice {definitionName = inputName, definitionSlice = resolvedAnalysis.analysisSlice}) analysis)

resolveReferenceMatchesForNames :: (MonadLore m) => [GHC.Name] -> m [ReferenceMatch]
resolveReferenceMatchesForNames targetNames = do
  logTimedSectionStart "findReferences:getModSummaries"
  ModSummaries modSummaries <- getModSummaries
  logTimedSectionEnd "findReferences:getModSummaries"
  let targetSet = Set.fromList targetNames
      targetOccNames = targetOccurrenceNames targetNames
  logTimedSectionStart "findReferences:getReferenceOccurrenceIndex"
  ReferenceOccurrenceIndex occurrenceIndex <-
    getReferenceOccurrenceIndex (buildReferenceOccurrenceIndex modSummaries)
  logTimedSectionEnd "findReferences:getReferenceOccurrenceIndex"
  let candidateModules = lookupModulesForOccurrenceNames targetOccNames occurrenceIndex
  Log.debug $ "Resolved " <> show (Set.size targetOccNames) <> " target occurrence names to " <> show (length candidateModules) <> " candidate modules."
  logTimedSectionStart "findReferences:prepareCandidateModules"
  preparedModules <-
    prepareReferenceModules modSummaries candidateModules
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

matchingReferenceMatches :: Set.Set GHC.Name -> ReferenceModuleAnalysis -> [ReferenceMatch]
matchingReferenceMatches targetSet moduleAnalysis =
  mergeReferenceMatchesBySlice $
    mapMaybe (mkReferenceMatch targetSet) (Map.elems moduleAnalysis.referenceModuleDefinitions)

mkReferenceMatch :: Set.Set GHC.Name -> Maybe DefinitionAnalysis -> Maybe ReferenceMatch
mkReferenceMatch _ Nothing = Nothing
mkReferenceMatch targetSet (Just definitionAnalysis) =
  case concatMap (\targetName -> Map.findWithDefault [] targetName definitionAnalysis.analysisReferenceSpans) (Set.toList targetSet) of
    [] ->
      Nothing
    matchedSpans ->
      Just
        ReferenceMatch
          { referenceSlice = definitionAnalysis.analysisSlice,
            matchedReferenceSpans = dedupeSpans matchedSpans,
            matchedReferenceUsageSpans =
              dedupeSpans $
                concatMap
                  (\targetName -> Map.findWithDefault [] targetName definitionAnalysis.analysisReferenceUsageSpans)
                  (Set.toList targetSet),
            matchedReferenceSectionSpans =
              dedupeSpans $
                concatMap
                  (\targetName -> Map.findWithDefault [] targetName definitionAnalysis.analysisReferenceSectionSpans)
                  (Set.toList targetSet)
          }

targetOccurrenceNames :: [GHC.Name] -> Set.Set Text
targetOccurrenceNames =
  Set.fromList
    . map (T.pack . GHC.occNameString . GHC.nameOccName)

lookupModulesForOccurrenceNames :: Set.Set Text -> Map.Map Text (Set.Set GHC.Module) -> [GHC.Module]
lookupModulesForOccurrenceNames targetOccNames occurrenceIndex =
  Set.toList $
    foldl'
      (\modules occName -> modules <> Map.findWithDefault Set.empty occName occurrenceIndex)
      Set.empty
      (Set.toList targetOccNames)

resolveDefinitionClosure :: (MonadLore m) => Int -> GHC.Name -> m [DefinitionSlice]
resolveDefinitionClosure maxDepth inputName = do
  namedSlices <- resolveDefinitionClosureNamed maxDepth inputName
  pure (mergeSlicesByModule (map (.definitionSlice) namedSlices))

resolveDefinitionClosureNamed :: (MonadLore m) => Int -> GHC.Name -> m [NamedDefinitionSlice]
resolveDefinitionClosureNamed maxDepth inputName = do
  ModSummaries modSummaries <- getModSummaries
  let depth = max 0 maxDepth
  result <- go modSummaries depth inputName (ClosureState emptyResolverCache Set.empty [])
  pure result.closureSlices
  where
    go modSummaries depth name state = do
      (cache', analysis) <- resolveDefinitionAnalysis modSummaries state.closureCache name
      case analysis of
        Nothing ->
          pure state {closureCache = cache'}
        Just definitionAnalysis ->
          let slice = analysisSlice definitionAnalysis
              key =
                ClosureEntryKey
                  { closureEntryDefinitionKey = definitionKey slice,
                    closureEntryName = name
                  }
           in if Set.member key state.closureSeen
                then pure state {closureCache = cache'}
                else
                  if depth == 0
                    then
                      pure
                        state
                          { closureCache = cache',
                            closureSeen = Set.insert key state.closureSeen,
                            closureSlices =
                              state.closureSlices
                                <> [ NamedDefinitionSlice
                                       { definitionName = name,
                                         definitionSlice = slice
                                       }
                                   ]
                          }
                    else do
                      result <-
                        foldlM
                          (go modSummaries (depth - 1))
                          state
                            { closureCache = cache',
                              closureSeen = Set.insert key state.closureSeen,
                              closureSlices = []
                            }
                          (analysisRecursiveNames definitionAnalysis)
                      pure
                        result
                          { closureSlices =
                              state.closureSlices
                                <> ( NamedDefinitionSlice
                                       { definitionName = name,
                                         definitionSlice = slice
                                       }
                                       : result.closureSlices
                                   )
                          }

    foldlM f = go'
      where
        go' acc [] = pure acc
        go' acc (x : xs) = do
          acc' <- f x acc
          go' acc' xs

analysisRecursiveNames :: DefinitionAnalysis -> [GHC.Name]
analysisRecursiveNames definitionAnalysis =
  dedupeNames
    ( definitionAnalysis.analysisReferences
        <> definitionAnalysis.analysisUsedInstances
    )

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

mergeSlicesByModule :: [DefinitionSlice] -> [DefinitionSlice]
mergeSlicesByModule =
  Map.elems . foldl insertSlice Map.empty
  where
    insertSlice acc slice =
      Map.insertWith mergeTwo slice.definitionModule slice acc

    mergeTwo new old =
      fromMaybe old $ mergeDefinitionSlices [old, new]

resolveDefinitionAnalysis ::
  (MonadLore m) =>
  Map.Map GHC.Module GHC.ModSummary ->
  ResolverCache ->
  GHC.Name ->
  m (ResolverCache, Maybe DefinitionAnalysis)
resolveDefinitionAnalysis modSummaries cache inputName =
  case Map.lookup inputName cache.cachedAnalyses of
    Just analysis ->
      pure (cache, analysis)
    Nothing ->
      case GHC.nameModule_maybe inputName of
        Nothing ->
          pure (rememberAnalysis Nothing)
        Just definingModule -> do
          maybeModuleAnalysis <- getReferenceModuleAnalysis modSummaries definingModule
          let analysis = do
                moduleAnalysis <- maybeModuleAnalysis
                Map.lookup inputName moduleAnalysis.referenceModuleDefinitions >>= id
          pure
            ( cache
                { cachedAnalyses =
                    Map.insert inputName analysis cache.cachedAnalyses
                },
              analysis
            )
  where
    rememberAnalysis analysis =
      ( cache
          { cachedAnalyses =
              Map.insert inputName analysis cache.cachedAnalyses
          },
        analysis
      )

buildReferenceOccurrenceIndex ::
  (MonadLore m) =>
  Map.Map GHC.Module GHC.ModSummary ->
  m ReferenceOccurrenceIndex
buildReferenceOccurrenceIndex modSummaries = do
  parsedModuleCacheVar <- asks referenceParsedModuleCache
  parsedModuleCache <- liftIO (readMVar parsedModuleCacheVar)
  Log.debug $ "Building reference occurrence index for " <> show (Map.size modSummaries) <> " modules."
  let occurrenceIndex =
        foldl' (buildModuleIndex parsedModuleCache) Map.empty (Map.keys modSummaries)
  Log.debug $ "Finished building reference occurrence index. Indexed " <> show (Map.size occurrenceIndex) <> " unique occurrence names."
  pure (ReferenceOccurrenceIndex occurrenceIndex)
  where
    buildModuleIndex parsedModuleCache occurrenceIndex homeModule =
      case Map.lookup homeModule parsedModuleCache of
        Nothing -> occurrenceIndex
        Just parsedModule ->
          foldl'
            (\index occName -> Map.insertWith (<>) occName (Set.singleton homeModule) index)
            occurrenceIndex
            (Set.toList (moduleOccurrenceNames parsedModule))

prepareReferenceModules ::
  (MonadLore m) =>
  Map.Map GHC.Module GHC.ModSummary ->
  [GHC.Module] ->
  m [ReferenceModuleAnalysis]
prepareReferenceModules modSummaries homeModules =
  catMaybes <$> traverse (getReferenceModuleAnalysis modSummaries) homeModules

getReferenceModuleAnalysis ::
  (MonadLore m) =>
  Map.Map GHC.Module GHC.ModSummary ->
  GHC.Module ->
  m (Maybe ReferenceModuleAnalysis)
getReferenceModuleAnalysis modSummaries homeModule = do
  cachedModuleAnalysis <- lookupReferenceModuleAnalysisCache homeModule
  case cachedModuleAnalysis of
    Just moduleAnalysis ->
      pure moduleAnalysis
    Nothing ->
      buildCachedReferenceModuleAnalysis modSummaries homeModule

buildCachedReferenceModuleAnalysis ::
  (MonadLore m) =>
  Map.Map GHC.Module GHC.ModSummary ->
  GHC.Module ->
  m (Maybe ReferenceModuleAnalysis)
buildCachedReferenceModuleAnalysis modSummaries homeModule
  | Map.notMember homeModule modSummaries =
      pure Nothing
  | otherwise = do
      maybeArtifacts <- loadCachedReferenceModuleArtifacts
      case maybeArtifacts of
        Just CachedReferenceModuleArtifacts {cachedParsedModuleSummary, cachedProcessedTypedFacts, cachedMinimalCoreFacts} -> do
          let moduleAnalysis =
                mergeReferenceModuleAnalysisWithCoreFacts
                  cachedMinimalCoreFacts
                  (buildReferenceModuleAnalysis homeModule cachedParsedModuleSummary cachedProcessedTypedFacts)
          cacheReferenceModuleAnalysis homeModule (Just moduleAnalysis)
          pure (Just moduleAnalysis)
        Nothing -> do
          Log.debug $ "Cached definition artifacts missing for " <> GHC.moduleNameString (GHC.moduleName homeModule)
          cacheReferenceModuleAnalysis homeModule Nothing
          pure Nothing
  where
    loadCachedReferenceModuleArtifacts = do
      parsedModuleCacheVar <- asks referenceParsedModuleCache
      typedModuleCacheVar <- asks referenceTypedModuleCache
      maybeParsedSummary <- prepareParsedModuleSummary parsedModuleCacheVar
      maybeProcessedTypedFacts <- prepareProcessedTypedFacts typedModuleCacheVar maybeParsedSummary
      maybeCoreFacts <- lookupMinimalCoreFacts
      pure do
        cachedParsedModuleSummary <- maybeParsedSummary
        cachedProcessedTypedFacts <- maybeProcessedTypedFacts
        cachedMinimalCoreFacts <- maybeCoreFacts
        pure CachedReferenceModuleArtifacts {cachedParsedModuleSummary, cachedProcessedTypedFacts, cachedMinimalCoreFacts}

    lookupMinimalCoreFacts = do
      coreFactsCacheVar <- asks referenceMinimalCoreModuleFactsCache
      coreFactsByModule <- liftIO (readMVar coreFactsCacheVar)
      pure (Map.lookup homeModule coreFactsByModule)

    prepareParsedModuleSummary parsedModuleCacheVar = do
      parsedModuleCache <- liftIO (readMVar parsedModuleCacheVar)
      case Map.lookup homeModule parsedModuleCache of
        Just (ParsedModuleProcessed parsedSummary) -> pure (Just parsedSummary)
        Just (ParsedModuleRaw parsedSource) -> do
          Log.debug $ "Starting parsed summary build for " <> GHC.moduleNameString (GHC.moduleName homeModule)
          parsedSummary <- liftIO $ Exception.evaluate (buildParsedModuleSummary parsedSource)
          Log.debug $ "Finished parsed summary build for " <> GHC.moduleNameString (GHC.moduleName homeModule)
          liftIO $
            modifyMVar_ parsedModuleCacheVar \cache ->
              let cache' = Map.insert homeModule (ParsedModuleProcessed parsedSummary) cache
               in Exception.evaluate cache'
          pure (Just parsedSummary)
        Nothing -> pure Nothing

    prepareProcessedTypedFacts typedModuleCacheVar maybeParsedSummary = do
      typedModuleCache <- liftIO (readMVar typedModuleCacheVar)
      case Map.lookup homeModule typedModuleCache of
        Just (TypedModuleProcessedData processedFacts) -> pure (Just processedFacts)
        Just (TypedModuleMinimalFacts minimalFacts) ->
          case maybeParsedSummary of
            Nothing -> pure Nothing
            Just parsedSummary -> do
              Log.debug $ "Starting processed typed facts build for " <> GHC.moduleNameString (GHC.moduleName homeModule)
              let processedFacts =
                    buildProcessedTypedDefinitionFacts
                      homeModule
                      parsedSummary
                      minimalFacts
              _ <- liftIO $ Exception.evaluate processedFacts
              Log.debug $ "Finished processed typed facts build for " <> GHC.moduleNameString (GHC.moduleName homeModule)
              liftIO $
                modifyMVar_ typedModuleCacheVar \cache ->
                  let cache' = Map.insert homeModule (TypedModuleProcessedData processedFacts) cache
                   in Exception.evaluate cache'
              pure (Just processedFacts)
        Nothing -> pure Nothing

moduleOccurrenceNames :: ParsedModuleCache -> Set.Set Text
moduleOccurrenceNames = \case
  ParsedModuleRaw parsedSource ->
    collectParsedOccurrenceNames parsedSource
  ParsedModuleProcessed parsedSummary ->
    parsedSummary.parsedModuleOccurrenceNames

logTimedSectionStart :: (MonadLore m) => String -> m ()
logTimedSectionStart label = do
  now <- liftIO getCurrentTime
  Log.debug $ "Starting " <> label <> " at " <> show now

logTimedSectionEnd :: (MonadLore m) => String -> m ()
logTimedSectionEnd label = do
  now <- liftIO getCurrentTime
  Log.debug $ "Finished " <> label <> " at " <> show now

definitionKey :: DefinitionSlice -> DefinitionKey
definitionKey slice =
  DefinitionKey
    { definitionKeyModule = slice.definitionModule,
      definitionKeySpan =
        GHC.srcSpanToRealSrcSpan . declarationSpan . head $ slice.declarationSpans
    }

emptyResolverCache :: ResolverCache
emptyResolverCache =
  ResolverCache
    { cachedAnalyses = Map.empty
    }

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

dedupeNames :: [GHC.Name] -> [GHC.Name]
dedupeNames =
  Map.elems . Map.fromList . map (\n -> (GHC.occNameString (GHC.nameOccName n), n))

forceReferenceMatchesForRendering :: [ReferenceMatch] -> [ReferenceMatch]
forceReferenceMatchesForRendering matches =
  forceMatches matches `seq` matches
  where
    forceMatches [] = ()
    forceMatches (referenceMatch : restMatches) =
      referenceMatch.referenceSlice.definitionModule `deepseq`
        referenceMatch.referenceSlice.declarationSpans `deepseq`
          referenceMatch.matchedReferenceSpans `deepseq`
            referenceMatch.matchedReferenceUsageSpans `deepseq`
              referenceMatch.matchedReferenceSectionSpans `deepseq`
                forceMatches restMatches

dedupeSpans :: [GHC.SrcSpan] -> [GHC.SrcSpan]
dedupeSpans =
  nubOrdOn show

mergeReferenceMatchesBySlice :: [ReferenceMatch] -> [ReferenceMatch]
mergeReferenceMatchesBySlice =
  Map.elems . foldl insertMatch Map.empty
  where
    insertMatch acc referenceMatch =
      Map.insertWith mergeTwo (referenceMatchKey referenceMatch) referenceMatch acc

    mergeTwo new old =
      old
        { matchedReferenceSpans = dedupeSpans (old.matchedReferenceSpans <> new.matchedReferenceSpans),
          matchedReferenceUsageSpans = dedupeSpans (old.matchedReferenceUsageSpans <> new.matchedReferenceUsageSpans),
          matchedReferenceSectionSpans = dedupeSpans (old.matchedReferenceSectionSpans <> new.matchedReferenceSectionSpans)
        }

referenceMatchKey :: ReferenceMatch -> (GHC.Module, [(String, Maybe String)])
referenceMatchKey referenceMatch =
  ( referenceMatch.referenceSlice.definitionModule,
    map declarationKey referenceMatch.referenceSlice.declarationSpans
  )
  where
    declarationKey declarationSpans =
      ( show declarationSpans.declarationSpan,
        fmap show declarationSpans.signatureSpan
      )

sortDeclarationSpans :: [DeclarationSpans] -> [DeclarationSpans]
sortDeclarationSpans =
  List.sortOn (GHC.srcSpanToRealSrcSpan . declarationSpan)

dedupeDeclarationSpans :: [DeclarationSpans] -> [DeclarationSpans]
dedupeDeclarationSpans =
  nubOrdOn (\declarationSpans -> (show declarationSpans.declarationSpan, fmap show declarationSpans.signatureSpan))

resolveInstanceDefinitions :: (MonadLore m) => GHC.Name -> m [DefinitionSlice]
resolveInstanceDefinitions name = do
  instances <- listAssociatedInstances name
  let allNames = [GHC.getName clsInst | clsInst <- instances.classInstances] ++ [GHC.getName famInst | famInst <- instances.familyInstances]
  resolved <- mapM resolveDefinitionSlice allNames
  pure $ catMaybes resolved
