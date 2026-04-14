module Lore.Definition
  ( resolveDefinitionSlice,
    resolveReferenceMatches,
    resolveReferenceMatchesForNames,
    resolveReferenceDefinitions,
    resolveReferenceDefinitionsForNames,
    resolveDefinitionClosure,
    mergeDefinitionSlices,
    renderDeclarationSpansText,
    renderDefinitionSliceText,
    renderDefinitionModuleText,
    renderDefinitionModulesText,
    renderImport,
    DefinitionSlice (..),
    DeclarationSpans (..),
    ReferenceMatch (..),
    RequiredImport,
  )
where

import Control.Applicative ((<|>))
import Control.DeepSeq (deepseq)
import qualified Control.Exception as Exception
import Control.Monad (foldM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Containers.ListUtils (nubOrdOn)
import qualified Data.IntMap.Strict as IntMap
import Data.List (foldl')
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import qualified GHC
import qualified GHC.Plugins as GHC
import Lore.Internal.Definition.Analysis (buildParsedModuleSummary, buildProcessedTypedDefinitionFacts, buildReferenceModuleAnalysis, collectParsedOccurrenceNames, mergeReferenceModuleAnalysisWithCoreFacts, normalizeImportItems)
import Lore.Internal.Definition.Cache (cacheReferenceModuleAnalysis, getReferenceOccurrenceIndex, lookupReferenceModuleAnalysisCache)
import Lore.Internal.Definition.Types (DeclarationSpans (..), DefinitionAnalysis (..), DefinitionSlice (..), ImportQualifiedStyle (..), ParsedModuleCache (..), ReferenceMatch (..), ReferenceModuleAnalysis (..), ReferenceOccurrenceIndex (..), RequiredImport (..), RequiredImportItem (..), TypedModuleCache (..), parsedModuleOccurrenceNames)
import Lore.Internal.Lookup.ModSummaries (getModSummaries)
import Lore.Internal.Lookup.Types (ModSummaries (..))
import Lore.Internal.Session (SessionContext (..))
import qualified Lore.Logger as Log
import Lore.Monad
import System.Directory (getCurrentDirectory)
import System.FilePath (isRelative, makeRelative, normalise)
import UnliftIO (forConcurrently, modifyMVar_, readMVar)

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
    closureSeen :: Set.Set DefinitionKey,
    closureSlices :: [DefinitionSlice]
  }

newtype PreparedReferenceModule = PreparedReferenceModule
  { preparedReferenceModuleAnalysis :: ReferenceModuleAnalysis
  }

resolveDefinitionSlice :: (MonadLore m) => GHC.Name -> m (Maybe DefinitionSlice)
resolveDefinitionSlice inputName = do
  ModSummaries modSummaries <- getModSummaries
  (_, analysis) <- resolveDefinitionAnalysis modSummaries emptyResolverCache inputName
  pure (analysisSlice <$> analysis)

resolveReferenceDefinitions :: (MonadLore m) => GHC.Name -> m [DefinitionSlice]
resolveReferenceDefinitions targetName =
  resolveReferenceDefinitionsForNames [targetName]

resolveReferenceMatches :: (MonadLore m) => GHC.Name -> m [ReferenceMatch]
resolveReferenceMatches targetName =
  resolveReferenceMatchesForNames [targetName]

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
  let cache' = emptyResolverCache
  logTimedSectionStart "findReferences:prepareCandidateModules"
  (_, preparedModules) <-
    foldM
      (prepareCandidateModule modSummaries)
      (cache', [])
      candidateModules
  logTimedSectionEnd "findReferences:prepareCandidateModules"
  logTimedSectionStart "findReferences:analyzePreparedModules"
  resolvedMatches <-
    concat <$> forConcurrently preparedModules (liftIO . Exception.evaluate . analyzePreparedModule targetSet)
  logTimedSectionEnd "findReferences:analyzePreparedModules"
  logTimedSectionStart "findReferences:forceMatches"
  forcedMatches <- liftIO $ Exception.evaluate (forceReferenceMatchesForRendering resolvedMatches)
  logTimedSectionEnd "findReferences:forceMatches"
  Log.debug $
    "Finished resolving reference matches. Found "
      <> show (length forcedMatches)
      <> " matches in total"
  pure forcedMatches
  where
    prepareCandidateModule modSummaries (cache, preparedModules) homeModule = do
      (cache', maybePreparedModule) <- prepareReferenceModuleAnalysis modSummaries cache homeModule
      pure
        ( cache',
          case maybePreparedModule of
            Just preparedModule ->
              preparedModules <> [preparedModule]
            Nothing ->
              preparedModules
        )

resolveReferenceDefinitionsForNames :: (MonadLore m) => [GHC.Name] -> m [DefinitionSlice]
resolveReferenceDefinitionsForNames targetNames =
  mergeSlicesByModule . map referenceSlice <$> resolveReferenceMatchesForNames targetNames

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
            matchedReferenceSpans = dedupeSpans matchedSpans
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
  ModSummaries modSummaries <- getModSummaries
  let depth = max 0 maxDepth
  result <- go modSummaries depth inputName (ClosureState emptyResolverCache Set.empty [])
  pure (mergeSlicesByModule result.closureSlices)
  where
    go modSummaries depth name state = do
      (cache', analysis) <- resolveDefinitionAnalysis modSummaries state.closureCache name
      case analysis of
        Nothing ->
          pure state {closureCache = cache'}
        Just definitionAnalysis ->
          let slice = analysisSlice definitionAnalysis
              key = definitionKey slice
           in if Set.member key state.closureSeen
                then pure state {closureCache = cache'}
                else
                  if depth == 0
                    then
                      pure
                        state
                          { closureCache = cache',
                            closureSeen = Set.insert key state.closureSeen,
                            closureSlices = state.closureSlices <> [slice]
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
                      pure result {closureSlices = state.closureSlices <> (slice : result.closureSlices)}

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

renderDefinitionSliceText :: DefinitionSlice -> IO Text
renderDefinitionSliceText definitionSlice =
  T.intercalate "\n\n" <$> mapM renderDeclarationSpansText definitionSlice.declarationSpans

renderDefinitionModuleText :: DefinitionSlice -> IO Text
renderDefinitionModuleText definitionSlice = do
  renderedPath <- renderDefinitionModulePath definitionSlice
  renderedDeclarations <- mapM renderDeclarationBlock definitionSlice.declarationSpans
  let renderedImports =
        map (T.pack . renderImport) definitionSlice.requiredImports
      renderedBlocks =
        filter (not . T.null) $
          [renderImportsBlock renderedImports] <> renderedDeclarations
  pure $
    T.intercalate "\n\n" $
      ["=== " <> renderedPath <> " ==="]
        <> renderedBlocks

renderDefinitionModulesText :: [DefinitionSlice] -> IO Text
renderDefinitionModulesText definitionSlices =
  T.intercalate "\n\n"
    <$> mapM renderDefinitionModuleText (mergeSlicesByModule definitionSlices)

renderDeclarationSpansText :: DeclarationSpans -> IO Text
renderDeclarationSpansText spans = do
  declarationText <- readSpanText spans.declarationSpan
  signatureText <- traverse readSpanText spans.signatureSpan
  pure $
    maybe declarationText (<> "\n" <> declarationText) signatureText

renderDefinitionModulePath :: DefinitionSlice -> IO Text
renderDefinitionModulePath definitionSlice =
  case definitionSliceRealSrcSpan definitionSlice of
    Nothing ->
      pure "<definition source unavailable>"
    Just realSrcSpan -> do
      currentDirectory <- getCurrentDirectory
      pure . T.pack $
        relativeSourcePath currentDirectory (GHC.unpackFS (GHC.srcSpanFile realSrcSpan))

definitionSliceRealSrcSpan :: DefinitionSlice -> Maybe GHC.RealSrcSpan
definitionSliceRealSrcSpan definitionSlice =
  case mapMaybe declarationSpansRealSrcSpan definitionSlice.declarationSpans of
    realSrcSpan : _ -> Just realSrcSpan
    [] -> Nothing

declarationSpansRealSrcSpan :: DeclarationSpans -> Maybe GHC.RealSrcSpan
declarationSpansRealSrcSpan spans =
  realSrcSpanFromSrcSpan spans.declarationSpan
    <|> (spans.signatureSpan >>= realSrcSpanFromSrcSpan)

realSrcSpanFromSrcSpan :: GHC.SrcSpan -> Maybe GHC.RealSrcSpan
realSrcSpanFromSrcSpan = \case
  GHC.RealSrcSpan realSrcSpan _ ->
    Just realSrcSpan
  GHC.UnhelpfulSpan {} ->
    Nothing

renderImportsBlock :: [Text] -> Text
renderImportsBlock importsSection =
  case filter (not . T.null) importsSection of
    [] -> ""
    renderedImports ->
      T.intercalate "\n" $
        ["--- imports ---"] <> renderedImports

renderDeclarationBlock :: DeclarationSpans -> IO Text
renderDeclarationBlock declarationSpans = do
  declarationText <- renderDeclarationSpansText declarationSpans
  pure $
    T.intercalate
      "\n"
      [ "--- " <> renderDeclarationBlockHeader declarationSpans <> " ---",
        declarationText
      ]

renderDeclarationBlockHeader :: DeclarationSpans -> Text
renderDeclarationBlockHeader declarationSpans =
  case declarationSpansLineRange declarationSpans of
    Nothing ->
      "definition"
    Just (startLine, endLine) ->
      "lines " <> T.pack (show startLine) <> "-" <> T.pack (show endLine)

declarationSpansLineRange :: DeclarationSpans -> Maybe (Int, Int)
declarationSpansLineRange declarationSpans = do
  firstSpan <- minimumMaybe realSrcSpans
  lastSpan <- maximumMaybe realSrcSpans
  pure (GHC.srcSpanStartLine firstSpan, GHC.srcSpanEndLine lastSpan)
  where
    realSrcSpans =
      mapMaybe realSrcSpanFromSrcSpan $
        maybeToList declarationSpans.signatureSpan <> [declarationSpans.declarationSpan]

minimumMaybe :: (Ord a) => [a] -> Maybe a
minimumMaybe = \case
  [] -> Nothing
  values -> Just (minimum values)

maximumMaybe :: (Ord a) => [a] -> Maybe a
maximumMaybe = \case
  [] -> Nothing
  values -> Just (maximum values)

relativeSourcePath :: FilePath -> FilePath -> FilePath
relativeSourcePath currentDirectory sourcePath =
  normalise $
    if isRelative sourcePath
      then sourcePath
      else makeRelative currentDirectory sourcePath

readSpanText :: GHC.SrcSpan -> IO Text
readSpanText = \case
  GHC.RealSrcSpan realSpan _ ->
    sliceRealSpan realSpan . T.lines . T.pack <$> readFile (GHC.unpackFS (GHC.srcSpanFile realSpan))
  GHC.UnhelpfulSpan {} ->
    pure "<definition source unavailable>"

sliceRealSpan :: GHC.RealSrcSpan -> [Text] -> Text
sliceRealSpan realSpan fileLines =
  case drop (GHC.srcSpanStartLine realSpan - 1) fileLines of
    [] ->
      ""
    relevantLines ->
      T.intercalate
        "\n"
        ( zipWith
            sliceLine
            [GHC.srcSpanStartLine realSpan .. GHC.srcSpanEndLine realSpan]
            (take (GHC.srcSpanEndLine realSpan - GHC.srcSpanStartLine realSpan + 1) relevantLines)
        )
  where
    sliceLine lineNo line
      | lineNo == GHC.srcSpanStartLine realSpan && lineNo == GHC.srcSpanEndLine realSpan =
          T.take width (T.drop startCol line)
      | lineNo == GHC.srcSpanStartLine realSpan =
          T.drop startCol line
      | lineNo == GHC.srcSpanEndLine realSpan =
          T.take endCol line
      | otherwise =
          line
      where
        startCol = GHC.srcSpanStartCol realSpan - 1
        endCol = GHC.srcSpanEndCol realSpan - 1
        width = endCol - startCol

renderImport :: RequiredImport -> String
renderImport RequiredImport {..} =
  unwords $
    ["import"]
      <> ["{-# SOURCE #-}" | importSource]
      <> maybe [] pure importPackageQualifier
      <> ["qualified" | importQualifiedStyle == QualifiedPre]
      <> [modulePart]
      <> maybe [] (\alias -> ["as", GHC.moduleNameString alias]) importAlias
      <> case renderedItems of
        [] -> []
        xs -> ["(" <> List.intercalate ", " (map renderItem xs) <> ")"]
  where
    modulePart =
      GHC.moduleNameString importModule
        <> case importQualifiedStyle of
          QualifiedPost -> " qualified"
          _ -> ""

    renderedItems
      | isQualifiedImport importQualifiedStyle
          && importAlias /= Nothing
          && not importOriginallyExplicit =
          []
      | otherwise =
          importItems

    renderItem = \case
      ImportName name ->
        renderName name
      ImportParent parentName childNames ->
        renderName parentName
          <> "("
          <> List.intercalate ", " (map renderName childNames)
          <> ")"

    renderName =
      GHC.occNameString . GHC.nameOccName

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
  occurrenceIndex <-
    foldM (buildModuleIndex parsedModuleCache) Map.empty (Map.keys modSummaries)
  Log.debug $ "Finished building reference occurrence index. Indexed " <> show (Map.size occurrenceIndex) <> " unique occurrence names."
  pure (ReferenceOccurrenceIndex occurrenceIndex)
  where
    buildModuleIndex parsedModuleCache occurrenceIndex homeModule =
      pure $
        case Map.lookup homeModule parsedModuleCache of
          Nothing -> occurrenceIndex
          Just parsedModule ->
            foldl'
              (\index occName -> Map.insertWith (<>) occName (Set.singleton homeModule) index)
              occurrenceIndex
              (Set.toList (moduleOccurrenceNames parsedModule))

prepareReferenceModuleAnalysis ::
  (MonadLore m) =>
  Map.Map GHC.Module GHC.ModSummary ->
  ResolverCache ->
  GHC.Module ->
  m (ResolverCache, Maybe PreparedReferenceModule)
prepareReferenceModuleAnalysis modSummaries cache homeModule = do
  maybeModuleAnalysis <- getReferenceModuleAnalysis modSummaries homeModule
  case maybeModuleAnalysis of
    Nothing ->
      pure (cache, Nothing)
    Just moduleAnalysis -> do
      pure
        ( cache',
          Just
            PreparedReferenceModule
              { preparedReferenceModuleAnalysis = moduleAnalysis
              }
        )
  where
    cache' = cache

analyzePreparedModule :: Set.Set GHC.Name -> PreparedReferenceModule -> [ReferenceMatch]
analyzePreparedModule targetSet preparedModule =
  matchingReferenceMatches targetSet preparedModule.preparedReferenceModuleAnalysis

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
buildCachedReferenceModuleAnalysis modSummaries homeModule = do
  parsedModuleCacheVar <- asks referenceParsedModuleCache
  typedModuleCacheVar <- asks referenceTypedModuleCache
  coreFactsCacheVar <- asks referenceMinimalCoreModuleFactsCache
  parsedModuleCache <- liftIO (readMVar parsedModuleCacheVar)
  coreFactsByModule <- liftIO (readMVar coreFactsCacheVar)
  maybeParsedSummary <- prepareParsedModuleSummary parsedModuleCacheVar parsedModuleCache
  maybeTypedFactsByDefinition <- prepareProcessedTypedFacts typedModuleCacheVar maybeParsedSummary
  case (Map.lookup homeModule modSummaries, maybeParsedSummary, maybeTypedFactsByDefinition, Map.lookup homeModule coreFactsByModule) of
    (Nothing, _, _, _) ->
      pure Nothing
    (_, Just parsedSummary, Just typedFactsByDefinition, Just coreFacts) -> do
      let moduleAnalysis =
            mergeReferenceModuleAnalysisWithCoreFacts
              coreFacts
              (buildReferenceModuleAnalysis homeModule parsedSummary typedFactsByDefinition)
      cacheReferenceModuleAnalysis homeModule (Just moduleAnalysis)
      pure (Just moduleAnalysis)
    _ -> do
      Log.debug $ "Cached definition artifacts missing for " <> GHC.moduleNameString (GHC.moduleName homeModule)
      cacheReferenceModuleAnalysis homeModule Nothing
      pure Nothing
  where
    prepareParsedModuleSummary parsedModuleCacheVar parsedModuleCache =
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
        { matchedReferenceSpans = dedupeSpans (old.matchedReferenceSpans <> new.matchedReferenceSpans)
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

isQualifiedImport :: ImportQualifiedStyle -> Bool
isQualifiedImport = (/= NotQualified)

sortDeclarationSpans :: [DeclarationSpans] -> [DeclarationSpans]
sortDeclarationSpans =
  List.sortOn (GHC.srcSpanToRealSrcSpan . declarationSpan)

dedupeDeclarationSpans :: [DeclarationSpans] -> [DeclarationSpans]
dedupeDeclarationSpans =
  nubOrdOn (\declarationSpans -> (show declarationSpans.declarationSpan, fmap show declarationSpans.signatureSpan))
