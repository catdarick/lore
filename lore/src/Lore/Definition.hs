module Lore.Definition
  ( resolveDefinitionSlice,
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
    RequiredImport,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (foldM)
import Data.Containers.ListUtils (nubOrdOn)
import Data.Data (Data, Typeable, cast, gmapQ)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC
import qualified GHC.Data.Bag as Bag
import qualified GHC.Plugins as GHC
import qualified GHC.Tc.Types as GHC.Tc
import Lore.Internal.Definition.Cache (cacheReferenceModuleAnalysis, cacheReferenceModuleSearch, lookupReferenceModuleAnalysisCache, lookupReferenceModuleSearchCache)
import Lore.Internal.Definition.Types (DeclarationSpans (..), DefinitionAnalysis (..), DefinitionSlice (..), ReferenceModuleAnalysis (..), ReferenceModuleSearch (..), RequiredImport (..), RequiredImportItem (..))
import Lore.Internal.Lookup.ModSummaries (getModSummaries)
import Lore.Internal.Lookup.NameToInstances (getNameToInstancesIndex)
import Lore.Internal.Lookup.Types (ModSummaries (..), NameToInstancesIndex (..))
import Lore.Monad
import System.Directory (getCurrentDirectory)
import System.FilePath (isRelative, makeRelative, normalise)

data DefinitionMatch = DefinitionMatch
  { matchedDeclaration :: GHC.LHsDecl GHC.GhcPs,
    matchedSignature :: Maybe (GHC.LHsDecl GHC.GhcPs)
  }

data ModuleContext = ModuleContext
  { parsedModule :: GHC.ParsedModule,
    typecheckedModule :: GHC.TypecheckedModule,
    desugaredModule :: GHC.DesugaredModule
  }

data ResolverCache = ResolverCache
  { cachedModules :: Map.Map GHC.Module ModuleContext,
    cachedAnalyses :: Map.Map GHC.Name (Maybe DefinitionAnalysis)
  }

newtype OccurrenceSyntax = OccurrenceSyntax
  { syntaxQualifier :: Maybe GHC.ModuleName
  }
  deriving stock (Eq)

data ReferencedOccurrence = ReferencedOccurrence
  { occurrenceName :: GHC.Name,
    occurrenceParent :: Maybe GHC.Name,
    occurrenceSyntax :: OccurrenceSyntax,
    occurrenceCandidates :: [Int]
  }

data DefinitionKey = DefinitionKey
  { definitionKeyModule :: GHC.Module,
    definitionKeySpan :: Maybe GHC.RealSrcSpan
  }
  deriving stock (Eq, Ord)

data SourceImport = SourceImport
  { sourceImportId :: Int,
    sourceImportDecl :: GHC.LImportDecl GHC.GhcRn
  }

data ClosureState = ClosureState
  { closureCache :: ResolverCache,
    closureSeen :: Set.Set DefinitionKey,
    closureSlices :: [DefinitionSlice]
  }

resolveDefinitionSlice :: (MonadLore m) => GHC.Name -> m (Maybe DefinitionSlice)
resolveDefinitionSlice inputName = do
  ModSummaries modSummaries <- getModSummaries
  (_, analysis) <- resolveDefinitionAnalysis modSummaries emptyResolverCache inputName
  pure (analysisSlice <$> analysis)

resolveReferenceDefinitions :: (MonadLore m) => GHC.Name -> m [DefinitionSlice]
resolveReferenceDefinitions targetName =
  resolveReferenceDefinitionsForNames [targetName]

resolveReferenceDefinitionsForNames :: (MonadLore m) => [GHC.Name] -> m [DefinitionSlice]
resolveReferenceDefinitionsForNames targetNames = do
  ModSummaries modSummaries <- getModSummaries
  let targetSet = Set.fromList targetNames
      targetOccNames = targetOccurrenceNames targetNames
      homeModules = Map.keys modSummaries
  (cache', candidateModules) <-
    foldM
      (collectCandidateModule targetOccNames modSummaries)
      (emptyResolverCache, [])
      homeModules
  (_, resolvedSlices) <-
    foldM
      (collectReferencingModule targetSet modSummaries)
      (cache', [])
      candidateModules
  pure (mergeSlicesByModule resolvedSlices)
  where
    collectCandidateModule targetOccNames modSummaries (cache, candidateModules) homeModule = do
      (cache', maybeSearch) <- resolveReferenceModuleSearch modSummaries cache homeModule
      pure
        ( cache',
          case maybeSearch of
            Just moduleSearch
              | moduleMentionsAnyTargetOccName targetOccNames moduleSearch ->
                  candidateModules <> [homeModule]
            _ ->
              candidateModules
        )

    collectReferencingModule targetSet modSummaries (cache, resolvedSlices) homeModule = do
      (cache', maybeModuleAnalysis) <- resolveReferenceModuleAnalysis modSummaries cache homeModule
      pure
        ( cache',
          case maybeModuleAnalysis of
            Just moduleAnalysis ->
              resolvedSlices <> matchingDefinitionSlices targetSet moduleAnalysis
            Nothing ->
              resolvedSlices
        )

    matchingDefinitionSlices targetSet moduleAnalysis =
      [ definitionAnalysis.analysisSlice
      | Just definitionAnalysis <- Map.elems moduleAnalysis.referenceModuleDefinitions,
        any (`Set.member` targetSet) definitionAnalysis.analysisReferences
      ]

targetOccurrenceNames :: [GHC.Name] -> Set.Set Text
targetOccurrenceNames =
  Set.fromList
    . map (T.pack . GHC.occNameString . GHC.nameOccName)

moduleMentionsAnyTargetOccName :: Set.Set Text -> ReferenceModuleSearch -> Bool
moduleMentionsAnyTargetOccName targetOccNames moduleSearch =
  not $
    Set.null $
      Set.intersection targetOccNames moduleSearch.referenceModuleOccurrenceNames

enumerateModuleReferenceDefinitionNames :: (MonadLore m) => GHC.Module -> m [GHC.Name]
enumerateModuleReferenceDefinitionNames homeModule = do
  topLevelNames <- enumerateModuleDefinitionNames homeModule
  instanceNames <- enumerateModuleInstanceDefinitionNames homeModule
  pure (dedupeNames (topLevelNames <> instanceNames))

enumerateModuleDefinitionNames :: (MonadLore m) => GHC.Module -> m [GHC.Name]
enumerateModuleDefinitionNames homeModule = do
  maybeModuleInfo <- GHC.getModuleInfo homeModule
  pure $
    case maybeModuleInfo of
      Nothing ->
        []
      Just moduleInfo ->
        filter ((== Just homeModule) . GHC.nameModule_maybe) $
          fromMaybe [] (GHC.modInfoTopLevelScope moduleInfo)

enumerateModuleInstanceDefinitionNames :: (MonadLore m) => GHC.Module -> m [GHC.Name]
enumerateModuleInstanceDefinitionNames homeModule = do
  NameToInstancesIndex nameToInstancesIndex <- getNameToInstancesIndex
  pure $
    filter belongsToModule $
      dedupeNames $
        concatMap referencedInstanceNames (GHC.nonDetNameEnvElts nameToInstancesIndex)
  where
    belongsToModule name =
      GHC.nameModule_maybe name == Just homeModule

    referencedInstanceNames (classInstances, familyInstances) =
      [GHC.getName clsInst | clsInst <- classInstances]
        <> [GHC.getName famInst | famInst <- familyInstances]

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
      <> ["qualified" | importQualifiedStyle == GHC.QualifiedPre]
      <> [modulePart]
      <> maybe [] (\alias -> ["as", GHC.moduleNameString alias]) importAlias
      <> case renderedItems of
        [] -> []
        xs -> ["(" <> List.intercalate ", " (map renderItem xs) <> ")"]
  where
    modulePart =
      GHC.moduleNameString importModule
        <> case importQualifiedStyle of
          GHC.QualifiedPost -> " qualified"
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
          (cache', context) <- resolveModuleContext modSummaries cache definingModule
          let analysis = context >>= analyzeDefinition definingModule inputName
          pure
            ( cache'
                { cachedAnalyses =
                    Map.insert inputName analysis cache'.cachedAnalyses
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

resolveReferenceModuleSearch ::
  (MonadLore m) =>
  Map.Map GHC.Module GHC.ModSummary ->
  ResolverCache ->
  GHC.Module ->
  m (ResolverCache, Maybe ReferenceModuleSearch)
resolveReferenceModuleSearch modSummaries cache homeModule =
  lookupReferenceModuleSearchCache homeModule >>= \case
    Just cachedSearch ->
      pure (cache, cachedSearch)
    Nothing ->
      case Map.lookup homeModule modSummaries of
        Nothing -> do
          cacheReferenceModuleSearch homeModule Nothing
          pure (cache, Nothing)
        Just summary -> do
          parsedModule <- GHC.parseModule summary
          let moduleSearch =
                Just $
                  ReferenceModuleSearch
                    { referenceModuleOccurrenceNames =
                        collectModuleOccurrenceNames (GHC.pm_parsed_source parsedModule)
                    }
          cacheReferenceModuleSearch homeModule moduleSearch
          pure (cache, moduleSearch)

resolveReferenceModuleAnalysis ::
  (MonadLore m) =>
  Map.Map GHC.Module GHC.ModSummary ->
  ResolverCache ->
  GHC.Module ->
  m (ResolverCache, Maybe ReferenceModuleAnalysis)
resolveReferenceModuleAnalysis modSummaries cache homeModule =
  lookupReferenceModuleAnalysisCache homeModule >>= \case
    Just cachedAnalysis ->
      pure (cacheDefinitionAnalyses cache cachedAnalysis, cachedAnalysis)
    Nothing -> do
      (cache', maybeContext) <- resolveModuleContext modSummaries cache homeModule
      case maybeContext of
        Nothing -> do
          cacheReferenceModuleAnalysis homeModule Nothing
          pure (cache', Nothing)
        Just context -> do
          definitionNames <- enumerateModuleReferenceDefinitionNames homeModule
          let analysesByName =
                Map.fromList
                  [ (definitionName, analyzeDefinition homeModule definitionName context)
                  | definitionName <- definitionNames
                  ]
              moduleAnalysis =
                Just $
                  ReferenceModuleAnalysis
                    { referenceModuleDefinitions = analysesByName
                    }
              cache'' = cacheDefinitionAnalyses cache' moduleAnalysis
          cacheReferenceModuleAnalysis homeModule moduleAnalysis
          pure (cache'', moduleAnalysis)
  where
    cacheDefinitionAnalyses cache' = \case
      Nothing ->
        cache'
      Just moduleAnalysis ->
        cache'
          { cachedAnalyses =
              Map.union moduleAnalysis.referenceModuleDefinitions cache'.cachedAnalyses
          }

resolveModuleContext ::
  (MonadLore m) =>
  Map.Map GHC.Module GHC.ModSummary ->
  ResolverCache ->
  GHC.Module ->
  m (ResolverCache, Maybe ModuleContext)
resolveModuleContext modSummaries cache definingModule =
  case Map.lookup definingModule cache.cachedModules of
    Just context ->
      pure (cache, Just context)
    Nothing ->
      case Map.lookup definingModule modSummaries of
        Nothing ->
          pure (cache, Nothing)
        Just summary -> do
          parsedModule <- GHC.parseModule summary
          typecheckedModule <- GHC.typecheckModule parsedModule
          desugaredModule <- GHC.desugarModule typecheckedModule
          let context =
                ModuleContext
                  { parsedModule,
                    typecheckedModule,
                    desugaredModule
                  }
          pure
            ( cache
                { cachedModules =
                    Map.insert definingModule context cache.cachedModules
                },
              Just context
            )

analyzeDefinition ::
  GHC.Module ->
  GHC.Name ->
  ModuleContext ->
  Maybe DefinitionAnalysis
analyzeDefinition definingModule target ModuleContext {parsedModule, typecheckedModule, desugaredModule} = do
  DefinitionMatch {matchedDeclaration, matchedSignature} <-
    findDefinitionMatch target (GHC.pm_parsed_source parsedModule)
  let (tcg, _details) = GHC.tm_internals_ typecheckedModule
      sourceImports =
        [ SourceImport importId importDecl
        | (importId, importDecl) <- zip [0 ..] (GHC.Tc.tcg_rn_imports tcg),
          not (GHC.unLoc importDecl).ideclExt.ideclImplicit
        ]
      spans =
        DeclarationSpans
          { declarationSpan = GHC.getLocA matchedDeclaration,
            signatureSpan = GHC.getLocA <$> matchedSignature
          }
      occurrences =
        collectReferencedOccurrences
          definingModule
          spans
          sourceImports
          tcg
          (GHC.pm_parsed_source parsedModule)
          typecheckedModule
      slice =
        DefinitionSlice
          { definitionModule = definingModule,
            declarationSpans = [spans],
            requiredImports = buildImports sourceImports occurrences
          }
      usedInstances =
        collectUsedInstanceNames target spans desugaredModule
  pure
    DefinitionAnalysis
      { analysisSlice = slice,
        analysisReferences = collectReferencedNames target spans occurrences,
        analysisUsedInstances = usedInstances
      }

collectUsedInstanceNames ::
  GHC.Name ->
  DeclarationSpans ->
  GHC.DesugaredModule ->
  [GHC.Name]
collectUsedInstanceNames target spans desugaredModule =
  dedupeNames $
    concatMap collectExprUsedInstanceNames $
      definitionCoreExprs target declarationSpan (GHC.mg_binds (GHC.dm_core_module desugaredModule))
  where
    declarationSpan = spans.declarationSpan

definitionCoreExprs ::
  GHC.Name ->
  GHC.SrcSpan ->
  [GHC.CoreBind] ->
  [GHC.CoreExpr]
definitionCoreExprs target declarationSpan =
  concatMap (bindingCoreExprs target declarationSpan)

bindingCoreExprs ::
  GHC.Name ->
  GHC.SrcSpan ->
  GHC.CoreBind ->
  [GHC.CoreExpr]
bindingCoreExprs target declarationSpan = \case
  GHC.NonRec binder rhs
    | matchesDefinitionBinder target declarationSpan binder ->
        [rhs]
    | otherwise ->
        []
  GHC.Rec bindings ->
    [rhs | (binder, rhs) <- bindings, matchesDefinitionBinder target declarationSpan binder]

matchesDefinitionBinder ::
  GHC.Name ->
  GHC.SrcSpan ->
  GHC.CoreBndr ->
  Bool
matchesDefinitionBinder target declarationSpan binder =
  binderName == target
    || GHC.nameSrcSpan binderName `GHC.isSubspanOf` declarationSpan
  where
    binderName = GHC.getName binder

collectExprUsedInstanceNames :: GHC.CoreExpr -> [GHC.Name]
collectExprUsedInstanceNames = \case
  GHC.Var variable ->
    [GHC.getName variable | GHC.isDFunId variable]
  GHC.Lit _ ->
    []
  GHC.App function argument ->
    collectExprUsedInstanceNames function <> collectExprUsedInstanceNames argument
  GHC.Lam _ body ->
    collectExprUsedInstanceNames body
  GHC.Let binding body ->
    collectBindUsedInstanceNames binding <> collectExprUsedInstanceNames body
  GHC.Case scrutinee _ _ alternatives ->
    collectExprUsedInstanceNames scrutinee
      <> concatMap collectAltUsedInstanceNames alternatives
  GHC.Cast expression _ ->
    collectExprUsedInstanceNames expression
  GHC.Tick _ expression ->
    collectExprUsedInstanceNames expression
  GHC.Type _ ->
    []
  GHC.Coercion _ ->
    []

collectBindUsedInstanceNames :: GHC.CoreBind -> [GHC.Name]
collectBindUsedInstanceNames = \case
  GHC.NonRec _ rhs ->
    collectExprUsedInstanceNames rhs
  GHC.Rec bindings ->
    concatMap (collectExprUsedInstanceNames . snd) bindings

collectAltUsedInstanceNames :: GHC.CoreAlt -> [GHC.Name]
collectAltUsedInstanceNames (GHC.Alt _ _ expression) =
  collectExprUsedInstanceNames expression

collectModuleOccurrenceNames :: GHC.ParsedSource -> Set.Set Text
collectModuleOccurrenceNames parsedSource =
  Set.fromList
    [ T.pack (GHC.occNameString (GHC.rdrNameOcc (GHC.unLoc locatedName)))
    | locatedName <- collectTyped parsedSource :: [GHC.LocatedN GHC.RdrName]
    ]

collectReferencedOccurrences ::
  GHC.Module ->
  DeclarationSpans ->
  [SourceImport] ->
  GHC.Tc.TcGblEnv ->
  GHC.ParsedSource ->
  GHC.TypecheckedModule ->
  [ReferencedOccurrence]
collectReferencedOccurrences definingModule spans sourceImports tcg parsedSource typecheckedModule =
  case GHC.tm_renamed_source typecheckedModule of
    Nothing ->
      []
    Just (renamedGroup, _imports, _exports, _docs) ->
      dedupeOccurrences $
        mapMaybe toReferencedOccurrence $
          collectLocatedNames targetSpans renamedGroup
  where
    targetSpans =
      declarationSpan spans
        : maybeToList spans.signatureSpan

    occurrenceSyntax =
      collectOccurrenceSyntax targetSpans parsedSource

    toReferencedOccurrence locatedName = do
      gre <- GHC.lookupGRE_Name (GHC.Tc.tcg_rdr_env tcg) (GHC.unLoc locatedName)
      let occurrenceName = GHC.unLoc locatedName
          syntax =
            fromMaybe (OccurrenceSyntax Nothing) $
              lookup (locatedSpan locatedName) occurrenceSyntax
      guardReference definingModule spans occurrenceName $
        ReferencedOccurrence
          { occurrenceName,
            occurrenceParent =
              case GHC.gre_par gre of
                GHC.ParentIs parentName -> Just parentName
                GHC.NoParent -> Nothing,
            occurrenceSyntax = syntax,
            occurrenceCandidates =
              dedupeIds
                [ importId
                | importSpec <- Bag.bagToList (GHC.gre_imp gre),
                  supportsOccurrence syntax (GHC.is_decl importSpec),
                  Just importId <- [findImportId (GHC.is_dloc (GHC.is_decl importSpec))]
                ]
          }

    findImportId importSpan =
      sourceImportId <$> List.find ((== importSpan) . GHC.getLocA . sourceImportDecl) sourceImports

buildImports ::
  [SourceImport] ->
  [ReferencedOccurrence] ->
  [RequiredImport]
buildImports sourceImports occurrences =
  mapMaybe buildRequiredImport $
    IntMap.toAscList assignedOccurrences
  where
    chosenImports =
      chooseMinimalImports importedOccurrences

    importedOccurrences =
      filter (not . null . occurrenceCandidates) occurrences

    assignedOccurrences =
      IntMap.fromListWith
        (<>)
        [ (importId, [ref])
        | ref <- importedOccurrences,
          Just importId <- [List.find (`IntSet.member` chosenImports) ref.occurrenceCandidates]
        ]

    chooseMinimalImports =
      go IntSet.empty
      where
        go chosen [] = chosen
        go chosen remaining =
          let counts =
                IntMap.fromListWith
                  (+)
                  [(c, 1 :: Int) | ref <- remaining, c <- ref.occurrenceCandidates]
              bestImport =
                fst $
                  List.maximumBy (\a b -> compare (snd a) (snd b)) $
                    IntMap.toList counts
              chosen' = IntSet.insert bestImport chosen
           in go chosen' (filter (not . coveredBy chosen') remaining)

        coveredBy chosen ref =
          any (`IntSet.member` chosen) ref.occurrenceCandidates

    buildRequiredImport (importId, refs) = do
      si <- List.find ((== importId) . sourceImportId) sourceImports
      let decl = GHC.unLoc si.sourceImportDecl
      pure
        RequiredImport
          { importKey = importId,
            importModule = GHC.unLoc decl.ideclName,
            importPackageQualifier = pkgQualString decl.ideclPkgQual,
            importSource = decl.ideclSource == GHC.IsBoot,
            importQualifiedStyle = decl.ideclQualified,
            importAlias = GHC.unLoc <$> decl.ideclAs,
            importOriginallyExplicit = decl.ideclImportList /= Nothing,
            importItems = normalizeImportItems (concatMap occurrenceItems refs)
          }

    occurrenceItems ReferencedOccurrence {occurrenceName, occurrenceParent} =
      case occurrenceParent of
        Just parentName
          | parentName /= occurrenceName ->
              [ImportParent parentName [occurrenceName]]
        _ ->
          [ImportName occurrenceName]

collectReferencedNames ::
  GHC.Name ->
  DeclarationSpans ->
  [ReferencedOccurrence] ->
  [GHC.Name]
collectReferencedNames target spans =
  dedupeNames
    . filter (isFollowableReference target spans)
    . map (.occurrenceName)

isFollowableReference :: GHC.Name -> DeclarationSpans -> GHC.Name -> Bool
isFollowableReference target spans name =
  name /= target
    && case GHC.nameModule_maybe name of
      Nothing -> False
      Just definingModule ->
        not (definesName spans.declarationSpan definingModule name)

guardReference ::
  GHC.Module ->
  DeclarationSpans ->
  GHC.Name ->
  ReferencedOccurrence ->
  Maybe ReferencedOccurrence
guardReference definingModule spans occurrenceName occurrence
  | definesName spans.declarationSpan definingModule occurrenceName = Nothing
  | otherwise = Just occurrence

definesName :: GHC.SrcSpan -> GHC.Module -> GHC.Name -> Bool
definesName declarationSpan definingModule name =
  GHC.nameModule_maybe name == Just definingModule
    && GHC.nameSrcSpan name `GHC.isSubspanOf` declarationSpan

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
    { cachedModules = Map.empty,
      cachedAnalyses = Map.empty
    }

findDefinitionMatch :: GHC.Name -> GHC.ParsedSource -> Maybe DefinitionMatch
findDefinitionMatch target parsedSource = do
  decl <- findParentDeclaration target decls
  pure $ DefinitionMatch decl (findSignatureDeclaration (GHC.nameOccName target) decls)
  where
    decls =
      GHC.hsmodDecls $
        GHC.unLoc parsedSource

findParentDeclaration ::
  GHC.Name ->
  [GHC.LHsDecl GHC.GhcPs] ->
  Maybe (GHC.LHsDecl GHC.GhcPs)
findParentDeclaration target decls =
  case GHC.srcSpanToRealSrcSpan (GHC.nameSrcSpan target) of
    Nothing ->
      Nothing
    Just _ ->
      List.find (\decl -> GHC.nameSrcSpan target `GHC.isSubspanOf` GHC.getLocA decl) decls

findSignatureDeclaration ::
  GHC.OccName ->
  [GHC.LHsDecl GHC.GhcPs] ->
  Maybe (GHC.LHsDecl GHC.GhcPs)
findSignatureDeclaration targetOcc =
  List.find (isTopLevelSignatureFor targetOcc)

matchesLocatedRdrName ::
  GHC.OccName ->
  GHC.GenLocated l GHC.RdrName ->
  Bool
matchesLocatedRdrName targetOcc =
  (== targetOcc) . GHC.rdrNameOcc . GHC.unLoc

isTopLevelSignatureFor :: GHC.OccName -> GHC.LHsDecl GHC.GhcPs -> Bool
isTopLevelSignatureFor targetOcc decl =
  case GHC.unLoc decl of
    GHC.SigD _ sig ->
      signatureMatches targetOcc sig
    GHC.KindSigD _ (GHC.StandaloneKindSig _ name _) ->
      matchesLocatedRdrName targetOcc name
    _ ->
      False

signatureMatches :: GHC.OccName -> GHC.Sig GHC.GhcPs -> Bool
signatureMatches targetOcc = \case
  GHC.TypeSig _ names _ ->
    any (matchesLocatedRdrName targetOcc) names
  GHC.PatSynSig _ names _ ->
    any (matchesLocatedRdrName targetOcc) names
  _ ->
    False

collectOccurrenceSyntax ::
  [GHC.SrcSpan] ->
  GHC.ParsedSource ->
  [(GHC.SrcSpan, OccurrenceSyntax)]
collectOccurrenceSyntax targetSpans parsedSource =
  [ (span', syntaxFromRdrName (GHC.unLoc locatedName))
  | locatedName <- collectTyped parsedSource,
    let span' = locatedSpan locatedName,
    spanWithin targetSpans span'
  ]

collectLocatedNames ::
  (Data a) =>
  [GHC.SrcSpan] ->
  a ->
  [GHC.LocatedN GHC.Name]
collectLocatedNames targetSpans =
  filter (spanWithin targetSpans . locatedSpan)
    . collectTyped

collectTyped :: forall b a. (Typeable b, Data a) => a -> [b]
collectTyped = go
  where
    go :: forall x. (Data x) => x -> [b]
    go value =
      maybeToList (cast value) <> concat (gmapQ go value)

locatedSpan :: GHC.LocatedN a -> GHC.SrcSpan
locatedSpan =
  GHC.locA . GHC.getLoc

syntaxFromRdrName :: GHC.RdrName -> OccurrenceSyntax
syntaxFromRdrName =
  OccurrenceSyntax . fmap fst . GHC.isQual_maybe

supportsOccurrence :: OccurrenceSyntax -> GHC.ImpDeclSpec -> Bool
supportsOccurrence OccurrenceSyntax {syntaxQualifier} importDeclSpec =
  case syntaxQualifier of
    Nothing ->
      not (GHC.is_qual importDeclSpec)
    Just qualifier ->
      GHC.is_as importDeclSpec == qualifier

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

normalizeImportItems :: [RequiredImportItem] -> [RequiredImportItem]
normalizeImportItems items =
  standaloneItems <> parentItems
  where
    standaloneNames =
      dedupeNames
        [ name
        | ImportName name <- items
        ]

    childNamesByParent =
      Map.fromListWith
        (<>)
        [ (parentName, childNames)
        | ImportParent parentName childNames <- items
        ]

    standaloneItems =
      map ImportName $
        filter (`Map.notMember` childNamesByParent) standaloneNames

    parentItems =
      [ ImportParent parentName (dedupeNames childNames)
      | (parentName, childNames) <- List.sortOn (renderName . fst) (Map.toList childNamesByParent)
      ]

    renderName =
      GHC.occNameString . GHC.nameOccName

dedupeOccurrences :: [ReferencedOccurrence] -> [ReferencedOccurrence]
dedupeOccurrences =
  List.nubBy sameOccurrence
  where
    sameOccurrence left right =
      left.occurrenceName == right.occurrenceName
        && left.occurrenceParent == right.occurrenceParent
        && left.occurrenceSyntax == right.occurrenceSyntax
        && left.occurrenceCandidates == right.occurrenceCandidates

dedupeIds :: [Int] -> [Int]
dedupeIds =
  IntSet.toAscList . IntSet.fromList

dedupeNames :: [GHC.Name] -> [GHC.Name]
dedupeNames =
  Map.elems . Map.fromList . map (\n -> (GHC.occNameString (GHC.nameOccName n), n))

isQualifiedImport :: GHC.ImportDeclQualifiedStyle -> Bool
isQualifiedImport = (/= GHC.NotQualified)

sortDeclarationSpans :: [DeclarationSpans] -> [DeclarationSpans]
sortDeclarationSpans =
  List.sortOn (GHC.srcSpanToRealSrcSpan . declarationSpan)

dedupeDeclarationSpans :: [DeclarationSpans] -> [DeclarationSpans]
dedupeDeclarationSpans =
  nubOrdOn (\declarationSpans -> (show declarationSpans.declarationSpan, fmap show declarationSpans.signatureSpan))

pkgQualString :: GHC.PkgQual -> Maybe String
pkgQualString = \case
  GHC.NoPkgQual -> Nothing
  pkgQual -> Just (GHC.showSDocUnsafe (GHC.ppr pkgQual))

spanWithin :: [GHC.SrcSpan] -> GHC.SrcSpan -> Bool
spanWithin targetSpans span' =
  any (span' `GHC.isSubspanOf`) targetSpans
