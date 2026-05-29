module Lore.Tools.FindDeadCode
  ( FindDeadCodeOptions (..),
    FindDeadCodeResult,
    FindDeadCodeOutput (..),
    FindDeadCodeFailure (..),
    FindDeadCodeFailureReason (..),
    FindDeadCodeReady (..),
    RenderedDeadDefinition (..),
    findDeadCode,
    renderFindDeadCodeOutput,
    renderFindDeadCodeReady,
  )
where

import qualified Data.Map.Strict as Map
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC
import GHC.Generics (Generic)
import qualified Lore as Core
import Lore
  ( DeadCodeOptions (..),
    DeadCodeResult (..),
    DeadDefinition (..),
    DefinitionSource (..),
    MonadLore,
    Symbol (..),
    resolveDefinitionSourceNamed,
  )
import Lore.Tools.Config (LoreConfig (..), loadLoreConfig)
import Lore.Tools.Internal.SymbolResolution
  ( ResolvedSymbolQuery (..),
    SymbolsResolved (resolvedQueries),
    SymbolsUnresolved,
    resolveUniqueSymbolQueries,
    unresolvedSymbolQueriesMessage,
  )
import Lore.Tools.Render.Doc (LoreDoc, ToLoreDoc (toLoreDoc), paragraph)
import Lore.Tools.Render.Text (quoteText, renderModuleName, renderSymbolName)
import Lore.Tools.Result
  ( Paginated (..),
    PaginationRenderConfig (..),
    PartialLoadWarning,
    PageRequest (..),
    ToolRun (..),
    defaultPageRequest,
    loadedSessionPartialWarning,
    paginateItemsWithPageRequest,
    paginationSummaryDoc,
    withLoadedSession,
  )

data FindDeadCodeOptions = FindDeadCodeOptions
  { findDeadCodeModules :: Maybe [Text],
    findDeadCodePageRequest :: Maybe PageRequest
  }
  deriving stock (Eq, Show, Generic)

type FindDeadCodeResult = ToolRun FindDeadCodeOutput

data FindDeadCodeOutput
  = FindDeadCodeFailed FindDeadCodeFailure
  | FindDeadCodeReadyResult FindDeadCodeReady

data FindDeadCodeFailure = FindDeadCodeFailure
  { findDeadCodeFailureReason :: FindDeadCodeFailureReason,
    findDeadCodeFailurePartialLoadWarning :: Maybe PartialLoadWarning
  }

data FindDeadCodeFailureReason
  = FindDeadCodeUnresolvedModules [Text]
  | FindDeadCodeInvalidConfig Text
  | FindDeadCodeUnresolvedAliveModules [Text]
  | FindDeadCodeUnresolvedSymbols SymbolsUnresolved
  | FindDeadCodeInvalidAliveSymbols [Text]

data FindDeadCodeReady = FindDeadCodeReady
  { findDeadCodeSummary :: Text,
    findDeadCodeHasDeadDefinitions :: Bool,
    findDeadCodePage :: Maybe (Paginated RenderedDeadDefinition),
    findDeadCodeWarnings :: [Text],
    findDeadCodePartialLoadWarning :: Maybe PartialLoadWarning
  }

data RenderedDeadDefinition = RenderedDeadDefinition
  { renderedDeadDefinitionModuleName :: Text,
    renderedDeadDefinitionSymbolNames :: Set.Set Text
  }

findDeadCode :: (MonadLore m) => FindDeadCodeOptions -> m FindDeadCodeResult
findDeadCode FindDeadCodeOptions {findDeadCodeModules, findDeadCodePageRequest} = do
  withLoadedSession \session -> do
    let partialLoadWarning =
          loadedSessionPartialWarning session "Dead-code results may be incomplete."
    eiTargetModules <- resolveOptionalLoadedHomeModules findDeadCodeModules
    case eiTargetModules of
      Left unresolvedModules ->
        pure $
          FindDeadCodeFailed
            FindDeadCodeFailure
              { findDeadCodeFailureReason = FindDeadCodeUnresolvedModules unresolvedModules,
                findDeadCodeFailurePartialLoadWarning = partialLoadWarning
              }
      Right targetModules -> do
        eiConfig <- loadLoreConfig
        case eiConfig of
          Left configError ->
            pure $
              FindDeadCodeFailed
                FindDeadCodeFailure
                  { findDeadCodeFailureReason = FindDeadCodeInvalidConfig configError,
                    findDeadCodeFailurePartialLoadWarning = partialLoadWarning
                  }
          Right config -> do
            eiAliveModules <- resolveLoadedHomeModulesByName config.loreConfigAliveModules
            case eiAliveModules of
              Left unresolvedAliveModules ->
                pure $
                  FindDeadCodeFailed
                    FindDeadCodeFailure
                      { findDeadCodeFailureReason = FindDeadCodeUnresolvedAliveModules unresolvedAliveModules,
                        findDeadCodeFailurePartialLoadWarning = partialLoadWarning
                      }
              Right resolvedAliveModules -> do
                eiAliveNames <- resolveAliveRootNames config.loreConfigAliveSymbols
                case eiAliveNames of
                  Left unresolvedSymbols ->
                    pure $
                      FindDeadCodeFailed
                        FindDeadCodeFailure
                          { findDeadCodeFailureReason = FindDeadCodeUnresolvedSymbols unresolvedSymbols,
                            findDeadCodeFailurePartialLoadWarning = partialLoadWarning
                          }
                  Right (Left invalidAliveSymbols) ->
                    pure $
                      FindDeadCodeFailed
                        FindDeadCodeFailure
                          { findDeadCodeFailureReason = FindDeadCodeInvalidAliveSymbols invalidAliveSymbols,
                            findDeadCodeFailurePartialLoadWarning = partialLoadWarning
                          }
                  Right (Right aliveRootNames) -> do
                    deadCodeResult <-
                      Core.findDeadCode
                        Core.DeadCodeOptions
                          { deadCodeTargetModules = targetModules,
                            deadCodeAliveModules = resolvedAliveModules,
                            deadCodeAliveNames = aliveRootNames
                          }
                    let deadDefinitions =
                          filterRenderableDeadDefinitions deadCodeResult.deadCodeDeadDefinitions
                        hasDeadDefinitions = not (null deadDefinitions)
                        summaryLine = renderSummary deadCodeResult deadDefinitions
                        maybePage = paginateItemsWithPageRequest pageRequest deadDefinitions
                    pure $
                      FindDeadCodeReadyResult
                        FindDeadCodeReady
                          { findDeadCodeSummary = summaryLine,
                            findDeadCodeHasDeadDefinitions = hasDeadDefinitions,
                            findDeadCodePage = renderDeadDefinitionPage <$> maybePage,
                            findDeadCodeWarnings = deadCodeResult.deadCodeWarnings,
                            findDeadCodePartialLoadWarning = partialLoadWarning
                          }
  where
    pageRequest =
      maybe defaultPageRequest id findDeadCodePageRequest

resolveOptionalLoadedHomeModules ::
  (MonadLore m) =>
  Maybe [Text] ->
  m (Either [Text] (Maybe (Set.Set GHC.Module)))
resolveOptionalLoadedHomeModules maybeModuleNames =
  case maybeModuleNames of
    Nothing ->
      pure (Right Nothing)
    Just moduleNames -> do
      eiResolved <- resolveLoadedHomeModulesByName moduleNames
      pure (fmap Just eiResolved)

resolveLoadedHomeModulesByName ::
  (MonadLore m) =>
  [Text] ->
  m (Either [Text] (Set.Set GHC.Module))
resolveLoadedHomeModulesByName requestedModuleNames = do
  moduleGraph <- GHC.getModuleGraph
  let loadedModules =
        map GHC.ms_mod (GHC.mgModSummaries moduleGraph)
      loadedModulesByName =
        Map.fromListWith
          (++)
          [ (renderModuleName module_, [module_])
          | module_ <- loadedModules
          ]
  pure $
    let resolutions =
          map (resolveOne loadedModulesByName) requestedModuleNames
        unresolvedMessages =
          [ message
          | Left message <- resolutions
          ]
        resolvedModules =
          [ module_
          | Right module_ <- resolutions
          ]
     in if null unresolvedMessages
          then Right (Set.fromList resolvedModules)
          else Left unresolvedMessages
  where
    resolveOne loadedModulesByName requestedModuleName =
      case Map.lookup requestedModuleName loadedModulesByName of
        Nothing ->
          Left ("Module " <> quoteText requestedModuleName <> " is not present in the loaded home module graph.")
        Just [module_] ->
          Right module_
        Just _ ->
          Left ("Module " <> quoteText requestedModuleName <> " is ambiguous in the loaded home module graph.")

resolveAliveRootNames ::
  (MonadLore m) =>
  [Text] ->
  m (Either SymbolsUnresolved (Either [Text] (Set.Set GHC.Name)))
resolveAliveRootNames queries =
  if null queries
    then pure (Right (Right Set.empty))
    else do
      eiResolved <- resolveUniqueSymbolQueries queries
      case eiResolved of
        Left unresolved ->
          pure (Left unresolved)
        Right resolved -> do
          validations <- mapM resolveOneAliveQuery resolved.resolvedQueries
          let invalidMessages =
                [ invalidMessage
                | Left invalidMessage <- validations
                ]
              validRootNames =
                [ name
                | Right name <- validations
                ]
          pure $
            if null invalidMessages
              then Right (Right (Set.fromList validRootNames))
              else Right (Left invalidMessages)

resolveOneAliveQuery ::
  (MonadLore m) =>
  ResolvedSymbolQuery ->
  m (Either Text GHC.Name)
resolveOneAliveQuery resolvedQuery = do
  maybeAliveName <- firstAliveName candidates
  pure $
    case maybeAliveName of
      Just aliveName ->
        Right aliveName
      Nothing ->
        Left $
          "Symbol "
            <> quoteText resolvedQuery.queryText
            <> " resolved, but it is not a loaded home definition and cannot be used as a dead-code root."
  where
    candidates =
      dedupeNames [resolvedQuery.resolvedSymbol.name, resolvedQuery.resolvedRootName]

firstAliveName :: (MonadLore m) => [GHC.Name] -> m (Maybe GHC.Name)
firstAliveName [] =
  pure Nothing
firstAliveName (name : restNames) = do
  maybeSource <- resolveDefinitionSourceNamed name
  case maybeSource of
    Just _ ->
      pure (Just name)
    Nothing ->
      firstAliveName restNames

dedupeNames :: [GHC.Name] -> [GHC.Name]
dedupeNames =
  foldr
    ( \name deduped ->
        if name `elem` deduped
          then deduped
          else name : deduped
    )
    []

renderSummary :: DeadCodeResult -> [DeadDefinition] -> Text
renderSummary deadCodeResult deadDefinitions =
  "Scanned "
    <> T.pack (show deadCodeResult.deadCodeTotalDefinitions)
    <> " definitions: "
    <> T.pack (show deadCodeResult.deadCodeAliveDefinitions)
    <> " alive, "
    <> T.pack (show (length deadDefinitions))
    <> " dead."

renderDeadDefinitionPage ::
  Paginated DeadDefinition ->
  Paginated RenderedDeadDefinition
renderDeadDefinitionPage page =
  page
    { paginatedItems = map renderDeadDefinition page.paginatedItems
    }

renderDeadDefinition :: DeadDefinition -> RenderedDeadDefinition
renderDeadDefinition deadDefinition =
  RenderedDeadDefinition
    { renderedDeadDefinitionModuleName =
        renderModuleName deadDefinition.deadDefinitionSource.definitionSourceModule,
      renderedDeadDefinitionSymbolNames =
        Set.map renderSymbolName deadDefinition.deadDefinitionNames
    }

instance ToLoreDoc FindDeadCodeOutput where
  toLoreDoc = renderFindDeadCodeOutput

instance ToLoreDoc FindDeadCodeFailure where
  toLoreDoc failed =
    mconcat
      [ toLoreDoc failed.findDeadCodeFailureReason,
        maybe mempty toLoreDoc failed.findDeadCodeFailurePartialLoadWarning
      ]

instance ToLoreDoc FindDeadCodeFailureReason where
  toLoreDoc = \case
    FindDeadCodeUnresolvedModules unresolved ->
      paragraph (T.intercalate "\n" unresolved)
    FindDeadCodeInvalidConfig configError ->
      paragraph configError
    FindDeadCodeUnresolvedAliveModules unresolved ->
      paragraph (T.intercalate "\n" unresolved)
    FindDeadCodeUnresolvedSymbols unresolvedSymbols ->
      paragraph (unresolvedSymbolQueriesMessage unresolvedSymbols)
    FindDeadCodeInvalidAliveSymbols invalidSymbols ->
      paragraph (T.intercalate "\n" invalidSymbols)

instance ToLoreDoc FindDeadCodeReady where
  toLoreDoc = renderFindDeadCodeReady

renderFindDeadCodeOutput :: FindDeadCodeOutput -> LoreDoc
renderFindDeadCodeOutput = \case
  FindDeadCodeFailed failed ->
    toLoreDoc failed
  FindDeadCodeReadyResult ready ->
    toLoreDoc ready

renderFindDeadCodeReady :: FindDeadCodeReady -> LoreDoc
renderFindDeadCodeReady ready =
  case ready.findDeadCodePage of
    Nothing ->
      mconcat
        [ paragraph ready.findDeadCodeSummary,
          paragraph "No dead definitions found.",
          renderDeadCodeWarnings ready.findDeadCodeWarnings,
          maybe mempty toLoreDoc ready.findDeadCodePartialLoadWarning
        ]
    Just page ->
      mconcat
        [ paragraph ready.findDeadCodeSummary,
          paginationSummaryDoc
            PaginationRenderConfig
              { paginationItemLabel = "dead definitions",
                paginationSkipArgName = Just "skip"
              }
            page,
          renderDeadDefinitionListing page.paginatedItems,
          renderDeadCodeWarnings ready.findDeadCodeWarnings,
          maybe mempty toLoreDoc ready.findDeadCodePartialLoadWarning
        ]

renderDeadDefinitionListing :: [RenderedDeadDefinition] -> LoreDoc
renderDeadDefinitionListing renderedDeadDefinitions =
  mconcat
    ( map renderDeadDefinitionGroup
        (Map.toAscList (groupDeadDefinitionsByModule renderedDeadDefinitions))
    )

groupDeadDefinitionsByModule :: [RenderedDeadDefinition] -> Map.Map Text [Text]
groupDeadDefinitionsByModule =
  foldl'
    ( \grouped rendered ->
        Map.insertWith
          (flip (++))
          rendered.renderedDeadDefinitionModuleName
          (Set.toAscList rendered.renderedDeadDefinitionSymbolNames)
          grouped
    )
    Map.empty

renderDeadDefinitionGroup :: (Text, [Text]) -> LoreDoc
renderDeadDefinitionGroup (moduleName, symbolNames) =
  case symbolNames of
    [] ->
      mempty
    [symbolName] ->
      paragraph (moduleName <> "." <> symbolName)
    multipleSymbolNames ->
      paragraph
        ( moduleName
            <> ":\n"
            <> T.intercalate "\n" (map ("- " <>) multipleSymbolNames)
        )

renderDeadCodeWarnings :: [Text] -> LoreDoc
renderDeadCodeWarnings warnings =
  mconcat
    [ paragraph ("Warning: " <> warningText)
    | warningText <- warnings
    ]

filterRenderableDeadDefinitions :: [DeadDefinition] -> [DeadDefinition]
filterRenderableDeadDefinitions =
  mapMaybe normalizeDeadDefinition

normalizeDeadDefinition :: DeadDefinition -> Maybe DeadDefinition
normalizeDeadDefinition deadDefinition =
  let renderableNames =
        Set.filter isRenderableDeadCodeName deadDefinition.deadDefinitionNames
   in if Set.null renderableNames
        then Nothing
        else
          Just
            deadDefinition
              { deadDefinitionNames = renderableNames
              }

isRenderableDeadCodeName :: GHC.Name -> Bool
isRenderableDeadCodeName name =
  not ("$" `T.isPrefixOf` renderSymbolName name)
