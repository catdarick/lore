module Lore.Mcp.Tools.FindDeadCode
  ( findDeadCodeTool,
  )
where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as J
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.OpenApi (ToSchema)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC
import GHC.Generics (Generic)
import Lore
  ( DeadCodeOptions (..),
    DeadCodeResult (..),
    DeadDefinition (..),
    DefinitionSource (..),
    MonadLore,
    Symbol (..),
    findDeadCode,
    resolveDefinitionSourceNamed,
  )
import Lore.Mcp.Config (LoreConfig (..), loadLoreConfig)
import Lore.Mcp.Internal.Annotated
  ( Description,
    Example,
    ExampleList,
    Field,
    FieldType (..),
    WithMeta,
  )
import Lore.Mcp.Internal.LoreDoc (LoreDoc, ToLoreDoc (toLoreDoc), numberedListFrom, paragraph)
import Lore.Mcp.Internal.Tool (SomeTool (..), ToolWithArgs (..))
import Lore.Mcp.Tools.Shared
  ( Paginated (..),
    PaginationRenderConfig (..),
    PartialLoadWarning,
    ToolRun (..),
    loadedSessionPartialWarning,
    paginateItems,
    paginationSummaryDoc,
    withLoadedSession,
  )
import Lore.Mcp.Tools.Shared.Rendering (quoteText, renderModuleName, renderSymbolName)
import Lore.Mcp.Tools.Shared.Source (declarationSpansLineRange, definitionSourcePath)
import Lore.Mcp.Tools.Shared.SymbolResolution
  ( ResolvedSymbolQuery (..),
    SymbolsResolved (resolvedQueries),
    SymbolsUnresolved,
    resolveUniqueSymbolQueries,
    unresolvedSymbolQueriesMessage,
  )

data FindDeadCodeArgs (fieldType :: FieldType) = FindDeadCodeArgs
  { modules ::
      Maybe (Field fieldType [Text])
        `WithMeta` '[ Description "Only report dead definitions from these loaded home modules.",
                      ExampleList '["Demo", "Demo.Support"]
                    ],
    skip ::
      Maybe (Field fieldType Int)
        `WithMeta` '[ Description "Used for pagination. Number of initial dead definitions to skip.",
                      Example 25
                    ]
  }
  deriving stock (Generic)

instance J.FromJSON (FindDeadCodeArgs 'ValueType)

instance ToSchema (FindDeadCodeArgs 'MetadataType)

findDeadCodeTool :: (MonadLore m) => SomeTool m
findDeadCodeTool =
  SomeToolWithArgs
    ToolWithArgs
      { name = "findDeadCode",
        description = Just "Find top-level dead declarations using project-wide reachability over cached definition indexes.",
        handler = findDeadCodeHandler
      }

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
    findDeadCodePage :: Maybe (Paginated RenderedDeadDefinition),
    findDeadCodeWarnings :: [Text],
    findDeadCodePartialLoadWarning :: Maybe PartialLoadWarning
  }

data RenderedDeadDefinition = RenderedDeadDefinition
  { renderedDeadDefinitionLabel :: Text
  }

findDeadCodeHandler :: (MonadLore m) => FindDeadCodeArgs 'ValueType -> m FindDeadCodeResult
findDeadCodeHandler FindDeadCodeArgs {modules, skip} = do
  withLoadedSession \session -> do
    let partialLoadWarning =
          loadedSessionPartialWarning session "Dead-code results may be incomplete."
    eiTargetModules <- resolveOptionalLoadedHomeModules modules
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
                      findDeadCode
                        DeadCodeOptions
                          { deadCodeTargetModules = targetModules,
                            deadCodeAliveModules = resolvedAliveModules,
                            deadCodeAliveNames = aliveRootNames
                          }
                    let deadDefinitions = deadCodeResult.deadCodeDeadDefinitions
                        summaryLine = renderSummary deadCodeResult deadDefinitions
                    case paginateItems resolvedSkip maxResults deadDefinitions of
                      Nothing ->
                        pure $
                          FindDeadCodeReadyResult
                            FindDeadCodeReady
                              { findDeadCodeSummary = summaryLine,
                                findDeadCodePage = Nothing,
                                findDeadCodeWarnings = deadCodeResult.deadCodeWarnings,
                                findDeadCodePartialLoadWarning = partialLoadWarning
                              }
                      Just page -> do
                        renderedPage <- renderDeadDefinitionPage page
                        pure $
                          FindDeadCodeReadyResult
                            FindDeadCodeReady
                              { findDeadCodeSummary = summaryLine,
                                findDeadCodePage = Just renderedPage,
                                findDeadCodeWarnings = deadCodeResult.deadCodeWarnings,
                                findDeadCodePartialLoadWarning = partialLoadWarning
                              }
  where
    resolvedSkip =
      max 0 (fromMaybe 0 skip)
    maxResults = 100

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
      resolutions =
        map (resolveOne loadedModulesByName) requestedModuleNames
      unresolvedMessages =
        [ message
        | Left message <- resolutions
        ]
      resolvedModules =
        [ module_
        | Right module_ <- resolutions
        ]
  pure $
    if null unresolvedMessages
      then Right (Set.fromList resolvedModules)
      else Left unresolvedMessages
  where
    resolveOne loadedModulesByName requestedModuleName =
      case Map.lookup requestedModuleName loadedModulesByName of
        Nothing ->
          Left ("Module " <> quoteText requestedModuleName <> " is not present in the loaded home module graph.")
        Just [] ->
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
  (MonadLore m) =>
  Paginated DeadDefinition ->
  m (Paginated RenderedDeadDefinition)
renderDeadDefinitionPage page = do
  renderedItems <- mapM renderDeadDefinition page.paginatedItems
  pure
    page
      { paginatedItems = renderedItems
      }

renderDeadDefinition :: (MonadLore m) => DeadDefinition -> m RenderedDeadDefinition
renderDeadDefinition deadDefinition = do
  renderedPath <- liftIO (definitionSourcePath deadDefinition.deadDefinitionSource)
  let renderedModuleName =
        renderModuleName deadDefinition.deadDefinitionSource.definitionSourceModule
      renderedNames =
        T.intercalate ", " (map renderSymbolName (Set.toAscList deadDefinition.deadDefinitionNames))
      renderedLocation =
        case declarationSpansLineRange deadDefinition.deadDefinitionSource.definitionSourceSpans of
          Just (startLine, endLine) ->
            if startLine == endLine
              then renderedPath <> ":" <> T.pack (show startLine)
              else renderedPath <> ":" <> T.pack (show startLine) <> "-" <> T.pack (show endLine)
          Nothing ->
            renderedPath
  pure
    RenderedDeadDefinition
      { renderedDeadDefinitionLabel =
          renderedModuleName <> "." <> renderedNames <> " - " <> renderedLocation
      }

instance ToLoreDoc FindDeadCodeOutput where
  toLoreDoc = \case
    FindDeadCodeFailed failed ->
      toLoreDoc failed
    FindDeadCodeReadyResult ready ->
      toLoreDoc ready

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
  toLoreDoc ready =
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
            numberedListFrom
              (fromIntegral (page.paginatedSkippedItems + 1))
              (map (paragraph . (.renderedDeadDefinitionLabel)) page.paginatedItems),
            renderDeadCodeWarnings ready.findDeadCodeWarnings,
            maybe mempty toLoreDoc ready.findDeadCodePartialLoadWarning
          ]

renderDeadCodeWarnings :: [Text] -> LoreDoc
renderDeadCodeWarnings warnings =
  mconcat
    [ paragraph ("Warning: " <> warningText)
    | warningText <- warnings
    ]
