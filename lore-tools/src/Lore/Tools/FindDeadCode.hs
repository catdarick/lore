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

import Data.List (foldl')
import Data.Maybe (mapMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC
import qualified Lore as Core
import Lore
  ( DeadCodeOptions (..),
    DeadCodeResult (..),
    DeadDefinition (..),
    DefinitionSource (..),
    MonadLore,
  )
import Lore.Tools.FindDeadCode.Request
  ( FindDeadCodeFailureReason (..),
    FindDeadCodeOptions (..),
    ResolvedFindDeadCodeRequest (..),
    resolveFindDeadCodeRequest,
  )
import Lore.Tools.Render.Doc (LoreDoc, ToLoreDoc (toLoreDoc), paragraph)
import Lore.Tools.Render.Text (renderModuleName, renderSymbolName)
import Lore.Tools.Result
  ( Paginated (..),
    PaginationRenderConfig (..),
    PartialLoadWarning,
    ToolRun (..),
    defaultPageRequest,
    loadedSessionPartialWarning,
    paginateItemsWithPageRequest,
    paginationSummaryDoc,
    withLoadedSession,
  )

type FindDeadCodeResult = ToolRun FindDeadCodeOutput

data FindDeadCodeOutput
  = FindDeadCodeFailed FindDeadCodeFailure
  | FindDeadCodeReadyResult FindDeadCodeReady

data FindDeadCodeFailure = FindDeadCodeFailure
  { findDeadCodeFailureReason :: FindDeadCodeFailureReason,
    findDeadCodeFailurePartialLoadWarning :: Maybe PartialLoadWarning
  }

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
findDeadCode options@FindDeadCodeOptions {findDeadCodePageRequest} = do
  withLoadedSession \session -> do
    let partialLoadWarning =
          loadedSessionPartialWarning session "Dead-code results may be incomplete."
    eiRequest <- resolveFindDeadCodeRequest options
    case eiRequest of
      Left failureReason ->
        pure $
          FindDeadCodeFailed
            FindDeadCodeFailure
              { findDeadCodeFailureReason = failureReason,
                findDeadCodeFailurePartialLoadWarning = partialLoadWarning
              }
      Right resolvedRequest -> do
        deadCodeResult <-
          Core.findDeadCode
            Core.DeadCodeOptions
              { deadCodeTargetModules = resolvedRequest.resolvedDeadCodeTargetModules,
                deadCodeAliveModules = resolvedRequest.resolvedDeadCodeAliveModules,
                deadCodeAliveNames = resolvedRequest.resolvedDeadCodeAliveNames
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
