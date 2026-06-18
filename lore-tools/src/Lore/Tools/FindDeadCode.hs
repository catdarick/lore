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

import qualified Data.List as List
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC
import qualified Lore as Core
import Lore
  ( DeadCodeOptions (..),
    DeadCodeResult (..),
    DeadDefinition (..),
    DeadDefinitionKind (..),
    MonadLore,
    definitionSourceModule,
  )
import Lore.Tools.FindDeadCode.Request
  ( FindDeadCodeFailureReason (..),
    FindDeadCodeOptions (..),
    ResolvedFindDeadCodeRequest (..),
    resolveFindDeadCodeRequest,
  )
import Lore.Tools.Render.Doc (LoreDoc, ToLoreDoc (toLoreDoc), paragraph, heading3)
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
  { renderedDeadDefinitionKind :: DeadDefinitionKind,
    renderedDeadDefinitionModuleName :: Text,
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
            renderedDeadDefinitions =
              map renderDeadDefinition deadDefinitions
            hasDeadDefinitions = not (null renderedDeadDefinitions)
            summaryLine = renderSummary deadCodeResult deadDefinitions
            maybePage = paginateItemsWithPageRequest pageRequest renderedDeadDefinitions
        pure $
          FindDeadCodeReadyResult
            FindDeadCodeReady
              { findDeadCodeSummary = summaryLine,
                findDeadCodeHasDeadDefinitions = hasDeadDefinitions,
                findDeadCodePage = maybePage,
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
    <> T.pack (show safeDeadDefinitionCount)
    <> " safe-delete dead, "
    <> T.pack (show testOnlyDeadDefinitionCount)
    <> " test-only dead."
  where
    safeDeadDefinitionCount =
      countDeadDefinitionsByKind SafeDeleteDeadDefinition deadDefinitions
    testOnlyDeadDefinitionCount =
      countDeadDefinitionsByKind TestOnlyDeadDefinition deadDefinitions

countDeadDefinitionsByKind :: DeadDefinitionKind -> [DeadDefinition] -> Int
countDeadDefinitionsByKind kind =
  length . filter ((== kind) . deadDefinitionKind)

renderDeadDefinition :: DeadDefinition -> RenderedDeadDefinition
renderDeadDefinition deadDefinition =
  RenderedDeadDefinition
    { renderedDeadDefinitionKind = deadDefinition.deadDefinitionKind,
      renderedDeadDefinitionModuleName =
        renderModuleName (definitionSourceModule deadDefinition.deadDefinitionSource),
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
    [ renderDeadDefinitionSection "Completely unreachable" safeDeadDefinitions,
      renderDeadDefinitionSection "Only reachable from tests" testOnlyDeadDefinitions
    ]
  where
    (safeDeadDefinitions, testOnlyDeadDefinitions) =
      List.partition ((== SafeDeleteDeadDefinition) . renderedDeadDefinitionKind) renderedDeadDefinitions

renderDeadDefinitionSection :: Text -> [RenderedDeadDefinition] -> LoreDoc
renderDeadDefinitionSection title renderedDeadDefinitions =
  if null renderedDeadDefinitions
    then mempty
    else
      heading3 title
        <> mconcat
          (map renderDeadDefinitionGroup (groupAdjacentDeadDefinitionsByModule renderedDeadDefinitions))

groupAdjacentDeadDefinitionsByModule :: [RenderedDeadDefinition] -> [(Text, [Text])]
groupAdjacentDeadDefinitionsByModule [] =
  []
groupAdjacentDeadDefinitionsByModule (rendered : rest) =
  let (sameModule, remaining) =
        span ((== rendered.renderedDeadDefinitionModuleName) . renderedDeadDefinitionModuleName) rest
      moduleDefinitions =
        rendered : sameModule
   in ( rendered.renderedDeadDefinitionModuleName,
        concatMap (Set.toAscList . renderedDeadDefinitionSymbolNames) moduleDefinitions
      )
        : groupAdjacentDeadDefinitionsByModule remaining

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
