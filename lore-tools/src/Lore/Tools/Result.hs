module Lore.Tools.Result
  ( ToolRun (..),
    ToolBlocked (..),
    ResultLimit (..),
    PageRequest (..),
    defaultPageRequest,
    normalizePageRequest,
    LoadedSession (..),
    withLoadedSession,
    withInterpreterSession,
    PartialLoadWarning (..),
    partialLoadWarningFromLoadResult,
    partialLoadWarningDoc,
    withPartialLoadWarning,
    loadedSessionPartialWarning,
    renderPartialLoadWarning,
    paginateItemsWithPageRequest,
    Paginated (..),
    PaginationRenderConfig (..),
    paginationSummaryDoc,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Lore
  ( LoadHomeModulesOptions (..),
    LoadHomeModulesResult (..),
    MonadLore,
    interpreterContextIsReady,
    loadHomeModules,
    lookupLastLoadHomeModulesResult,
  )
import Lore.Tools.Render.Doc (LoreDoc, ToLoreDoc (toLoreDoc), paragraph)

data ToolRun a
  = ToolRunBlocked ToolBlocked
  | ToolRunReady a

data ToolBlocked
  = InterpreterContextNotReady

data ResultLimit
  = Unlimited
  | Limit Int
  deriving stock (Eq, Show)

data PageRequest = PageRequest
  { pageOffset :: Int,
    pageLimit :: ResultLimit
  }
  deriving stock (Eq, Show)

defaultPageRequest :: PageRequest
defaultPageRequest =
  PageRequest
    { pageOffset = 0,
      pageLimit = Unlimited
    }

normalizePageRequest :: PageRequest -> PageRequest
normalizePageRequest request =
  request
    { pageOffset = max 0 request.pageOffset,
      pageLimit =
        case request.pageLimit of
          Unlimited -> Unlimited
          Limit limit -> Limit (max 0 limit)
    }

newtype LoadedSession = LoadedSession
  { loadedSessionLoadResult :: LoadHomeModulesResult
  }

withLoadedSession ::
  (MonadLore m) =>
  (LoadedSession -> m a) ->
  m (ToolRun a)
withLoadedSession action = do
  loadResult <- ensureLoadedSession
  ToolRunReady <$> action (LoadedSession loadResult)

withInterpreterSession ::
  (MonadLore m) =>
  (LoadedSession -> m a) ->
  m (ToolRun a)
withInterpreterSession action = do
  loadResult <- ensureLoadedSession
  ready <- interpreterContextIsReady
  if ready
    then ToolRunReady <$> action (LoadedSession loadResult)
    else pure (ToolRunBlocked InterpreterContextNotReady)

instance (ToLoreDoc a) => ToLoreDoc (ToolRun a) where
  toLoreDoc = \case
    ToolRunBlocked blocked ->
      toLoreDoc blocked
    ToolRunReady value ->
      toLoreDoc value

instance ToLoreDoc ToolBlocked where
  toLoreDoc = \case
    InterpreterContextNotReady ->
      paragraph "Interpreter context is not ready. Run reloadHomeModules again."

ensureLoadedSession :: (MonadLore m) => m LoadHomeModulesResult
ensureLoadedSession = do
  maybeLoadResult <- lookupLastLoadHomeModulesResult
  case maybeLoadResult of
    Just loadResult ->
      pure loadResult
    Nothing ->
      loadHomeModules LoadHomeModulesOptions {enableAutoRefactor = True}

data PartialLoadWarning = PartialLoadWarning
  { partialLoadLoaded :: Int,
    partialLoadTotal :: Int,
    partialLoadSuffix :: Text
  }

partialLoadWarningFromLoadResult :: LoadHomeModulesResult -> Text -> Maybe PartialLoadWarning
partialLoadWarningFromLoadResult loadResult partialLoadSuffix
  | loadResult.loadHomeModulesFailed > 0 =
      Just
        PartialLoadWarning
          { partialLoadLoaded = loadResult.loadHomeModulesLoaded,
            partialLoadTotal = loadResult.loadHomeModulesTotal,
            partialLoadSuffix
          }
  | otherwise =
      Nothing

partialLoadWarningDoc :: Maybe PartialLoadWarning -> LoreDoc
partialLoadWarningDoc =
  maybe mempty toLoreDoc

withPartialLoadWarning :: Maybe PartialLoadWarning -> LoreDoc -> LoreDoc
withPartialLoadWarning warning body =
  body <> partialLoadWarningDoc warning

loadedSessionPartialWarning ::
  LoadedSession ->
  Text ->
  Maybe PartialLoadWarning
loadedSessionPartialWarning session suffix =
  partialLoadWarningFromLoadResult session.loadedSessionLoadResult suffix

renderPartialLoadWarning :: PartialLoadWarning -> Text
renderPartialLoadWarning warning =
  "Warning: only "
    <> T.pack (show warning.partialLoadLoaded)
    <> " of "
    <> T.pack (show warning.partialLoadTotal)
    <> " modules loaded successfully. "
    <> warning.partialLoadSuffix

data Paginated a = Paginated
  { paginatedTotalItems :: Int,
    paginatedSkippedItems :: Int,
    -- Number of items actually rendered.
    paginatedShownItems :: Int,
    -- Number of items consumed from the underlying result stream.
    -- This can be greater than shownItems when entries are omitted or filtered.
    paginatedConsumedItems :: Int,
    paginatedItems :: [a]
  }

data PaginationRenderConfig = PaginationRenderConfig
  { paginationItemLabel :: Text,
    paginationSkipArgName :: Maybe Text
  }

paginationSummaryDoc :: PaginationRenderConfig -> Paginated a -> LoreDoc
paginationSummaryDoc config paginated =
  paragraph (T.intercalate "\n" (showingLine : overflowLines))
  where
    showingLine
      | paginated.paginatedSkippedItems == 0 && paginated.paginatedShownItems == paginated.paginatedTotalItems =
          "Showing all " <> T.pack (show paginated.paginatedTotalItems) <> " " <> config.paginationItemLabel <> "."
      | otherwise =
          "Showing "
            <> T.pack (show paginated.paginatedShownItems)
            <> " of "
            <> T.pack (show paginated.paginatedTotalItems)
            <> " "
            <> config.paginationItemLabel
            <> skippedSuffix
            <> "."

    skippedSuffix
      | paginated.paginatedSkippedItems > 0 =
          ", after skipping "
            <> T.pack (show paginated.paginatedSkippedItems)
      | otherwise =
          ""

    overflowLines
      | remaining > 0 && nextSkip > paginated.paginatedSkippedItems =
          [ "And "
              <> T.pack (show remaining)
              <> " more "
              <> config.paginationItemLabel
              <> nextSkipHint
              <> "."
          ]
      | otherwise =
          []

    nextSkipHint =
      case config.paginationSkipArgName of
        Nothing ->
          ""
        Just skipArgName ->
          " (set "
            <> skipArgName
            <> " to "
            <> T.pack (show nextSkip)
            <> " to get the next page if required)"

    remaining =
      max
        0
        ( paginated.paginatedTotalItems
            - paginated.paginatedSkippedItems
            - paginated.paginatedConsumedItems
        )

    nextSkip =
      paginated.paginatedSkippedItems + paginated.paginatedConsumedItems

instance ToLoreDoc PartialLoadWarning where
  toLoreDoc =
    paragraph . renderPartialLoadWarning

paginateItemsWithPageRequest :: PageRequest -> [a] -> Maybe (Paginated a)
paginateItemsWithPageRequest request items =
  case items of
    [] ->
      Nothing
    _ ->
      Just
        Paginated
          { paginatedTotalItems = totalItems,
            paginatedSkippedItems = skippedItems,
            paginatedShownItems = length visibleItems,
            paginatedConsumedItems = length visibleItems,
            paginatedItems = visibleItems
          }
  where
    normalizedRequest = normalizePageRequest request
    totalItems = length items
    skippedItems = min normalizedRequest.pageOffset totalItems
    visibleItems =
      case normalizedRequest.pageLimit of
        Unlimited ->
          drop skippedItems items
        Limit maxItems ->
          take maxItems (drop skippedItems items)
