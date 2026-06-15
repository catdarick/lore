module Lore.Tools.LookupInstances
  ( LookupInstancesOptions (..),
    LookupInstancesResult,
    LookupInstancesOutput (..),
    LookupInstancesFailure (..),
    LookupInstancesFailureReason (..),
    LookupInstancesReady (..),
    lookupInstances,
  )
where

import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Set as Set
import qualified GHC.Core.FamInstEnv as GHC (FamInst)
import qualified GHC.Core.InstEnv as GHC (ClsInst)
import qualified GHC.Plugins as Plugins
import Lore
  ( Instances (..),
    MonadLore,
    listIntersectingInstances,
  )
import Lore.Tools.Internal.SymbolResolution
  ( ResolvedSymbolQuery (resolvedRootName),
    SymbolsResolved (resolvedQueries),
    SymbolsUnresolved,
    resolveUniqueSymbolQueries,
  )
import Lore.Tools.Render.Doc (ToLoreDoc (toLoreDoc), bulletList, paragraph)
import Lore.Tools.Render.Ghc (renderOutputable)
import Lore.Tools.Result
  ( Paginated (..),
    PaginationRenderConfig (..),
    PageRequest (..),
    PartialLoadWarning,
    ToolRun (..),
    loadedSessionPartialWarning,
    paginateItemsWithPageRequest,
    paginationSummaryDoc,
    withLoadedSession,
  )

data LookupInstancesOptions = LookupInstancesOptions
  { lookupInstancesNames :: [Text],
    lookupInstancesPageRequest :: PageRequest
  }
  deriving stock (Eq, Show)

type LookupInstancesResult = ToolRun LookupInstancesOutput

data LookupInstancesOutput
  = LookupInstancesFailed LookupInstancesFailure
  | LookupInstancesReadyResult LookupInstancesReady

data LookupInstancesFailure = LookupInstancesFailure
  { lookupInstancesFailureReason :: LookupInstancesFailureReason,
    lookupInstancesFailurePartialLoadWarning :: Maybe PartialLoadWarning
  }

data LookupInstancesFailureReason
  = LookupInstancesUnresolvedSymbols SymbolsUnresolved
  | LookupInstancesInternalError Text

data LookupInstancesReady = LookupInstancesReady
  { lookupInstancesQueriedNames :: [Text],
    lookupInstancesPage :: Maybe (Paginated MatchingInstance),
    lookupInstancesPartialLoadWarning :: Maybe PartialLoadWarning
  }

instance ToLoreDoc LookupInstancesOutput where
  toLoreDoc = \case
    LookupInstancesFailed failed ->
      toLoreDoc failed
    LookupInstancesReadyResult ready ->
      toLoreDoc ready

instance ToLoreDoc LookupInstancesFailure where
  toLoreDoc failed =
    mconcat
      [ toLoreDoc failed.lookupInstancesFailureReason,
        maybe mempty toLoreDoc failed.lookupInstancesFailurePartialLoadWarning
      ]

instance ToLoreDoc LookupInstancesFailureReason where
  toLoreDoc = \case
    LookupInstancesUnresolvedSymbols unresolved ->
      toLoreDoc unresolved
    LookupInstancesInternalError message ->
      paragraph message

instance ToLoreDoc LookupInstancesReady where
  toLoreDoc ready =
    case ready.lookupInstancesPage of
      Nothing ->
        mconcat
          [ paragraph ("Found 0 matching instances for " <> renderQuotedNames ready.lookupInstancesQueriedNames <> "."),
            maybe mempty toLoreDoc ready.lookupInstancesPartialLoadWarning
          ]
      Just page ->
        mconcat
          [ paginationSummaryDoc
              PaginationRenderConfig
                { paginationItemLabel = "matching instances",
                  paginationSkipArgName = Just "skip"
                }
              page,
            bulletList (map (paragraph . matchingInstanceLabel) page.paginatedItems),
            maybe mempty toLoreDoc ready.lookupInstancesPartialLoadWarning
          ]

lookupInstances :: (MonadLore m) => LookupInstancesOptions -> m LookupInstancesResult
lookupInstances LookupInstancesOptions {lookupInstancesNames, lookupInstancesPageRequest} =
  withLoadedSession \session -> do
    let partialLoadWarning =
          loadedSessionPartialWarning session "Instance lookup results may be incomplete."
    eiResolved <- resolveUniqueSymbolQueries lookupInstancesNames
    case eiResolved of
      Left unresolved ->
        pure
          (LookupInstancesFailed LookupInstancesFailure {lookupInstancesFailureReason = LookupInstancesUnresolvedSymbols unresolved, lookupInstancesFailurePartialLoadWarning = partialLoadWarning})
      Right resolved -> do
        let resolvedRootNames = dedupeNamesBy renderName (map (.resolvedRootName) resolved.resolvedQueries)
        intersectingInstances <- listIntersectingInstances resolvedRootNames
        pure
          (LookupInstancesReadyResult LookupInstancesReady {lookupInstancesQueriedNames = lookupInstancesNames, lookupInstancesPage = paginateMatchingInstances lookupInstancesPageRequest (toMatchingInstances intersectingInstances), lookupInstancesPartialLoadWarning = partialLoadWarning})

data MatchingInstance
  = MatchingClassInstance GHC.ClsInst
  | MatchingFamilyInstance GHC.FamInst

matchingInstanceLabel :: MatchingInstance -> Text
matchingInstanceLabel matchingInstance =
  case matchingInstance of
    MatchingClassInstance classInstance ->
      renderOutputable classInstance
    MatchingFamilyInstance familyInstance ->
      renderOutputable familyInstance

paginateMatchingInstances :: PageRequest -> [MatchingInstance] -> Maybe (Paginated MatchingInstance)
paginateMatchingInstances pageRequest =
  paginateItemsWithPageRequest pageRequest

toMatchingInstances :: Instances -> [MatchingInstance]
toMatchingInstances instances_ =
  map MatchingClassInstance instances_.classInstances
    <> map MatchingFamilyInstance instances_.familyInstances

renderName :: Plugins.Name -> String
renderName name =
  case Plugins.nameModule_maybe name of
    Nothing ->
      "<no-module>." <> Plugins.getOccString name
    Just module_ ->
      Plugins.moduleNameString (Plugins.moduleName module_) <> "." <> Plugins.getOccString name

dedupeNamesBy :: (Ord key) => (name -> key) -> [name] -> [name]
dedupeNamesBy renderKey =
  reverse . snd . List.foldl' dedupeName (Set.empty, [])
  where
    dedupeName (seenKeys, dedupedNames) name =
      let key = renderKey name
       in if key `Set.member` seenKeys
            then (seenKeys, dedupedNames)
            else (Set.insert key seenKeys, name : dedupedNames)

renderQuotedNames :: [Text] -> Text
renderQuotedNames names =
  "[" <> T.intercalate ", " (map quoteText names) <> "]"
  where
    quoteText value = "\"" <> value <> "\""
