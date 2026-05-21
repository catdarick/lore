module Lore.Mcp.Tools.LookupInstances
  ( lookupInstancesTool,
  )
where

import qualified Data.Aeson as J
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.OpenApi (ToSchema)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC.Core.FamInstEnv as GHC (FamInst)
import qualified GHC.Core.InstEnv as GHC (ClsInst)
import GHC.Generics (Generic)
import qualified GHC.Plugins as Plugins
import Lore
  ( Instances (..),
    MonadLore,
    listIntersectingInstances,
  )
import Lore.Mcp.Internal.Annotated
  ( Description,
    Example,
    ExampleList,
    Field,
    FieldType (..),
    MinItems,
    WithMeta,
  )
import Lore.Mcp.Internal.LoreDoc (ToLoreDoc (toLoreDoc), bulletList, paragraph)
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
import Lore.Mcp.Tools.Shared.Outputable (renderOutputable)
import Lore.Mcp.Tools.Shared.SymbolResolution
  ( ResolvedSymbolQuery (resolvedRootName),
    SymbolsResolved (resolvedQueries),
    SymbolsUnresolved,
    resolveUniqueSymbolQueries,
  )

data LookupInstancesArgs (fieldType :: FieldType) = LookupInstancesArgs
  { names ::
      Field fieldType [Text]
        `WithMeta` '[ Description "Provide two or more symbol names. Module qualification (e.g., Some.Module.someFunction) is supported and can be used to resolve ambiguity or provide specific scope.",
                      ExampleList '["Show", "Int", "Some.Module.someFunction"],
                      MinItems 2
                    ],
    skip ::
      Maybe (Field fieldType Int)
        `WithMeta` '[ Description "Used for pagination. Number of initial results to skip. Use it only if a previous result was truncated and you want to see the next page of results.",
                      Example 5
                    ]
  }
  deriving stock (Generic)

instance J.FromJSON (LookupInstancesArgs 'ValueType)

instance ToSchema (LookupInstancesArgs 'MetadataType)

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

lookupInstancesTool :: (MonadLore m) => SomeTool m
lookupInstancesTool =
  SomeToolWithArgs
    ToolWithArgs
      { name = "lookupInstances",
        description = Just "Find loaded class or family instance declarations whose instance head mentions all queried symbols. This matches what is currently indexed in the loaded session; it does not infer likely instances beyond the indexed results. Example: [\"Show\", \"Int\"] matches `instance Show Int`; [\"Int\", \"String\"] matches only instances where both types appear together.",
        handler = lookupInstancesHandler
      }

lookupInstancesHandler :: (MonadLore m) => LookupInstancesArgs 'ValueType -> m LookupInstancesResult
lookupInstancesHandler LookupInstancesArgs {names, skip} = do
  withLoadedSession \session -> do
    let partialLoadWarning =
          loadedSessionPartialWarning session "Instance lookup results may be incomplete."
    eiResolved <- resolveUniqueSymbolQueries names
    case eiResolved of
      Left unresolved ->
        pure
          (LookupInstancesFailed LookupInstancesFailure {lookupInstancesFailureReason = LookupInstancesUnresolvedSymbols unresolved, lookupInstancesFailurePartialLoadWarning = partialLoadWarning})
      Right resolved -> do
        let resolvedRootNames = dedupeNamesBy renderName (map (.resolvedRootName) resolved.resolvedQueries)
        intersectingInstances <- listIntersectingInstances resolvedRootNames
        pure
          (LookupInstancesReadyResult LookupInstancesReady {lookupInstancesQueriedNames = names, lookupInstancesPage = paginateMatchingInstances resolvedSkip (toMatchingInstances intersectingInstances), lookupInstancesPartialLoadWarning = partialLoadWarning})
  where
    resolvedSkip =
      max 0 (fromMaybe 0 skip)

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

paginateMatchingInstances :: Int -> [MatchingInstance] -> Maybe (Paginated MatchingInstance)
paginateMatchingInstances skip =
  paginateItems skip maxRenderedMatchingInstances

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
  reverse . snd . foldl' dedupeName (Set.empty, [])
  where
    dedupeName (seenKeys, dedupedNames) name =
      let key = renderKey name
       in if key `Set.member` seenKeys
            then (seenKeys, dedupedNames)
            else (Set.insert key seenKeys, name : dedupedNames)

maxRenderedMatchingInstances :: Int
maxRenderedMatchingInstances = 25

renderQuotedNames :: [Text] -> Text
renderQuotedNames names =
  "[" <> T.intercalate ", " (map quoteText names) <> "]"
  where
    quoteText value = "\"" <> value <> "\""
