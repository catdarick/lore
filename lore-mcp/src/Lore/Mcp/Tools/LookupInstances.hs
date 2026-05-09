module Lore.Mcp.Tools.LookupInstances
  ( lookupInstancesTool,
  )
where

import qualified Data.Aeson as J
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
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
    PathToRoot (..),
    Symbol (..),
    findMatchingSymbols,
    listIntersectingInstances,
    lookupLastLoadTargetsResult,
    mergePathsToRootOn,
    parseAndNormalizeName,
    resolvePathToRoot,
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
import Lore.Mcp.Internal.Render
  ( ListMarker (..),
    RenderList (..),
    Renderable (..),
    Truncation (..),
    totalItems,
    (|>),
  )
import Lore.Mcp.Internal.Tool (SomeTool (..), ToolWithArgs (..))
import Lore.Mcp.Tools.Shared.Outputable (renderOutputable)
import Lore.Mcp.Tools.Shared.PartialLoadWarning (mkPartialWarning)

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

lookupInstancesTool :: (MonadLore m) => SomeTool m
lookupInstancesTool =
  SomeToolWithArgs
    ToolWithArgs
      { name = "lookupInstances",
        description = Just "Find loaded class or family instance declarations whose instance head mentions all queried symbols. This matches what is currently indexed in the loaded session; it does not infer likely instances beyond the indexed results. Example: [\"Show\", \"Int\"] matches `instance Show Int`; [\"Int\", \"String\"] matches only instances where both types appear together.",
        handler = lookupInstancesHandler
      }

lookupInstancesHandler :: (MonadLore m) => LookupInstancesArgs 'ValueType -> m Text
lookupInstancesHandler LookupInstancesArgs {names, skip} = do
  maybeLoadResult <- lookupLastLoadTargetsResult
  case maybeLoadResult of
    Nothing ->
      pure "Targets have not been loaded yet. Run reloadHomeModules first."
    Just loadResult -> do
      resolution <- resolveLookupNames names
      case resolution of
        Left ambiguousQueries ->
          pure $
            renderText $
              renderAmbiguousQueries names ambiguousQueries
                |> mkPartialWarning loadResult
        Right resolvedQueries -> do
          intersectingInstances <-
            if resolvedQueries.hasMissingQueries
              then pure (Instances [] [])
              else listIntersectingInstances resolvedQueries.resolvedNames
          let toRender =
                renderLookupInstancesResult resolvedSkip (toMatchingInstances intersectingInstances)
                  |> mkPartialWarning loadResult
          pure (renderText toRender)
  where
    resolvedSkip =
      max 0 (fromMaybe 0 skip)

renderLookupInstancesResult :: Int -> [MatchingInstance] -> Text
renderLookupInstancesResult skip lookupResult =
  case NE.nonEmpty lookupResult of
    Nothing ->
      "Found 0 matching instances."
    Just matchingInstances ->
      renderText (renderMatchingInstancesList skip matchingInstances)

data MatchingInstance
  = MatchingClassInstance GHC.ClsInst
  | MatchingFamilyInstance GHC.FamInst

data AmbiguousQuery = AmbiguousQuery
  { queryText :: Text,
    matchedRoots :: [Plugins.Name]
  }

data ResolvedQueries = ResolvedQueries
  { resolvedNames :: [Plugins.Name],
    hasMissingQueries :: Bool
  }

data LookupNameResolution
  = LookupNameMissing
  | LookupNameResolved Plugins.Name
  | LookupNameAmbiguous AmbiguousQuery

renderMatchingInstancesList :: Int -> NonEmpty MatchingInstance -> RenderList
renderMatchingInstancesList skip matchingInstances =
  RenderList
    { renderHeader =
        \ctx -> Just $ "Found " <> T.pack (show ctx.totalItems) <> " matching instances:",
      contentIndentWidth = 0,
      markerStyle = BulletMarker,
      itemsList = fmap RenderedMatchingInstance matchingInstances,
      skip = skip,
      truncation =
        Just
          Truncation
            { maxItems = maxRenderedMatchingInstances,
              itemName = "matching instances",
              skipArgName = Just "skip"
            }
    }

newtype RenderedMatchingInstance = RenderedMatchingInstance MatchingInstance

instance Renderable RenderedMatchingInstance where
  renderText (RenderedMatchingInstance matchingInstance) =
    case matchingInstance of
      MatchingClassInstance classInstance ->
        renderOutputable classInstance
      MatchingFamilyInstance familyInstance ->
        renderOutputable familyInstance

resolveLookupNames :: (MonadLore m) => [Text] -> m (Either [AmbiguousQuery] ResolvedQueries)
resolveLookupNames queries = do
  resolvedQueries <- mapM resolveLookupName queries
  let ambiguousQueries = [ambiguousQuery | LookupNameAmbiguous ambiguousQuery <- resolvedQueries]
      resolvedNames = [resolvedName | LookupNameResolved resolvedName <- resolvedQueries]
      hasMissingQueries = any isMissing resolvedQueries
  if null ambiguousQueries
    then
      pure $
        Right
          ResolvedQueries
            { resolvedNames,
              hasMissingQueries
            }
    else pure (Left ambiguousQueries)
  where
    isMissing = \case
      LookupNameMissing -> True
      _ -> False

resolveLookupName :: (MonadLore m) => Text -> m LookupNameResolution
resolveLookupName query = do
  matchingSymbols <- Set.toList <$> findMatchingSymbols (parseAndNormalizeName query)
  rootNames <- resolveRootNames matchingSymbols
  case rootNames of
    [] ->
      pure LookupNameMissing
    [rootName] ->
      pure (LookupNameResolved rootName)
    _ ->
      pure
        ( LookupNameAmbiguous
            AmbiguousQuery
              { queryText = query,
                matchedRoots = rootNames
              }
        )

resolveRootNames :: (MonadLore m) => [Symbol] -> m [Plugins.Name]
resolveRootNames symbols = do
  pathsToRoot <- mapM (resolvePathToRoot . (.name)) symbols
  let mergedPaths = mergePathsToRootOn renderName pathsToRoot
  pure $
    Set.toList $
      Set.fromList $
        map (NE.last . (.unPathToRoot)) mergedPaths
  where
    renderName name =
      case Plugins.nameModule_maybe name of
        Nothing ->
          "<no-module>." <> Plugins.getOccString name
        Just module_ ->
          Plugins.moduleNameString (Plugins.moduleName module_) <> "." <> Plugins.getOccString name

toMatchingInstances :: Instances -> [MatchingInstance]
toMatchingInstances instances_ =
  map MatchingClassInstance instances_.classInstances
    <> map MatchingFamilyInstance instances_.familyInstances

renderAmbiguousQueries :: [Text] -> [AmbiguousQuery] -> Text
renderAmbiguousQueries queries ambiguousQueries =
  T.intercalate "\n" $
    [ "One or more names are ambiguous and resolve to multiple roots. Qualify them with a module prefix.",
      ""
    ]
      <> concatMap renderAmbiguousQuery (zip [1 :: Int ..] ambiguousQueries)
      <> [ "",
           "Run the tool again with qualified names, for example: " <> renderExampleQualification queries ambiguousQueries
         ]

renderAmbiguousQuery :: (Int, AmbiguousQuery) -> [Text]
renderAmbiguousQuery (index, ambiguousQuery) =
  [ "  " <> T.pack (show index) <> ". " <> quoteText ambiguousQuery.queryText <> " matches:"
  ]
    <> map (("       - " <>) . renderNameWithModule) ambiguousQuery.matchedRoots

renderNameWithModule :: Plugins.Name -> Text
renderNameWithModule name =
  case Plugins.nameModule_maybe name of
    Nothing ->
      T.pack (Plugins.getOccString name)
    Just module_ ->
      T.pack (Plugins.moduleNameString (Plugins.moduleName module_))
        <> "."
        <> T.pack (Plugins.getOccString name)

renderExampleQualification :: [Text] -> [AmbiguousQuery] -> Text
renderExampleQualification queries ambiguousQueries =
  let qualified =
        map (qualifyName ambiguousQueries) queries
   in "[" <> T.intercalate ", " (map quoteText qualified) <> "]"
  where
    qualifyName allAmbiguous query =
      case [ambiguous | ambiguous <- allAmbiguous, ambiguous.queryText == query] of
        AmbiguousQuery {matchedRoots = rootName : _} : _ ->
          renderNameWithModule rootName
        _ ->
          query

quoteText :: Text -> Text
quoteText value =
  "\"" <> value <> "\""

maxRenderedMatchingInstances :: Int
maxRenderedMatchingInstances = 25
