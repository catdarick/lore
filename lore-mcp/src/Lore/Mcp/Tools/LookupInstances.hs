module Lore.Mcp.Tools.LookupInstances
  ( lookupInstancesTool,
  )
where

import qualified Data.Aeson as J
import Data.List (foldl')
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
    listIntersectingInstances,
    lookupLastLoadHomeModulesResult,
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
import Lore.Mcp.Tools.Shared.SymbolResolution (ResolvedSymbolQuery (resolvedRootName), withResolvedSymbols)

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
  maybeLoadResult <- lookupLastLoadHomeModulesResult
  case maybeLoadResult of
    Nothing ->
      pure "Home modules have not been loaded yet. Run reloadHomeModules first."
    Just loadResult -> do
      renderedBody <-
        withResolvedSymbols names \resolvedQueries -> do
          let resolvedRootNames = dedupeNamesBy renderName (map (.resolvedRootName) resolvedQueries)
          intersectingInstances <- listIntersectingInstances resolvedRootNames
          pure (renderLookupInstancesResult resolvedSkip (toMatchingInstances intersectingInstances))
      pure (renderText (renderedBody |> mkPartialWarning loadResult))
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
