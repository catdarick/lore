module Lore.Mcp.Tools.LookupInstances
  ( lookupInstancesTool,
  )
where

import qualified Data.Aeson as J
import Data.List (intercalate, nub)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC
import GHC.Generics (Generic)
import qualified GHC.Utils.Outputable as Outputable
import Lore
  ( LoadTargetsResult (..),
    LookupInstancesQuery (..),
    LookupInstancesResult (..),
    MatchingInstance (..),
    MonadLore,
    getLastLoadTargetsResult,
    lookupIntersectingRootInstances,
  )
import Lore.Mcp.Internal.Annotated
  ( Description,
    ExampleList,
    Field,
    FieldType (..),
    MinItems,
    WithMeta,
  )
import Lore.Mcp.Internal.Tool (SomeTool (..), ToolWithArgs (..))
import Lore.Mcp.Tools.Shared (appendPartialLoadWarning)

newtype LookupInstancesArgs (fieldType :: FieldType) = LookupInstancesArgs
  { names ::
      Field fieldType [Text]
        `WithMeta` '[ Description "Provide two or more symbol names. Module qualification (e.g., Some.Module.someFunction) is supported and can be used to resolve ambiguity or provide specific scope.",
                      ExampleList '["Show", "Int", "Some.Module.someFunction"],
                      MinItems 2
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
        description = Just "Find class or family instance declarations whose instance head mentions all queried symbols. Example: [\"Show\", \"Int\"] matches `instance Show Int`; [\"Int\", \"String\"] matches only instances where both types appear together. Queries are resolved to root declarations automatically.",
        handler = lookupInstancesHandler
      }

lookupInstancesHandler :: (MonadLore m) => LookupInstancesArgs 'ValueType -> m Text
lookupInstancesHandler LookupInstancesArgs {names} = do
  maybeLoadResult <- getLastLoadTargetsResult
  case maybeLoadResult of
    Nothing ->
      pure "Targets have not been loaded yet. Run reloadHomeModules first."
    Just loadResult -> do
      lookupResult <- lookupIntersectingRootInstances names
      pure (renderLookupInstancesResult loadResult lookupResult)

renderLookupInstancesResult :: LoadTargetsResult -> LookupInstancesResult -> Text
renderLookupInstancesResult loadResult lookupResult =
  appendPartialLoadWarning loadResult "Lookup results may be incomplete." renderedBody
  where
    renderedBody =
      T.intercalate "\n\n" $
        [renderQuerySection lookupResult.lookupInstancesQueries]
          <> [renderInstancesSection lookupResult.lookupInstancesResults]

renderQuerySection :: [LookupInstancesQuery] -> Text
renderQuerySection queries =
  T.intercalate "\n" $
    "Queries:"
      : map renderQuery queries

renderQuery :: LookupInstancesQuery -> Text
renderQuery query =
  "- "
    <> quoteText query.lookupInstancesQueryText
    <> ": "
    <> case renderMatchedNames query.lookupInstancesQueryMatches of
      [] -> "<no symbol matches>"
      renderedNames -> T.pack (intercalate ", " renderedNames)

renderMatchedNames :: [GHC.Name] -> [String]
renderMatchedNames =
  nub . map renderOutputable

renderInstancesSection :: [MatchingInstance] -> Text
renderInstancesSection instances_ =
  case instances_ of
    [] ->
      "Matching instances (instance head mentions all queried symbols):\n- <none>"
    _ ->
      T.intercalate "\n" $
        "Matching instances (instance head mentions all queried symbols):"
          : map (("- " <>) . renderMatchingInstance) instances_

renderMatchingInstance :: MatchingInstance -> Text
renderMatchingInstance = \case
  MatchingClassInstance _ classInstance ->
    renderOutputableText classInstance
  MatchingFamilyInstance _ familyInstance ->
    renderOutputableText familyInstance

renderOutputableText :: (Outputable.Outputable a) => a -> Text
renderOutputableText =
  T.pack . renderOutputable

renderOutputable :: (Outputable.Outputable a) => a -> String
renderOutputable =
  Outputable.showSDocUnsafe . Outputable.ppr

quoteText :: Text -> Text
quoteText value =
  "\"" <> value <> "\""
