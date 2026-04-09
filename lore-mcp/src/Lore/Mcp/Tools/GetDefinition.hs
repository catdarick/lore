module Lore.Mcp.Tools.GetDefinition
  ( getDefinitionTool,
  )
where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as J
import Data.Function (on)
import Data.List (nubBy)
import Data.Maybe (fromMaybe)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Lore
  ( DefinitionSlice,
    LoadTargetsResult (..),
    MonadLore,
    SymbolInfo (..),
    getLastLoadTargetsResult,
    lookupRootSymbolInfo,
    renderDefinitionModulesText,
    resolveDefinitionClosure,
    resolveDefinitionSlice,
  )
import Lore.Mcp.Internal.Annotated (Description, Example, ExampleList, Field, FieldType (..), Maximum, MinItems, Minimum, WithMeta)
import Lore.Mcp.Internal.Tool (SomeTool (..), ToolWithArgs (..))
import Lore.Mcp.Tools.Shared (appendPartialLoadWarning)

data GetDefinitionArgs (fieldType :: FieldType) = GetDefinitionArgs
  { symbols ::
      Field fieldType [Text]
        `WithMeta` '[ Description "Exact symbol names to resolve and render definitions for. Queries are resolved to root declarations automatically, then merged before rendering.",
                      ExampleList '["HasIndex", "mkIndexed"],
                      MinItems 1
                    ],
    recursionDepth ::
      Field fieldType (Maybe Int)
        `WithMeta` '[ Description "Maximum recursive definition depth. Defaults to 0. If greater than 0, definitions will be resolved recursively to the specified depth, where 1 means only directly referenced definitions will be included, 2 means definitions directly referenced by those definitions will also be included, and so on.",
                      Example 2,
                      Minimum 0,
                      Maximum 20
                    ]
  }
  deriving stock (Generic)

instance J.FromJSON (GetDefinitionArgs 'ValueType)

instance ToSchema (GetDefinitionArgs 'MetadataType)

getDefinitionTool :: (MonadLore m) => SomeTool m
getDefinitionTool =
  SomeToolWithArgs
    ToolWithArgs
      { name = "getDefinition",
        description = Just "Get the definition of a symbol or list of symbols. Set recursionDepth if you want to include definitions of referenced symbols as well.",
        handler = getDefinitionHandler
      }

getDefinitionHandler :: (MonadLore m) => GetDefinitionArgs 'ValueType -> m Text
getDefinitionHandler GetDefinitionArgs {symbols, recursionDepth} = do
  maybeLoadResult <- getLastLoadTargetsResult
  case maybeLoadResult of
    Nothing ->
      pure "Targets have not been loaded yet. Run loadTargets first."
    Just loadResult -> do
      symbolInfos <- lookupRootSymbolInfos symbols
      renderedDefinitions <- renderSymbolDefinitions resolvedRecursionDepth symbolInfos
      pure (renderDefinitionResult loadResult symbols renderedDefinitions)
  where
    resolvedRecursionDepth =
      max 0 (fromMaybe defaultRecursionDepth recursionDepth)

defaultRecursionDepth :: Int
defaultRecursionDepth = 0

lookupRootSymbolInfos :: (MonadLore m) => [Text] -> m [SymbolInfo]
lookupRootSymbolInfos symbols = do
  symbolInfos <- concat <$> mapM lookupRootSymbolInfo symbols
  pure (nubBy ((==) `on` symbolName) symbolInfos)

renderSymbolDefinitions :: (MonadLore m) => Int -> [SymbolInfo] -> m (Maybe Text)
renderSymbolDefinitions recursionDepth symbolInfos = do
  definitionSlices <- concat <$> mapM (resolveSymbolDefinitions recursionDepth) symbolInfos
  if null definitionSlices
    then pure Nothing
    else Just <$> liftIO (renderDefinitionModulesText definitionSlices)

resolveSymbolDefinitions :: (MonadLore m) => Int -> SymbolInfo -> m [DefinitionSlice]
resolveSymbolDefinitions recursionDepth symbolInfo
  | recursionDepth == 0 =
      maybe [] pure <$> resolveDefinitionSlice symbolInfo.symbolName
  | otherwise =
      resolveDefinitionClosure recursionDepth symbolInfo.symbolName

renderDefinitionResult :: LoadTargetsResult -> [Text] -> Maybe Text -> Text
renderDefinitionResult loadResult symbols renderedDefinitions =
  appendPartialLoadWarning loadResult "Definition results may be incomplete." renderedBody
  where
    renderedBody =
      case renderedDefinitions of
        Nothing ->
          "No definitions found for " <> quoteTexts symbols <> "."
        Just definitionText ->
          definitionText

quoteTexts :: [Text] -> Text
quoteTexts values =
  "[" <> T.intercalate ", " (map (\value -> "\"" <> value <> "\"") values) <> "]"
