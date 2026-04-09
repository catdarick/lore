module Lore.Mcp.Tools.GetDefinition
  ( getDefinitionTool,
  )
where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as J
import Data.Either (lefts)
import Data.Function (on)
import Data.List (nubBy, sortOn)
import Data.Maybe (fromMaybe)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC
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
        `WithMeta` '[ Description "Exact symbol names to resolve and render definitions for. Module qualification (e.g., Some.Module.someFunction) is supported and can be used to resolve ambiguity or provide specific scope.",
                      ExampleList '["HasIndex", "mkIndexed", "Some.Module.someFunction"],
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
      resolution <- resolveRequestedSymbols symbols
      case resolution of
        Left (missingSymbols, ambiguousQueries) ->
          pure (renderAmbiguityResult loadResult missingSymbols ambiguousQueries)
        Right resolvedSymbols -> do
          renderedDefinitions <- renderSymbolDefinitions resolvedRecursionDepth resolvedSymbols.resolvedSymbolInfos
          pure (renderDefinitionResult loadResult symbols resolvedSymbols.missingQueries renderedDefinitions)
  where
    resolvedRecursionDepth =
      max 0 (fromMaybe defaultRecursionDepth recursionDepth)

defaultRecursionDepth :: Int
defaultRecursionDepth = 0

data AmbiguousQuery = AmbiguousQuery
  { ambiguousQueryText :: Text,
    ambiguousQueryMatches :: [SymbolInfo]
  }

data ResolvedSymbols = ResolvedSymbols
  { missingQueries :: [Text],
    resolvedSymbolInfos :: [SymbolInfo]
  }

data ResolvedQuery
  = MissingQuery Text
  | ResolvedQuery SymbolInfo

resolveRequestedSymbols :: (MonadLore m) => [Text] -> m (Either ([Text], [AmbiguousQuery]) ResolvedSymbols)
resolveRequestedSymbols symbols = do
  resolvedQueries <- mapM resolveRequestedSymbol symbols
  pure $
    case lefts resolvedQueries of
      [] ->
        Right
          ResolvedSymbols
            { missingQueries = [queryText | Right (MissingQuery queryText) <- resolvedQueries],
              resolvedSymbolInfos = nubBy ((==) `on` symbolName) [symbolInfo | Right (ResolvedQuery symbolInfo) <- resolvedQueries]
            }
      ambiguousQueries ->
        Left
          ( [queryText | Right (MissingQuery queryText) <- resolvedQueries],
            ambiguousQueries
          )

resolveRequestedSymbol :: (MonadLore m) => Text -> m (Either AmbiguousQuery ResolvedQuery)
resolveRequestedSymbol symbol = do
  symbolInfos <- lookupRootSymbolInfo symbol
  pure $
    case symbolInfos of
      [] ->
        Right (MissingQuery symbol)
      [symbolInfo] ->
        Right (ResolvedQuery symbolInfo)
      ambiguousMatches ->
        Left
          AmbiguousQuery
            { ambiguousQueryText = symbol,
              ambiguousQueryMatches = ambiguousMatches
            }

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

renderDefinitionResult :: LoadTargetsResult -> [Text] -> [Text] -> Maybe Text -> Text
renderDefinitionResult loadResult symbols missingSymbols renderedDefinitions =
  appendPartialLoadWarning loadResult "Definition results may be incomplete." renderedBody
  where
    renderedBody =
      T.intercalate "\n\n" $
        missingSymbolsSection missingSymbols
          <> case renderedDefinitions of
            Nothing ->
              ["No definitions found for " <> quoteTexts symbols <> "."]
            Just definitionText ->
              [definitionText]

renderAmbiguityResult :: LoadTargetsResult -> [Text] -> [AmbiguousQuery] -> Text
renderAmbiguityResult loadResult missingSymbols ambiguousQueries =
  appendPartialLoadWarning loadResult "Definition results may be incomplete." renderedBody
  where
    ambiguousCount = length ambiguousQueries
    renderedBody =
      T.intercalate "\n\n" $
        missingSymbolsSection missingSymbols
          <> [ T.intercalate "\n" $
                 [ T.pack (show ambiguousCount)
                     <> " requested name"
                     <> pluralSuffix ambiguousCount
                     <> " "
                     <> ambiguousVerb ambiguousCount
                     <> " ambiguous. More qualification is required:"
                 ]
                   <> concatMap renderAmbiguousQuery (zip [1 :: Int ..] ambiguousQueries)
                   <> ["", "Run the tool again with a qualified symbol name, for example: " <> renderExampleQualification ambiguousQueries]
             ]

renderAmbiguousQuery :: (Int, AmbiguousQuery) -> [Text]
renderAmbiguousQuery (index, ambiguousQuery) =
  ["  " <> T.pack (show index) <> ". " <> ambiguousQuery.ambiguousQueryText <> " is defined in:"]
    <> map (("       - " <>) . renderModuleName) (ambiguousDefinitionModules ambiguousQuery.ambiguousQueryMatches)

ambiguousDefinitionModules :: [SymbolInfo] -> [GHC.Module]
ambiguousDefinitionModules =
  map head
    . groupModules
    . sortOn renderModuleName
    . map definedIn
  where
    groupModules [] = []
    groupModules (module_ : modules) =
      let (matchingModules, rest) = span ((== renderModuleName module_) . renderModuleName) modules
       in (module_ : matchingModules) : groupModules rest

renderModuleName :: GHC.Module -> Text
renderModuleName =
  T.pack . GHC.moduleNameString . GHC.moduleName

renderExampleQualification :: [AmbiguousQuery] -> Text
renderExampleQualification ambiguousQueries =
  case ambiguousQueries of
    ambiguousQuery : _ ->
      case ambiguousDefinitionModules ambiguousQuery.ambiguousQueryMatches of
        module_ : _ ->
          renderModuleName module_ <> "." <> queryOccName ambiguousQuery.ambiguousQueryText
        [] ->
          ambiguousQuery.ambiguousQueryText
    [] ->
      "<module>.<symbol>"

queryOccName :: Text -> Text
queryOccName queryText =
  case reverse (T.splitOn "." queryText) of
    occName : _ | not (T.null occName) -> occName
    _ -> queryText

pluralSuffix :: Int -> Text
pluralSuffix count
  | count == 1 = ""
  | otherwise = "s"

ambiguousVerb :: Int -> Text
ambiguousVerb count
  | count == 1 = "is"
  | otherwise = "are"

missingSymbolsSection :: [Text] -> [Text]
missingSymbolsSection [] = []
missingSymbolsSection missingSymbols =
  [ T.intercalate "\n" $
      [ T.pack (show (length missingSymbols))
          <> " requested name"
          <> pluralSuffix (length missingSymbols)
          <> " "
          <> missingVerb (length missingSymbols)
          <> " not found:"
      ]
        <> map (("  - " <>) . quoteText) missingSymbols
  ]

missingVerb :: Int -> Text
missingVerb count
  | count == 1 = "was"
  | otherwise = "were"

quoteTexts :: [Text] -> Text
quoteTexts values =
  "[" <> T.intercalate ", " (map (\value -> "\"" <> value <> "\"") values) <> "]"

quoteText :: Text -> Text
quoteText value =
  "\"" <> value <> "\""
