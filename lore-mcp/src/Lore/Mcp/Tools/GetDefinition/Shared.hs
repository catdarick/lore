{-# LANGUAGE DeriveAnyClass #-}

module Lore.Mcp.Tools.GetDefinition.Shared
  ( DefinitionExpansion (..),
    GetDefinitionArgs (..),
    GetDefinitionResult,
    GetDefinitionOutput (..),
    GetDefinitionFailed (..),
    GetDefinitionFailure (..),
    GetDefinitionReady (..),
    OmittedDefinitions (..),
    ModuleOmittedSymbols (..),
    FilteredDefinitions (..),
    BuildDefinitionsStrategy,
    defaultDefinitionExpansion,
    maxRenderedDefinitionResults,
    getDefinitionHandlerWithStrategy,
    mkOmittedDefinitions,
  )
where

import qualified Data.Aeson as J
import Data.List (foldl', sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.OpenApi (ToSchema)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified GHC.Plugins as GHC
import Lore
  ( MonadLore,
    NamedDefinitionSource (..),
    Symbol (..),
    SymbolInfo (..),
    lookupSymbolInfo,
    resolveDefinitionClosureSourcesNamed,
    resolveDefinitionSourceNamed,
  )
import Lore.Mcp.Internal.Annotated (Description, Example, ExampleList, Field, FieldType (..), MinItems, WithMeta)
import Lore.Mcp.Internal.LoreDoc
  ( LoreDoc,
    SourceFile,
    ToLoreDoc (toLoreDoc),
    paragraph,
    sourceFile,
  )
import Lore.Mcp.Tools.Shared
  ( Paginated (..),
    PartialLoadWarning (..),
    ToolRun (..),
    loadedSessionPartialWarning,
    withLoadedSession,
    withPartialLoadWarning,
  )
import Lore.Mcp.Tools.Shared.SymbolResolution
  ( ResolvedSymbolQuery (resolvedSymbol),
    SymbolsResolved (resolvedQueries),
    SymbolsUnresolved,
    resolveUniqueSymbolQueries,
    unresolvedSymbolQueriesMessage,
  )

data GetDefinitionArgs (fieldType :: FieldType) = GetDefinitionArgs
  { symbols ::
      Field fieldType [Text]
        `WithMeta` '[ Description "Exact symbol names to resolve and render definitions for. Module qualification (e.g., Some.Module.someFunction) is supported and can be used to resolve ambiguity or provide specific scope.",
                      ExampleList '["HasIndex", "mkIndexed", "Some.Module.someFunction"],
                      MinItems 1
                    ],
    skip ::
      Maybe (Field fieldType Int)
        `WithMeta` '[ Description "Used for pagination. Number of initial results to skip. Use it only if a previous result was truncated and you want to see the next page of results.",
                      Example 30
                    ],
    expansion ::
      Field fieldType (Maybe DefinitionExpansion)
        `WithMeta` '[ Description "How much related definitions to return. Use \"None\" to return only the requested symbol's definitions. Use \"Direct\" to also include definitions of symbols referenced directly by the requested definitions (maxDepth=1). Use \"Recursive\" to include direct dependencies and their dependencies (maxDepth=2, maxSymbols=30)."
                    ]
  }
  deriving stock (Generic)

instance J.FromJSON (GetDefinitionArgs 'ValueType)

instance ToSchema (GetDefinitionArgs 'MetadataType)

data DefinitionExpansion
  = None
  | Direct
  | Recursive
  deriving stock (Eq, Show, Generic)
  deriving anyclass (J.ToJSON, J.FromJSON, ToSchema)

type GetDefinitionResult = ToolRun GetDefinitionOutput

data GetDefinitionOutput
  = GetDefinitionFailedResult GetDefinitionFailed
  | GetDefinitionReadyResult GetDefinitionReady

data GetDefinitionFailed = GetDefinitionFailed
  { getDefinitionFailure :: GetDefinitionFailure,
    getDefinitionFailedPartialLoadWarning :: Maybe PartialLoadWarning
  }

data GetDefinitionFailure
  = GetDefinitionUnresolvedSymbols SymbolsUnresolved
  | GetDefinitionInternalError Text

data GetDefinitionReady = GetDefinitionReady
  { getDefinitionSymbols :: [Text],
    getDefinitionPage :: Maybe (Paginated SourceFile),
    getDefinitionOmitted :: OmittedDefinitions,
    getDefinitionPartialLoadWarning :: Maybe PartialLoadWarning,
    getDefinitionRenderNotifyKnowledgeResetHint :: Bool
  }

data OmittedDefinitions = OmittedDefinitions
  { omittedDefinitionSymbolsByModule :: [ModuleOmittedSymbols],
    omittedDefinitionCount :: Int
  }

data ModuleOmittedSymbols = ModuleOmittedSymbols
  { moduleOmittedSymbolsModuleName :: Text,
    moduleOmittedSymbolsSymbolNames :: [Text]
  }

data FilteredDefinitions = FilteredDefinitions
  { filteredDefinitionPage :: Maybe (Paginated SourceFile),
    filteredOmittedDefinitions :: OmittedDefinitions
  }

type BuildDefinitionsStrategy m =
  Int ->
  Set.Set GHC.Name ->
  [NamedDefinitionSource] ->
  m FilteredDefinitions

getDefinitionHandlerWithStrategy :: (MonadLore m) => Bool -> GetDefinitionArgs 'ValueType -> BuildDefinitionsStrategy m -> m GetDefinitionResult
getDefinitionHandlerWithStrategy shouldRenderNotifyKnowledgeResetHint GetDefinitionArgs {symbols, skip, expansion} buildDefinitions = do
  withLoadedSession \session -> do
    let partialLoadWarning =
          loadedSessionPartialWarning session "Definition results may be incomplete."
    eiResolvedQueries <- resolveUniqueSymbolQueries symbols
    case eiResolvedQueries of
      Left unresolvedQueries ->
        pure $
          GetDefinitionFailedResult
            GetDefinitionFailed
              { getDefinitionFailure = GetDefinitionUnresolvedSymbols unresolvedQueries,
                getDefinitionFailedPartialLoadWarning = partialLoadWarning
              }
      Right resolved -> do
        resolvedSymbolInfos <-
          catMaybes
            <$> mapM
              (\resolvedQuery -> lookupSymbolInfo resolvedQuery.resolvedSymbol.name)
              resolved.resolvedQueries
        let directlyRequestedSymbolNames =
              Set.fromList (map (.symbolName) resolvedSymbolInfos)
        definitionEntries <- concat <$> mapM (resolveSymbolDefinitions resolvedExpansion) resolvedSymbolInfos
        filteredDefinitions <- buildDefinitions resolvedSkip directlyRequestedSymbolNames definitionEntries
        pure $
          GetDefinitionReadyResult
            GetDefinitionReady
              { getDefinitionSymbols = symbols,
                getDefinitionPage = filteredDefinitions.filteredDefinitionPage,
                getDefinitionOmitted = filteredDefinitions.filteredOmittedDefinitions,
                getDefinitionPartialLoadWarning = partialLoadWarning,
                getDefinitionRenderNotifyKnowledgeResetHint = shouldRenderNotifyKnowledgeResetHint
              }
  where
    resolvedSkip =
      max 0 (fromMaybe 0 skip)
    resolvedExpansion =
      fromMaybe defaultDefinitionExpansion expansion

defaultDefinitionExpansion :: DefinitionExpansion
defaultDefinitionExpansion = None

resolveSymbolDefinitions :: (MonadLore m) => DefinitionExpansion -> SymbolInfo -> m [NamedDefinitionSource]
resolveSymbolDefinitions expansion symbolInfo =
  case expansion of
    None ->
      maybe [] (pure . NamedDefinitionSource symbolInfo.symbolName) <$> resolveDefinitionSourceNamed symbolInfo.symbolName
    Direct ->
      resolveDefinitionClosureSourcesNamed directExpansionMaxDepth symbolInfo.symbolName
    Recursive ->
      take maxRenderedDefinitionResults <$> resolveDefinitionClosureSourcesNamed recursiveExpansionMaxDepth symbolInfo.symbolName

directExpansionMaxDepth :: Int
directExpansionMaxDepth = 1

recursiveExpansionMaxDepth :: Int
recursiveExpansionMaxDepth = 2

mkOmittedDefinitions :: [GHC.Name] -> OmittedDefinitions
mkOmittedDefinitions names =
  OmittedDefinitions
    { omittedDefinitionSymbolsByModule =
        sortOn (.moduleOmittedSymbolsModuleName) (map toModuleOmittedSymbols (Map.toList grouped)),
      omittedDefinitionCount = length names
    }
  where
    grouped =
      foldl' collectDefinition Map.empty names

    collectDefinition groupedByModule name =
      Map.insertWith (<>) (definitionModuleName name) [definitionSymbolName name] groupedByModule

    toModuleOmittedSymbols (moduleName, symbolNames) =
      ModuleOmittedSymbols
        { moduleOmittedSymbolsModuleName = moduleName,
          moduleOmittedSymbolsSymbolNames = dedupeTexts symbolNames
        }

definitionModuleName :: GHC.Name -> Text
definitionModuleName name =
  case GHC.nameModule_maybe name of
    Just module_ -> T.pack (GHC.moduleNameString (GHC.moduleName module_))
    Nothing -> "<unknown module>"

definitionSymbolName :: GHC.Name -> Text
definitionSymbolName =
  T.pack . GHC.getOccString

dedupeTexts :: [Text] -> [Text]
dedupeTexts =
  reverse . snd . foldl' dedupeText (Set.empty, [])
  where
    dedupeText (seenTexts, deduped) value
      | Set.member value seenTexts =
          (seenTexts, deduped)
      | otherwise =
          (Set.insert value seenTexts, value : deduped)

quoteTexts :: [Text] -> Text
quoteTexts values =
  "[" <> T.intercalate ", " (map (\value -> "\"" <> value <> "\"") values) <> "]"

omittedDefinitionsSectionHeader :: Text
omittedDefinitionsSectionHeader =
  "The following definitions are unchanged and were omitted, you already have them in your context:"

notifyKnowledgeResetHint :: Text
notifyKnowledgeResetHint =
  "IF AND ONLY IF your active conversation history was just wiped, or you have suffered a total memory reset and literally cannot see these definitions in your previous turns, you should execute the `notifyKnowledgeReset` tool to resync the server cache."

renderOmittedDefinitionsLines :: OmittedDefinitions -> [Text]
renderOmittedDefinitionsLines omittedDefinitions =
  map ("  - " <>) (map renderModuleLine omittedDefinitions.omittedDefinitionSymbolsByModule)
  where
    renderModuleLine moduleSymbols =
      moduleSymbols.moduleOmittedSymbolsModuleName
        <> ": "
        <> T.intercalate ", " (take maxRenderedOmittedSymbolsPerModule moduleSymbols.moduleOmittedSymbolsSymbolNames)
        <> overflowSuffix moduleSymbols

    overflowSuffix moduleSymbols =
      let hiddenCount = length moduleSymbols.moduleOmittedSymbolsSymbolNames - maxRenderedOmittedSymbolsPerModule
       in if hiddenCount > 0
            then " and " <> T.pack (show hiddenCount) <> " more"
            else ""

instance ToLoreDoc GetDefinitionOutput where
  toLoreDoc = \case
    GetDefinitionFailedResult failed ->
      toLoreDoc failed
    GetDefinitionReadyResult ready ->
      renderReady ready

instance ToLoreDoc GetDefinitionFailed where
  toLoreDoc failed =
    withPartialLoadWarning failed.getDefinitionFailedPartialLoadWarning $
      paragraph (renderGetDefinitionFailure failed.getDefinitionFailure)

instance ToLoreDoc GetDefinitionFailure where
  toLoreDoc =
    paragraph . renderGetDefinitionFailure

renderGetDefinitionFailure :: GetDefinitionFailure -> Text
renderGetDefinitionFailure = \case
  GetDefinitionUnresolvedSymbols unresolvedQueries ->
    unresolvedSymbolQueriesMessage unresolvedQueries
  GetDefinitionInternalError message ->
    message

renderReady :: GetDefinitionReady -> LoreDoc
renderReady ready =
  case ready.getDefinitionPage of
    Nothing
      | ready.getDefinitionOmitted.omittedDefinitionCount > 0 ->
          withPartialLoadWarning ready.getDefinitionPartialLoadWarning $
            paragraph $
              T.intercalate
                "\n"
                (omittedLines <> notifyHintLines)
      | otherwise ->
          withPartialLoadWarning ready.getDefinitionPartialLoadWarning $
            paragraph ("No definitions found for " <> quoteTexts ready.getDefinitionSymbols <> ".")
    Just page ->
      mconcat
        [ mconcat (map sourceFile page.paginatedItems),
          footerSection page,
          partialWarningSection
        ]
  where
    footerSection page =
      case footerLines page of
        [] ->
          mempty
        lines_ ->
          paragraph (T.intercalate "\n" lines_)

    footerLines page =
      paginationOverflowLine page
        <> omittedLines
        <> notifyHintLines

    paginationOverflowLine page
      | remainingItems page > 0 =
          [ "And "
              <> T.pack (show (remainingItems page))
              <> " more definition results (set skip to "
              <> T.pack (show (nextSkip page))
              <> " to get the next page if required)."
          ]
      | otherwise =
          []

    remainingItems page =
      max
        0
        ( page.paginatedTotalItems
            - page.paginatedSkippedItems
            - page.paginatedConsumedItems
        )

    nextSkip page =
      page.paginatedSkippedItems + page.paginatedConsumedItems

    omittedLines
      | ready.getDefinitionOmitted.omittedDefinitionCount <= 0 =
          []
      | otherwise =
          [omittedDefinitionsSectionHeader] <> renderOmittedDefinitionsLines ready.getDefinitionOmitted

    notifyHintLines
      | ready.getDefinitionRenderNotifyKnowledgeResetHint
          && ready.getDefinitionOmitted.omittedDefinitionCount > 0 =
          [notifyKnowledgeResetHint]
      | otherwise =
          []

    partialWarningSection =
      maybe mempty toLoreDoc ready.getDefinitionPartialLoadWarning

maxRenderedDefinitionResults :: Int
maxRenderedDefinitionResults = 30

maxRenderedOmittedSymbolsPerModule :: Int
maxRenderedOmittedSymbolsPerModule = 10
