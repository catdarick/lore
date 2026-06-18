{-# LANGUAGE DeriveAnyClass #-}

module Lore.Mcp.Tools.GetDefinitions.Shared
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
    maxRenderedDefinitionResults,
    mkOmittedDefinitions,
    toGetDefinitionRequest,
    toGetDefinitionResult,
  )
where

import qualified Data.Aeson as J
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Lore.Mcp.Internal.Annotated (Description, Example, ExampleList, Field, FieldType (..), Maximum, MinItems, Minimum, WithMeta)
import Lore.Tools.GetDefinition
  ( BuildDefinitionsStrategy,
    FilteredDefinitions (..),
    GetDefinitionCoreFailed (..),
    GetDefinitionCoreFailure,
    GetDefinitionCoreOutput (..),
    GetDefinitionCoreReady (..),
    GetDefinitionRequest (..),
    ModuleOmittedSymbols (..),
    OmittedDefinitions (..),
    RecursiveExpansionOptions (..),
    mkOmittedDefinitions,
  )
import qualified Lore.Tools.GetDefinition as Core
import Lore.Tools.Internal.SymbolResolution
  ( SymbolsUnresolved,
    unresolvedSymbolQueriesMessage,
  )
import Lore.Tools.Pagination (ToolPolicy (..), limitToIntWithDefault, mcpDefaultToolPolicy)
import Lore.Tools.Render.Doc
  ( LoreDoc,
    SourceFile,
    ToLoreDoc (toLoreDoc),
    paragraph,
    sourceFile,
  )
import Lore.Tools.Result
  ( PageRequest (..),
    Paginated (..),
    PartialLoadWarning (..),
    ResultLimit (..),
    ToolRun (..),
    withPartialLoadWarning,
  )

-- Schema-annotated args stay in MCP.
data GetDefinitionArgs (fieldType :: FieldType) = GetDefinitionArgs
  { symbols ::
      Field fieldType [Text]
        `WithMeta` '[ Description "Exact symbol names to resolve and render definitions for. Module qualification (e.g., Blog.Article.publishArticle) is supported and can be used to resolve ambiguity or provide specific scope.",
                      ExampleList '["Article", "publishArticle", "Blog.Article.publishArticle"],
                      MinItems 1
                    ],
    skip ::
      Field fieldType (Maybe Int)
        `WithMeta` '[ Description "Used for pagination. Number of initial results to skip. Use it only if a previous result was truncated and you want to see the next page of results.",
                      Example 30,
                      Minimum 0,
                      Maximum 9999
                    ],
    expansion ::
      Maybe (Field fieldType DefinitionExpansion)
        `WithMeta` '[ Description "How much related definitions to return. Use \"None\" to return only the requested symbol's definitions. Use \"Direct\" to also include definitions of symbols referenced directly by the requested definitions. Use \"Recursive\" to include direct dependencies and their dependencies."
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

toGetDefinitionRequest :: GetDefinitionArgs 'ValueType -> GetDefinitionRequest
toGetDefinitionRequest GetDefinitionArgs {symbols, skip, expansion} =
  GetDefinitionRequest
    { getDefinitionRequestSymbols = symbols,
      getDefinitionRequestPageRequest =
        Just
          PageRequest
            { pageOffset = max 0 (maybe 0 id skip),
              pageLimit = Unlimited
            },
      getDefinitionRequestExpansion = fmap toCoreDefinitionExpansion expansion
    }

toCoreDefinitionExpansion :: DefinitionExpansion -> Core.DefinitionExpansion
toCoreDefinitionExpansion = \case
  None ->
    Core.NoExpansion
  Direct ->
    Core.ExpandRecursive
      RecursiveExpansionOptions
        { recursiveExpansionMaxDepth = Just 1,
          recursiveExpansionMaxDefinitions = Limit maxRenderedDefinitionResults
        }
  Recursive ->
    Core.ExpandRecursive
      RecursiveExpansionOptions
        { recursiveExpansionMaxDepth = Just 2,
          recursiveExpansionMaxDefinitions = Limit maxRenderedDefinitionResults
        }

maxRenderedDefinitionResults :: Int
maxRenderedDefinitionResults =
  limitToIntWithDefault 30 (definitionLimit mcpDefaultToolPolicy)

toGetDefinitionResult :: Bool -> ToolRun GetDefinitionCoreOutput -> GetDefinitionResult
toGetDefinitionResult shouldRenderNotifyKnowledgeResetHint = \case
  ToolRunBlocked blocked ->
    ToolRunBlocked blocked
  ToolRunReady coreOutput ->
    ToolRunReady (toGetDefinitionOutput shouldRenderNotifyKnowledgeResetHint coreOutput)

toGetDefinitionOutput :: Bool -> GetDefinitionCoreOutput -> GetDefinitionOutput
toGetDefinitionOutput shouldRenderNotifyKnowledgeResetHint = \case
  GetDefinitionCoreFailedResult failure ->
    GetDefinitionFailedResult (toGetDefinitionFailed failure)
  GetDefinitionCoreReadyResult ready ->
    GetDefinitionReadyResult (toGetDefinitionReady shouldRenderNotifyKnowledgeResetHint ready)

toGetDefinitionFailed :: GetDefinitionCoreFailed -> GetDefinitionFailed
toGetDefinitionFailed GetDefinitionCoreFailed {getDefinitionCoreFailure, getDefinitionCoreFailedPartialLoadWarning} =
  GetDefinitionFailed
    { getDefinitionFailure = toGetDefinitionFailure getDefinitionCoreFailure,
      getDefinitionFailedPartialLoadWarning = getDefinitionCoreFailedPartialLoadWarning
    }

toGetDefinitionFailure :: GetDefinitionCoreFailure -> GetDefinitionFailure
toGetDefinitionFailure = \case
  Core.GetDefinitionUnresolvedSymbols unresolvedQueries ->
    GetDefinitionUnresolvedSymbols unresolvedQueries
  Core.GetDefinitionInternalError message ->
    GetDefinitionInternalError message

toGetDefinitionReady :: Bool -> GetDefinitionCoreReady -> GetDefinitionReady
toGetDefinitionReady shouldRenderNotifyKnowledgeResetHint ready =
  GetDefinitionReady
    { getDefinitionSymbols = ready.getDefinitionCoreSymbols,
      getDefinitionPage = ready.getDefinitionCorePage,
      getDefinitionOmitted = ready.getDefinitionCoreOmitted,
      getDefinitionPartialLoadWarning = ready.getDefinitionCorePartialLoadWarning,
      getDefinitionRenderNotifyKnowledgeResetHint = shouldRenderNotifyKnowledgeResetHint
    }

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

quoteTexts :: [Text] -> Text
quoteTexts values =
  "[" <> T.intercalate ", " (map (\value -> "\"" <> value <> "\"") values) <> "]"

maxRenderedOmittedSymbolsPerModule :: Int
maxRenderedOmittedSymbolsPerModule = 10
