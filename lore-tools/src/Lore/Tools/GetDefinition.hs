module Lore.Tools.GetDefinition
  ( DefinitionExpansion (..),
    RecursiveExpansionOptions (..),
    GetDefinitionRequest (..),
    GetDefinitionCoreResult,
    GetDefinitionCoreOutput (..),
    GetDefinitionCoreFailed (..),
    GetDefinitionCoreFailure (..),
    GetDefinitionCoreReady (..),
    OmittedDefinitions (..),
    ModuleOmittedSymbols (..),
    FilteredDefinitions (..),
    BuildDefinitionsStrategy,
    defaultDefinitionExpansion,
    getDefinitionHandlerWithStrategy,
    mkOmittedDefinitions,
  )
where

import Data.List (foldl', sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
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
import Lore.Tools.Internal.SymbolResolution
  ( ResolvedSymbolQuery (resolvedSymbol),
    SymbolsResolved (resolvedQueries),
    SymbolsUnresolved,
    resolveUniqueSymbolQueries,
  )
import Lore.Tools.Render.Doc (SourceFile)
import Lore.Tools.Result
  ( PageRequest,
    Paginated (..),
    PartialLoadWarning (..),
    ResultLimit (..),
    ToolRun (..),
    defaultPageRequest,
    loadedSessionPartialWarning,
    normalizePageRequest,
    withLoadedSession,
  )
import Numeric.Natural (Natural)

data GetDefinitionRequest = GetDefinitionRequest
  { getDefinitionRequestSymbols :: [Text],
    getDefinitionRequestPageRequest :: Maybe PageRequest,
    getDefinitionRequestExpansion :: Maybe DefinitionExpansion
  }

data DefinitionExpansion
  = NoExpansion
  | ExpandDirect
  | ExpandRecursive RecursiveExpansionOptions
  deriving stock (Eq, Show, Generic)

data RecursiveExpansionOptions = RecursiveExpansionOptions
  { recursiveExpansionMaxDepth :: Maybe Natural,
    recursiveExpansionMaxDefinitions :: ResultLimit
  }
  deriving stock (Eq, Show, Generic)

data GetDefinitionCoreFailure
  = GetDefinitionUnresolvedSymbols SymbolsUnresolved
  | GetDefinitionInternalError Text

data GetDefinitionCoreFailed = GetDefinitionCoreFailed
  { getDefinitionCoreFailure :: GetDefinitionCoreFailure,
    getDefinitionCoreFailedPartialLoadWarning :: Maybe PartialLoadWarning
  }

type GetDefinitionCoreResult = ToolRun GetDefinitionCoreOutput

data GetDefinitionCoreOutput
  = GetDefinitionCoreFailedResult GetDefinitionCoreFailed
  | GetDefinitionCoreReadyResult GetDefinitionCoreReady

data GetDefinitionCoreReady = GetDefinitionCoreReady
  { getDefinitionCoreSymbols :: [Text],
    getDefinitionCorePage :: Maybe (Paginated SourceFile),
    getDefinitionCoreOmitted :: OmittedDefinitions,
    getDefinitionCorePartialLoadWarning :: Maybe PartialLoadWarning
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
  PageRequest ->
  Set.Set GHC.Name ->
  [NamedDefinitionSource] ->
  m FilteredDefinitions

defaultDefinitionExpansion :: DefinitionExpansion
defaultDefinitionExpansion = NoExpansion

getDefinitionHandlerWithStrategy :: (MonadLore m) => GetDefinitionRequest -> BuildDefinitionsStrategy m -> m GetDefinitionCoreResult
getDefinitionHandlerWithStrategy GetDefinitionRequest {getDefinitionRequestSymbols, getDefinitionRequestPageRequest, getDefinitionRequestExpansion} buildDefinitions =
  withLoadedSession \session -> do
    let partialLoadWarning =
          loadedSessionPartialWarning session "Definition results may be incomplete."
    eiResolvedQueries <- resolveUniqueSymbolQueries getDefinitionRequestSymbols
    case eiResolvedQueries of
      Left unresolvedQueries ->
        pure $
          GetDefinitionCoreFailedResult
            GetDefinitionCoreFailed
              { getDefinitionCoreFailure = GetDefinitionUnresolvedSymbols unresolvedQueries,
                getDefinitionCoreFailedPartialLoadWarning = partialLoadWarning
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
        filteredDefinitions <- buildDefinitions resolvedPageRequest directlyRequestedSymbolNames definitionEntries
        pure $
          GetDefinitionCoreReadyResult
            GetDefinitionCoreReady
              { getDefinitionCoreSymbols = getDefinitionRequestSymbols,
                getDefinitionCorePage = filteredDefinitions.filteredDefinitionPage,
                getDefinitionCoreOmitted = filteredDefinitions.filteredOmittedDefinitions,
                getDefinitionCorePartialLoadWarning = partialLoadWarning
              }
  where
    resolvedPageRequest =
      normalizePageRequest (fromMaybe defaultPageRequest getDefinitionRequestPageRequest)
    resolvedExpansion =
      fromMaybe defaultDefinitionExpansion getDefinitionRequestExpansion

resolveSymbolDefinitions :: (MonadLore m) => DefinitionExpansion -> SymbolInfo -> m [NamedDefinitionSource]
resolveSymbolDefinitions expansion symbolInfo =
  case expansion of
    NoExpansion ->
      maybe [] (pure . NamedDefinitionSource symbolInfo.symbolName) <$> resolveDefinitionSourceNamed symbolInfo.symbolName
    ExpandDirect ->
      resolveDefinitionClosureSourcesNamed directExpansionMaxDepth symbolInfo.symbolName
    ExpandRecursive options -> do
      definitions <- resolveDefinitionClosureSourcesNamed (recursiveDepth options) symbolInfo.symbolName
      pure $
        case options.recursiveExpansionMaxDefinitions of
          Unlimited ->
            definitions
          Limit limit ->
            take (max 0 limit) definitions

directExpansionMaxDepth :: Int
directExpansionMaxDepth = 1

recursiveDepth :: RecursiveExpansionOptions -> Int
recursiveDepth options =
  case options.recursiveExpansionMaxDepth of
    Nothing ->
      maxBound
    Just depth ->
      max 0 (fromIntegral depth)

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
