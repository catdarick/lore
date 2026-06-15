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

import qualified Data.List as List
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Ord (Down (..))
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified GHC.Plugins as GHC
import Lore
  ( DefinitionId,
    DefinitionSource (..),
    MonadLore,
    NamedDefinitionSource (..),
    Symbol (..),
    SymbolInfo (..),
    definitionSourceModule,
    lookupSymbolInfo,
    resolveDefinitionClosureSourcesNamed,
    resolveDefinitionSourceNamed,
  )
import Lore.Tools.Internal.DefinitionSourceRendering (definitionSourceSortKey)
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
        resolvedEntries <- concat <$> mapM (resolveSymbolDefinitions resolvedExpansion) resolvedSymbolInfos
        let orderedEntries =
              orderDefinitionSources resolvedEntries
            limitedEntries =
              applyExpansionDefinitionLimit resolvedExpansion orderedEntries
        filteredDefinitions <- buildDefinitions resolvedPageRequest directlyRequestedSymbolNames limitedEntries
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
      maybe [] (pure . namedRootDefinitionSource symbolInfo.symbolName) <$> resolveDefinitionSourceNamed symbolInfo.symbolName
    ExpandDirect ->
      resolveDefinitionClosureSourcesNamed directExpansionMaxDepth symbolInfo.symbolName
    ExpandRecursive options ->
      resolveDefinitionClosureSourcesNamed (recursiveDepth options) symbolInfo.symbolName

namedRootDefinitionSource :: GHC.Name -> DefinitionSource -> NamedDefinitionSource
namedRootDefinitionSource definitionName definitionSource =
  NamedDefinitionSource
    { definitionName,
      definitionDependencyDepth = 0,
      definitionSource
    }

applyExpansionDefinitionLimit :: DefinitionExpansion -> [NamedDefinitionSource] -> [NamedDefinitionSource]
applyExpansionDefinitionLimit expansion definitionEntries =
  case expansion of
    ExpandRecursive options ->
      case options.recursiveExpansionMaxDefinitions of
        Unlimited ->
          definitionEntries
        Limit limit ->
          take (max 0 limit) definitionEntries
    NoExpansion ->
      definitionEntries
    ExpandDirect ->
      definitionEntries

orderDefinitionSources :: [NamedDefinitionSource] -> [NamedDefinitionSource]
orderDefinitionSources definitionEntries =
  sortOn rankedSortKey dedupedEntries
  where
    dedupedEntries =
      dedupeDefinitionSourcesAtMinimumDepth definitionEntries
    moduleScores =
      definitionModuleScores dedupedEntries
    rankedSortKey definitionEntry =
      ( Down (Map.findWithDefault 0 (definitionSourceModule definitionEntry.definitionSource) moduleScores),
        definitionSourceSortKey definitionEntry
      )

dedupeDefinitionSourcesAtMinimumDepth :: [NamedDefinitionSource] -> [NamedDefinitionSource]
dedupeDefinitionSourcesAtMinimumDepth =
  Map.elems . List.foldl' collectDefinition (Map.empty :: Map.Map DefinitionId NamedDefinitionSource)
  where
    collectDefinition definitionsById definitionEntry =
      Map.insertWith preferShallower definitionId definitionEntry definitionsById
      where
        definitionId =
          definitionEntry.definitionSource.definitionSourceId

    preferShallower new old
      | new.definitionDependencyDepth < old.definitionDependencyDepth =
          new
      | otherwise =
          old

definitionModuleScores :: [NamedDefinitionSource] -> Map.Map GHC.Module Integer
definitionModuleScores =
  List.foldl' collectScore Map.empty
  where
    collectScore scoresByModule definitionEntry =
      Map.insertWith
        (+)
        (definitionSourceModule definitionEntry.definitionSource)
        (definitionDepthScore definitionEntry.definitionDependencyDepth)
        scoresByModule

definitionDepthScore :: Int -> Integer
definitionDepthScore depth =
  2 ^ max 0 (30 - 5 * max 0 depth)

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
      List.foldl' collectDefinition Map.empty names

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
  reverse . snd . List.foldl' dedupeText (Set.empty, [])
  where
    dedupeText (seenTexts, deduped) value
      | Set.member value seenTexts =
          (seenTexts, deduped)
      | otherwise =
          (Set.insert value seenTexts, value : deduped)
