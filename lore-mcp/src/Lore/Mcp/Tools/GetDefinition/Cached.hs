module Lore.Mcp.Tools.GetDefinition.Cached
  ( cachedGetDefinitionTool,
  )
where

import Control.Concurrent.MVar (modifyMVar)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as J
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import Data.OpenApi (ToSchema)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Fingerprint (Fingerprint (..), fingerprintString)
import GHC.Generics (Generic)
import qualified GHC.Plugins as GHC
import Lore (DeclarationSpans (..), DefinitionId, DefinitionSource (..), MonadLore, NamedDefinitionSource (..))
import Lore.Mcp.Internal.Annotated (Description, Example, ExampleList, Field, FieldType (..), Maximum, MinItems, Minimum, WithMeta)
import Lore.Mcp.Internal.Tool (SomeTool (..), ToolWithArgs (..))
import Lore.Mcp.Monad (MonadLoreMcp (..), sentDefinitionHashes)
import Lore.Mcp.Tools.GetDefinition.Shared
  ( CommonGetDefinitionArgs (..),
    FilteredDefinitions (..),
    PaginatedDefinitionSources (..),
    defaultRecursionDepth,
    getDefinitionHandlerWithStrategy,
    maxRenderedDefinitionResults,
    paginateDefinitionSources,
    renderPaginatedDefinitionSources,
  )
import qualified Lore.Mcp.Tools.Shared as Shared
import Text.Printf (printf)

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

cachedGetDefinitionTool :: (MonadLoreMcp m) => SomeTool m
cachedGetDefinitionTool =
  SomeToolWithArgs
    ToolWithArgs
      { name = "getDefinition",
        description = Just "Render source definitions for one or more exported symbols when source is available. In cached mode, repeated definitions are omitted unless force=true. Use recursionDepth to include referenced definitions. Returned imports are minified and may not exactly match original module import formatting. This can still succeed usefully during partial load if the requested definition is available.",
        handler = cachedGetDefinitionHandler
      }

cachedGetDefinitionHandler :: (MonadLoreMcp m) => GetDefinitionArgs 'ValueType -> m Text
cachedGetDefinitionHandler GetDefinitionArgs {symbols, skip, recursionDepth} =
  getDefinitionHandlerWithStrategy commonArgs renderWithKnowledgeCache
  where
    commonArgs =
      CommonGetDefinitionArgs
        { symbols,
          skip,
          recursionDepth = Just (max 0 (fromMaybeDefault defaultRecursionDepth recursionDepth))
        }

data HashedDefinitionEntry = HashedDefinitionEntry
  { definitionFingerprint :: Text,
    definitionEntry :: NamedDefinitionSource
  }

renderWithKnowledgeCache ::
  (MonadLoreMcp m) =>
  Int ->
  [NamedDefinitionSource] ->
  m FilteredDefinitions
renderWithKnowledgeCache skip definitionEntries = do
  hashedDefinitions <- hashDefinitionEntries definitionEntries
  let uniqueDefinitions =
        dedupeHashedDefinitionEntries hashedDefinitions
      visibleDefinitionPage =
        paginateDefinitionSources
          skip
          maxRenderedDefinitionResults
          (map (.definitionEntry) uniqueDefinitions)
      visibleDefinitionFingerprints =
        visibleDefinitionFingerprintsForPage visibleDefinitionPage uniqueDefinitions
  cache <- sentDefinitionHashes <$> getLoreMcpContext
  (visibleKnownFingerprints, visibleFreshDefinitions) <- liftIO $
    modifyMVar cache \knownFingerprints -> do
      let visibleFreshFingerprints =
            Set.difference visibleDefinitionFingerprints knownFingerprints
          visibleKnownFingerprints =
            Set.intersection visibleDefinitionFingerprints knownFingerprints
          visibleFreshDefinitions =
            filter
              (\definition -> Set.member definition.definitionFingerprint visibleFreshFingerprints)
              uniqueDefinitions
      pure
        ( Set.union knownFingerprints visibleFreshFingerprints,
          (visibleKnownFingerprints, visibleFreshDefinitions)
        )
  let omittedDefinitions =
        [ definition.definitionEntry.definitionName
        | definition <- uniqueDefinitions,
          Set.member definition.definitionFingerprint visibleKnownFingerprints
        ]
  renderedDefinitions <-
    renderFilteredVisibleDefinitionSources
      visibleDefinitionPage
      0
      maxRenderedDefinitionResults
      (map (.definitionEntry) visibleFreshDefinitions)
  pure
    FilteredDefinitions
      { renderedDefinitions,
        omittedKnownDefinitions = omittedDefinitions,
        omittedKnownDefinitionCount = length omittedDefinitions
      }

hashDefinitionEntries :: (MonadLore m) => [NamedDefinitionSource] -> m [HashedDefinitionEntry]
hashDefinitionEntries definitionEntries =
  mapM hashDefinitionEntry definitionEntries
  where
    hashDefinitionEntry definitionEntry =
      do
        let declarationSpans = definitionEntry.definitionSource.definitionSourceSpans
        declarationBody <- liftIO (Shared.renderDeclarationBodyText declarationSpans)
        pure
          HashedDefinitionEntry
            { definitionFingerprint = definitionFingerprintText definitionEntry declarationSpans declarationBody,
              definitionEntry
            }

definitionFingerprintText :: NamedDefinitionSource -> DeclarationSpans -> Text -> Text
definitionFingerprintText definitionEntry declarationSpans declarationBody =
  definitionBodyHash $
    definitionFingerprintIdentity definitionEntry declarationSpans
      <> "\n"
      <> declarationBody

definitionFingerprintIdentity :: NamedDefinitionSource -> DeclarationSpans -> Text
definitionFingerprintIdentity definitionEntry declarationSpans =
  case declarationSpans.declarationSpan of
    GHC.RealSrcSpan realSpan _ ->
      renderSymbolName definitionEntry.definitionName
        <> ":"
        <> renderModuleName definitionEntry
        <> ":"
        <> realSpanCoords realSpan
        <> ":"
        <> T.pack (GHC.unpackFS (GHC.srcSpanFile realSpan))
    GHC.UnhelpfulSpan unhelpfulSpan ->
      renderSymbolName definitionEntry.definitionName
        <> ":"
        <> renderModuleName definitionEntry
        <> ":"
        <> T.pack (show unhelpfulSpan)

renderModuleName :: NamedDefinitionSource -> Text
renderModuleName definitionEntry =
  renderModuleNameFromModule definitionEntry.definitionSource.definitionSourceModule

renderModuleNameFromModule :: GHC.Module -> Text
renderModuleNameFromModule definitionModule =
  T.pack (GHC.moduleNameString (GHC.moduleName definitionModule))

renderSymbolName :: GHC.Name -> Text
renderSymbolName name =
  T.pack (GHC.showSDocUnsafe (GHC.ppr name))

realSpanCoords :: GHC.RealSrcSpan -> Text
realSpanCoords realSpan =
  T.pack (show (GHC.srcSpanStartLine realSpan))
    <> ":"
    <> T.pack (show (GHC.srcSpanStartCol realSpan))
    <> "-"
    <> T.pack (show (GHC.srcSpanEndLine realSpan))
    <> ":"
    <> T.pack (show (GHC.srcSpanEndCol realSpan))

definitionBodyHash :: Text -> Text
definitionBodyHash declarationBody =
  case fingerprintString (T.unpack declarationBody) of
    Fingerprint highBits lowBits ->
      T.pack (printf "%016x%016x" highBits lowBits)

dedupeHashedDefinitionEntries :: [HashedDefinitionEntry] -> [HashedDefinitionEntry]
dedupeHashedDefinitionEntries =
  reverse . snd . foldl' dedupeOne (Set.empty, [])
  where
    dedupeOne (seenFingerprints, deduped) definition
      | Set.member definition.definitionFingerprint seenFingerprints =
          (seenFingerprints, deduped)
      | otherwise =
          (Set.insert definition.definitionFingerprint seenFingerprints, definition : deduped)

renderFilteredVisibleDefinitionSources ::
  (MonadLore m) =>
  Maybe PaginatedDefinitionSources ->
  Int ->
  Int ->
  [NamedDefinitionSource] ->
  m (Maybe Shared.PaginatedDefinitionModules)
renderFilteredVisibleDefinitionSources visibleDefinitionPage skip maxItems definitions =
  case visibleDefinitionPage of
    Just paginatedSources
      | null paginatedSources.visibleDefinitionSources ->
          -- Preserve original pagination metadata when the selected page exists but
          -- all entries were filtered out (for example, all are already known).
          pure $
            Just
              Shared.PaginatedDefinitionModules
                { totalItems = paginatedSources.sourceTotalItems,
                  skippedItems = paginatedSources.sourceSkippedItems,
                  shownItems = 0,
                  renderedPage = Just ""
                }
    _ ->
      renderPaginatedDefinitionSources skip maxItems definitions

visibleDefinitionFingerprintsForPage :: Maybe PaginatedDefinitionSources -> [HashedDefinitionEntry] -> Set.Set Text
visibleDefinitionFingerprintsForPage visibleDefinitionPage definitions =
  case visibleDefinitionPage of
    Nothing ->
      Set.empty
    Just paginatedSources ->
      Set.unions
        [ Map.findWithDefault Set.empty key fingerprintsBySliceKey
        | key <- Set.toList visibleKeys
        ]
      where
        visibleKeys =
          Set.fromList $
            map definitionSourceCacheKey paginatedSources.visibleDefinitionSources
  where
    fingerprintsBySliceKey =
      Map.fromListWith Set.union $
        concatMap entryFingerprintKeys definitions

    entryFingerprintKeys definition =
      [(definitionSourceCacheKey definition.definitionEntry, Set.singleton definition.definitionFingerprint)]

definitionSourceCacheKey :: NamedDefinitionSource -> DefinitionId
definitionSourceCacheKey definitionEntry =
  definitionEntry.definitionSource.definitionSourceId

fromMaybeDefault :: a -> Maybe a -> a
fromMaybeDefault fallback = \case
  Just value -> value
  Nothing -> fallback
