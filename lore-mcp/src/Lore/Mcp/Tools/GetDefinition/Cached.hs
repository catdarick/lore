module Lore.Mcp.Tools.GetDefinition.Cached
  ( cachedGetDefinitionTool,
  )
where

import Control.Concurrent.MVar (modifyMVar)
import Control.Monad.IO.Class (liftIO)
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Fingerprint (Fingerprint (..), fingerprintString)
import qualified GHC.Plugins as GHC
import Lore (DeclarationSpans (..), DefinitionId, DefinitionSource (..), MonadLore, NamedDefinitionSource (..))
import Lore.Mcp.Internal.Annotated (FieldType (..))
import Lore.Mcp.Internal.LoreDoc (SourceFile)
import Lore.Mcp.Internal.Tool (SomeTool (..), ToolWithArgs (..))
import Lore.Mcp.Monad (MonadLoreMcp (..), sentDefinitionHashes)
import Lore.Mcp.Tools.GetDefinition.Shared
  ( BuildDefinitionsStrategy,
    FilteredDefinitions (..),
    GetDefinitionArgs,
    GetDefinitionResult,
    getDefinitionHandlerWithStrategy,
    maxRenderedDefinitionResults,
    mkOmittedDefinitions,
  )
import Lore.Mcp.Tools.Shared (Paginated (..))
import Lore.Mcp.Tools.Shared.DefinitionSourceRendering
  ( PaginatedDefinitionSources (..),
    buildPaginatedDefinitionSourceFiles,
    paginateDefinitionSources,
  )
import Lore.Mcp.Tools.Shared.Source (declarationBodyText)
import Text.Printf (printf)

cachedGetDefinitionTool :: (MonadLoreMcp m) => Bool -> SomeTool m
cachedGetDefinitionTool shouldRenderNotifyKnowledgeResetHint =
  SomeToolWithArgs
    ToolWithArgs
      { name = "getDefinition",
        description = Just "Render source definitions for one or more exported symbols when source is available. In cached mode, repeated definitions are omitted unless force=true. Use expansion to control dependency inclusion: None (target only), Direct (maxDepth=1), Recursive (maxDepth=2, maxSymbols=200). Returned imports are minified and may not exactly match original module import formatting. This can still succeed usefully during partial load if the requested definition is available.",
        handler = cachedGetDefinitionHandler shouldRenderNotifyKnowledgeResetHint
      }

cachedGetDefinitionHandler :: (MonadLoreMcp m) => Bool -> GetDefinitionArgs 'ValueType -> m GetDefinitionResult
cachedGetDefinitionHandler shouldRenderNotifyKnowledgeResetHint args =
  getDefinitionHandlerWithStrategy shouldRenderNotifyKnowledgeResetHint args buildWithKnowledgeCache

data HashedDefinitionEntry = HashedDefinitionEntry
  { definitionFingerprint :: Text,
    definitionEntry :: NamedDefinitionSource
  }

buildWithKnowledgeCache ::
  (MonadLoreMcp m) =>
  BuildDefinitionsStrategy m
buildWithKnowledgeCache skip definitionEntries = do
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
  filteredDefinitionPage <-
    buildFilteredVisibleDefinitionSourceFiles
      visibleDefinitionPage
      0
      maxRenderedDefinitionResults
      (map (.definitionEntry) visibleFreshDefinitions)
  pure
    FilteredDefinitions
      { filteredDefinitionPage,
        filteredOmittedDefinitions = mkOmittedDefinitions omittedDefinitions
      }

hashDefinitionEntries :: (MonadLore m) => [NamedDefinitionSource] -> m [HashedDefinitionEntry]
hashDefinitionEntries definitionEntries =
  mapM hashDefinitionEntry definitionEntries
  where
    hashDefinitionEntry definitionEntry =
      do
        let declarationSpans = definitionEntry.definitionSource.definitionSourceSpans
        declarationBody <- liftIO (declarationBodyText declarationSpans)
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

buildFilteredVisibleDefinitionSourceFiles ::
  (MonadLore m) =>
  Maybe PaginatedDefinitionSources ->
  Int ->
  Int ->
  [NamedDefinitionSource] ->
  m (Maybe (Paginated SourceFile))
buildFilteredVisibleDefinitionSourceFiles visibleDefinitionPage skip maxItems definitions =
  case visibleDefinitionPage of
    Just paginatedSources
      | null definitions ->
          pure
            ( Just
                Paginated
                  { paginatedTotalItems = paginatedSources.sourceTotalItems,
                    paginatedSkippedItems = paginatedSources.sourceSkippedItems,
                    paginatedShownItems = 0,
                    paginatedConsumedItems = length paginatedSources.visibleDefinitionSources,
                    paginatedItems = []
                  }
            )
    _ ->
      buildPaginatedDefinitionSourceFiles skip maxItems definitions

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
