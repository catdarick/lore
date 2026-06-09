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
import Lore (DeclarationSpans (..), DefinitionId (..), DefinitionSource (..), MonadLore, NamedDefinitionSource (..), definitionSourceModule)
import Lore.Mcp.Internal.Annotated (FieldType (..))
import Lore.Mcp.Internal.Tool (SomeTool (..), ToolWithArgs (..))
import Lore.Mcp.Monad (MonadLoreMcp (..), sentDefinitionHashes)
import Lore.Mcp.Tools.GetDefinition.Shared
  ( BuildDefinitionsStrategy,
    GetDefinitionArgs,
    GetDefinitionResult,
    maxRenderedDefinitionResults,
    mkOmittedDefinitions,
    toGetDefinitionRequest,
    toGetDefinitionResult,
  )
import Lore.Tools.GetDefinition
  ( FilteredDefinitions (..),
    getDefinitionHandlerWithStrategy,
  )
import Lore.Tools.Internal.DefinitionSourceRendering
  ( PaginatedDefinitionSources (..),
    buildPaginatedDefinitionSourceFiles,
    paginateDefinitionSources,
  )
import Lore.Tools.Render.Doc (SourceFile)
import Lore.Tools.Render.Source (declarationBodyText)
import Lore.Tools.Result (PageRequest (..), Paginated (..), ResultLimit (..))
import Text.Printf (printf)

cachedGetDefinitionTool :: (MonadLoreMcp m) => Bool -> SomeTool m
cachedGetDefinitionTool shouldRenderNotifyKnowledgeResetHint =
  SomeToolWithArgs
    ToolWithArgs
      { name = "getDefinition",
        description = Just "Return source definitions for one or more exported symbols, when source code is available. To reduce duplicate output, definitions that were already returned earlier in this session are omitted if they have not changed.",
        handler = cachedGetDefinitionHandler shouldRenderNotifyKnowledgeResetHint
      }

cachedGetDefinitionHandler :: (MonadLoreMcp m) => Bool -> GetDefinitionArgs 'ValueType -> m GetDefinitionResult
cachedGetDefinitionHandler shouldRenderNotifyKnowledgeResetHint args =
  do
    coreResult <-
      getDefinitionHandlerWithStrategy
        (toGetDefinitionRequest args)
        buildWithKnowledgeCache
    pure (toGetDefinitionResult shouldRenderNotifyKnowledgeResetHint coreResult)

data HashedDefinitionEntry = HashedDefinitionEntry
  { definitionFingerprint :: Text,
    definitionEntry :: NamedDefinitionSource
  }

buildWithKnowledgeCache ::
  (MonadLoreMcp m) =>
  BuildDefinitionsStrategy m
buildWithKnowledgeCache pageRequest directlyRequestedSymbolNames definitionEntries = do
  let maxItems =
        case pageRequest.pageLimit of
          Unlimited -> maxRenderedDefinitionResults
          Limit requestedLimit -> min maxRenderedDefinitionResults (max 0 requestedLimit)
  hashedDefinitions <- hashDefinitionEntries definitionEntries
  let uniqueDefinitions =
        dedupeHashedDefinitionEntries hashedDefinitions
      visibleDefinitionPage =
        paginateDefinitionSources
          pageRequest.pageOffset
          maxItems
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
        [ requestedName
        | definition <- uniqueDefinitions,
          Set.member definition.definitionFingerprint visibleKnownFingerprints,
          requestedName <- Set.toList (definition.definitionEntry.definitionSource.definitionSourceNames `Set.intersection` directlyRequestedSymbolNames)
        ]
  filteredDefinitionPage <-
    buildFilteredVisibleDefinitionSourceFiles
      visibleDefinitionPage
      0
      maxItems
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
      renderDefinitionId definitionEntry.definitionSource.definitionSourceId
        <> ":"
        <> renderModuleName definitionEntry
        <> ":"
        <> realSpanCoords realSpan
        <> ":"
        <> T.pack (GHC.unpackFS (GHC.srcSpanFile realSpan))
    GHC.UnhelpfulSpan unhelpfulSpan ->
      renderDefinitionId definitionEntry.definitionSource.definitionSourceId
        <> ":"
        <> renderModuleName definitionEntry
        <> ":"
        <> T.pack (show unhelpfulSpan)

renderModuleName :: NamedDefinitionSource -> Text
renderModuleName definitionEntry =
  renderModuleNameFromModule (definitionSourceModule definitionEntry.definitionSource)

renderModuleNameFromModule :: GHC.Module -> Text
renderModuleNameFromModule definitionModule =
  T.pack (GHC.moduleNameString (GHC.moduleName definitionModule))

renderDefinitionId :: DefinitionId -> Text
renderDefinitionId definitionId =
  renderModuleNameFromModule definitionId.definitionIdModule
    <> ":"
    <> T.pack (show definitionId.definitionIdSpanKey)

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
