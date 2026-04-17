module Lore.Mcp.Tools.GetDefinition.Cached
  ( cachedGetDefinitionTool,
  )
where

import Control.Concurrent.MVar (modifyMVar, modifyMVar_)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as J
import Data.List (foldl')
import Data.OpenApi (ToSchema)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Fingerprint (Fingerprint (..), fingerprintString)
import GHC.Generics (Generic)
import qualified GHC.Plugins as GHC
import Lore (DeclarationSpans (..), DefinitionSlice (..), MonadLore, NamedDefinitionSlice (..))
import Lore.Mcp.Internal.Annotated (Description, Example, ExampleList, Field, FieldType (..), Maximum, MinItems, Minimum, WithMeta)
import Lore.Mcp.Internal.Tool (SomeTool (..), ToolWithArgs (..))
import Lore.Mcp.Monad (MonadLoreMcp (..), sentDefinitionHashes)
import Lore.Mcp.Tools.GetDefinition.Shared
  ( CommonGetDefinitionArgs (..),
    FilteredDefinitions (..),
    defaultRecursionDepth,
    getDefinitionHandlerWithStrategy,
    maxRenderedDefinitionResults,
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
                    ],
    force ::
      Maybe (Field fieldType Bool)
        `WithMeta` '[ Description "When true, the knowledge check is ignored and all requested symbol definitions are returned, including recursive results."
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
cachedGetDefinitionHandler GetDefinitionArgs {symbols, skip, recursionDepth, force} =
  getDefinitionHandlerWithStrategy commonArgs (renderWithKnowledgeCache forceDefinitions)
  where
    commonArgs =
      CommonGetDefinitionArgs
        { symbols,
          skip,
          recursionDepth = Just (max 0 (fromMaybeDefault defaultRecursionDepth recursionDepth))
        }
    forceDefinitions = fromMaybeDefault False force

data HashedDefinitionEntry = HashedDefinitionEntry
  { definitionFingerprint :: Text,
    definitionEntry :: NamedDefinitionSlice
  }

renderWithKnowledgeCache ::
  (MonadLoreMcp m) =>
  Bool ->
  Int ->
  [NamedDefinitionSlice] ->
  m FilteredDefinitions
renderWithKnowledgeCache forceDefinitions skip definitionEntries = do
  hashedDefinitions <- hashDefinitionEntries definitionEntries
  let uniqueDefinitions =
        dedupeHashedDefinitionEntries hashedDefinitions
      allFingerprints =
        Set.fromList (map (.definitionFingerprint) uniqueDefinitions)
      allDefinitionSlices =
        map (\definition -> definition.definitionEntry.definitionSlice) uniqueDefinitions
  cache <- sentDefinitionHashes <$> getLoreMcpContext
  if forceDefinitions
    then do
      liftIO $
        modifyMVar_ cache \knownFingerprints ->
          pure (Set.union knownFingerprints allFingerprints)
      renderedDefinitions <-
        liftIO $
          Shared.renderPaginatedDefinitionModules
            skip
            maxRenderedDefinitionResults
            allDefinitionSlices
      pure
        FilteredDefinitions
          { renderedDefinitions,
            omittedKnownDefinitions = [],
            omittedKnownDefinitionCount = 0
          }
    else do
      (knownFingerprints, freshDefinitions) <- liftIO $
        modifyMVar cache \knownFingerprints -> do
          let freshDefinitions =
                filter
                  (\definition -> Set.notMember definition.definitionFingerprint knownFingerprints)
                  uniqueDefinitions
              freshFingerprints =
                Set.fromList (map (.definitionFingerprint) freshDefinitions)
          pure
            ( Set.union knownFingerprints freshFingerprints,
              (knownFingerprints, freshDefinitions)
            )
      let freshDefinitionSlices =
            map (\definition -> definition.definitionEntry.definitionSlice) freshDefinitions
          omittedDefinitions =
            [ definition.definitionEntry.definitionName
            | definition <- uniqueDefinitions,
              Set.member definition.definitionFingerprint knownFingerprints
            ]
      renderedDefinitions <-
        liftIO $
          Shared.renderPaginatedDefinitionModules
            skip
            maxRenderedDefinitionResults
            freshDefinitionSlices
      pure
        FilteredDefinitions
          { renderedDefinitions,
            omittedKnownDefinitions = omittedDefinitions,
            omittedKnownDefinitionCount = length omittedDefinitions
          }

hashDefinitionEntries :: (MonadLore m) => [NamedDefinitionSlice] -> m [HashedDefinitionEntry]
hashDefinitionEntries definitionEntries =
  concat <$> mapM hashDefinitionEntry (expandDefinitionEntries definitionEntries)
  where
    hashDefinitionEntry definitionEntry =
      case definitionEntry.definitionSlice.declarationSpans of
        [declarationSpans] -> do
          declarationBody <- liftIO (Shared.renderDeclarationBodyText declarationSpans)
          pure
            [ HashedDefinitionEntry
                { definitionFingerprint = definitionFingerprintText definitionEntry declarationSpans declarationBody,
                  definitionEntry
                }
            ]
        _ ->
          pure []

expandDefinitionEntries :: [NamedDefinitionSlice] -> [NamedDefinitionSlice]
expandDefinitionEntries =
  concatMap \definitionEntry ->
    [ definitionEntry
        { definitionSlice =
            definitionEntry.definitionSlice
              { declarationSpans = [declarationSpans]
              }
        }
    | declarationSpans <- definitionEntry.definitionSlice.declarationSpans
    ]

definitionFingerprintText :: NamedDefinitionSlice -> DeclarationSpans -> Text -> Text
definitionFingerprintText definitionEntry declarationSpans declarationBody =
  definitionBodyHash $
    definitionFingerprintIdentity definitionEntry declarationSpans
      <> "\n"
      <> declarationBody

definitionFingerprintIdentity :: NamedDefinitionSlice -> DeclarationSpans -> Text
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

renderModuleName :: NamedDefinitionSlice -> Text
renderModuleName definitionEntry =
  T.pack (GHC.moduleNameString (GHC.moduleName definitionEntry.definitionSlice.definitionModule))

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

fromMaybeDefault :: a -> Maybe a -> a
fromMaybeDefault fallback = \case
  Just value -> value
  Nothing -> fallback
