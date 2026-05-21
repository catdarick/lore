module Lore.Mcp.Tools.ResolveInstance
  ( resolveInstanceTool,
  )
where

import qualified Data.Aeson as J
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC
import qualified GHC.Core.InstEnv as InstEnv
import GHC.Generics (Generic)
import qualified GHC.Plugins as Plugins
import Lore
  ( ChosenInstanceContextStatus (..),
    ChosenInstanceError (..),
    ChosenInstanceResolution (..),
    MonadLore,
    NamedDefinitionSource (..),
    resolveChosenClassInstanceFromTypeText,
    resolveDefinitionSourceNamed,
  )
import Lore.Internal.Lookup.TypeQuery
  ( TypeQueryUnresolvedSymbolQuery (..),
    TypeQueryUnresolvedSymbols (..),
  )
import Lore.Mcp.Internal.Annotated
  ( Description,
    Example,
    Field,
    FieldType (..),
    WithMeta,
  )
import Lore.Mcp.Internal.LoreDoc (LoreDoc, SourceFile, ToLoreDoc (toLoreDoc), paragraph, sourceFile)
import Lore.Mcp.Internal.Tool (SomeTool (..), ToolWithArgs (..))
import Lore.Mcp.Tools.Shared
  ( PartialLoadWarning,
    ToolRun (..),
    loadedSessionPartialWarning,
    withLoadedSession,
  )
import Lore.Mcp.Tools.Shared.DefinitionSourceRendering (buildDefinitionSourceFiles)
import Lore.Mcp.Tools.Shared.Outputable (renderOutputable)

data ResolveInstanceArgs (fieldType :: FieldType) = ResolveInstanceArgs
  { query ::
      Field fieldType Text
        `WithMeta` '[ Description "Class application to resolve.",
                      Example "Render (Maybe Foo)",
                      Example "Show Bar",
                      Example "TwoTypeClass TypeOne TypeTwo"
                    ]
  }
  deriving stock (Generic)

instance J.FromJSON (ResolveInstanceArgs 'ValueType)

instance ToSchema (ResolveInstanceArgs 'MetadataType)

resolveInstanceTool :: (MonadLore m) => SomeTool m
resolveInstanceTool =
  SomeToolWithArgs
    ToolWithArgs
      { name = "resolveInstance",
        description = Just "Resolve the class instance. When source is available, the tool renders the selected instance definition; otherwise it returns the selected instance head and defining module.",
        handler = resolveInstanceHandler
      }

type ResolveInstanceResult = ToolRun ResolveInstanceOutput

data ResolveInstanceOutput
  = ResolveInstanceFailed ResolveInstanceFailure
  | ResolveInstanceReadyResult ResolveInstanceReady

data ResolveInstanceFailure = ResolveInstanceFailure
  { resolveInstanceFailureReason :: ResolveInstanceFailureReason,
    resolveInstanceFailurePartialLoadWarning :: Maybe PartialLoadWarning
  }

data ResolveInstanceFailureReason
  = ResolveInstanceUnsupportedQuery !Text !Text
  | ResolveInstanceUnresolvedSymbols !TypeQueryUnresolvedSymbols
  | ResolveInstanceGhcTypeError !Text
  | ResolveInstanceLookupFailed !Text !Text

data ResolveInstanceReady = ResolveInstanceReady
  { resolveInstanceQuery :: !Text,
    resolveInstanceSelectedHead :: !Text,
    resolveInstanceInstantiatedAs :: ![Text],
    resolveInstanceRequiredConstraints :: ![Text],
    resolveInstanceContextStatus :: !ChosenInstanceContextStatus,
    resolveInstanceSourceFiles :: ![SourceFile],
    resolveInstancePartialLoadWarning :: !(Maybe PartialLoadWarning)
  }

instance ToLoreDoc ResolveInstanceOutput where
  toLoreDoc = \case
    ResolveInstanceFailed failed ->
      toLoreDoc failed
    ResolveInstanceReadyResult ready ->
      toLoreDoc ready

instance ToLoreDoc ResolveInstanceFailure where
  toLoreDoc failed =
    mconcat
      [ toLoreDoc failed.resolveInstanceFailureReason,
        maybe mempty toLoreDoc failed.resolveInstanceFailurePartialLoadWarning
      ]

instance ToLoreDoc ResolveInstanceFailureReason where
  toLoreDoc = \case
    ResolveInstanceUnsupportedQuery queryText details ->
      paragraph $
        "Unsupported instance query "
          <> quoteText queryText
          <> ":\n"
          <> details
    ResolveInstanceUnresolvedSymbols unresolved ->
      paragraph (renderUnresolvedTypeQuerySymbols unresolved)
    ResolveInstanceGhcTypeError details ->
      paragraph ("GHC rejected the resolved class application type:\n" <> details)
    ResolveInstanceLookupFailed queryText details ->
      paragraph $
        "Could not resolve a unique instance for "
          <> quoteText queryText
          <> ":\n"
          <> details

instance ToLoreDoc ResolveInstanceReady where
  toLoreDoc ready =
    mconcat
      ( [selectedInstanceDoc ready.resolveInstanceSelectedHead ready.resolveInstanceSourceFiles]
          <> optionalConstraintSections ready
          <> [maybe mempty toLoreDoc ready.resolveInstancePartialLoadWarning]
      )

optionalConstraintSections :: ResolveInstanceReady -> [LoreDoc]
optionalConstraintSections ready
  | null ready.resolveInstanceRequiredConstraints =
      []
  | otherwise =
      [ sectionListDoc "Instantiated as:" ready.resolveInstanceInstantiatedAs "(none)",
        sectionListDoc "Required constraints:" ready.resolveInstanceRequiredConstraints "(none)",
        unresolvedDoc ready.resolveInstanceRequiredConstraints ready.resolveInstanceContextStatus
      ]

selectedInstanceDoc :: Text -> [SourceFile] -> LoreDoc
selectedInstanceDoc selectedHead sourceFiles =
  mconcat
    [ paragraph "Selected instance:",
      if null sourceFiles
        then paragraph ("  " <> selectedHead)
        else mconcat (map sourceFile sourceFiles)
    ]

sectionListDoc :: Text -> [Text] -> Text -> LoreDoc
sectionListDoc sectionTitle values emptyValue =
  paragraph $
    sectionTitle
      <> "\n"
      <> T.intercalate "\n" (map ("- " <>) (if null values then [emptyValue] else values))

unresolvedDoc :: [Text] -> ChosenInstanceContextStatus -> LoreDoc
unresolvedDoc requiredConstraints contextStatus =
  case contextStatus of
    ChosenInstanceContextResolved ->
      sectionListDoc "Unresolved:" [] "(none)"
    ChosenInstanceContextUnresolved details ->
      let unresolvedConstraints =
            if null requiredConstraints
              then [details]
              else requiredConstraints
       in sectionListDoc "Unresolved:" unresolvedConstraints "(none)"

resolveInstanceHandler :: (MonadLore m) => ResolveInstanceArgs 'ValueType -> m ResolveInstanceResult
resolveInstanceHandler ResolveInstanceArgs {query} =
  withLoadedSession \session -> do
    let partialLoadWarning =
          loadedSessionPartialWarning session "Instance resolution results may be incomplete."
    case normalizeInstanceQuery query of
      Nothing ->
        pure
          ( ResolveInstanceFailed
              ResolveInstanceFailure
                { resolveInstanceFailureReason =
                    ResolveInstanceUnsupportedQuery
                      query
                      "Expected a class application such as `Render Foo` or `Render (Maybe Foo)`.",
                  resolveInstanceFailurePartialLoadWarning = partialLoadWarning
                }
          )
      Just parsedQuery -> do
        eiResolved <- resolveChosenClassInstanceFromTypeText parsedQuery.parsedClassApplication
        case eiResolved of
          Left err ->
            pure
              ( ResolveInstanceFailed
                  ResolveInstanceFailure
                    { resolveInstanceFailureReason = chosenInstanceErrorToFailure parsedQuery.parsedRenderedQuery err,
                      resolveInstanceFailurePartialLoadWarning = partialLoadWarning
                    }
              )
          Right resolved -> do
            sourceFiles <- resolveInstanceSourceFilesFor resolved.chosenInstance
            pure $
              ResolveInstanceReadyResult
                ResolveInstanceReady
                  { resolveInstanceQuery = parsedQuery.parsedRenderedQuery,
                    resolveInstanceSelectedHead = renderOutputable resolved.chosenInstance,
                    resolveInstanceInstantiatedAs = renderInstanceInstantiations resolved,
                    resolveInstanceRequiredConstraints = map renderType resolved.chosenInstanceContextPredicates,
                    resolveInstanceContextStatus = resolved.chosenInstanceContextStatus,
                    resolveInstanceSourceFiles = sourceFiles,
                    resolveInstancePartialLoadWarning = partialLoadWarning
                  }

data ParsedInstanceQuery = ParsedInstanceQuery
  { parsedClassApplication :: !Text,
    parsedRenderedQuery :: !Text
  }

normalizeInstanceQuery :: Text -> Maybe ParsedInstanceQuery
normalizeInstanceQuery rawQuery =
  case T.words rawQuery of
    [] ->
      Nothing
    "instance" : rest ->
      mkParsed (T.unwords rest)
    _ ->
      mkParsed (T.strip rawQuery)
  where
    mkParsed classApplication
      | T.null normalizedApplication =
          Nothing
      | otherwise =
          Just
            ParsedInstanceQuery
              { parsedClassApplication = normalizedApplication,
                parsedRenderedQuery = "instance " <> normalizedApplication
              }
      where
        normalizedApplication =
          T.strip classApplication

resolveInstanceSourceFilesFor :: (MonadLore m) => InstEnv.ClsInst -> m [SourceFile]
resolveInstanceSourceFilesFor clsInst = do
  let instanceName = GHC.getName clsInst
  maybeSource <- resolveDefinitionSourceNamed instanceName
  case maybeSource of
    Nothing ->
      pure []
    Just source ->
      buildDefinitionSourceFiles [NamedDefinitionSource instanceName source]

chosenInstanceErrorToFailure :: Text -> ChosenInstanceError -> ResolveInstanceFailureReason
chosenInstanceErrorToFailure rawQuery = \case
  ChosenInstanceTypeParseFailed details ->
    ResolveInstanceUnsupportedQuery rawQuery details
  ChosenInstanceNameResolutionFailed unresolved ->
    ResolveInstanceUnresolvedSymbols unresolved
  ChosenInstanceUnsupportedParsedType details ->
    ResolveInstanceUnsupportedQuery rawQuery details
  ChosenInstanceGhcTypeCheckFailed details ->
    ResolveInstanceGhcTypeError details
  ChosenInstanceNotAClassApplication details ->
    ResolveInstanceUnsupportedQuery rawQuery details
  ChosenInstanceLookupFailed details ->
    ResolveInstanceLookupFailed rawQuery details

renderType :: GHC.Type -> Text
renderType =
  T.pack . Plugins.showSDocUnsafe . Plugins.ppr

renderInstanceInstantiations :: ChosenInstanceResolution -> [Text]
renderInstanceInstantiations resolution =
  if length instanceTypeVars /= length resolution.chosenInstanceDfunInstTypes
    then []
    else zipWith renderOne instanceTypeVars resolution.chosenInstanceDfunInstTypes
  where
    (instanceTypeVars, _, _, _) =
      InstEnv.instanceSig resolution.chosenInstance

    renderOne typeVar instantiatedType =
      renderOutputable typeVar <> " ~ " <> renderType instantiatedType

renderUnresolvedTypeQuerySymbols :: TypeQueryUnresolvedSymbols -> Text
renderUnresolvedTypeQuerySymbols unresolved =
  T.intercalate "\n\n" (map renderUnresolvedQuery unresolved.unresolvedTypeQuerySymbols)
  where
    renderUnresolvedQuery = \case
      TypeQueryUnresolvedSymbolQueryMissing queryText ->
        "No symbols found for " <> quoteText queryText <> "."
      TypeQueryUnresolvedSymbolQueryAmbiguous queryText disambiguationHints ->
        T.intercalate
          "\n"
          ( [ "The requested name " <> quoteText queryText <> " is ambiguous. More qualification is required:",
              ""
            ]
              <> map ("  - " <>) disambiguationHints
              <> ["", "Run the tool again with a fully qualified symbol name from the list above."]
          )

quoteText :: Text -> Text
quoteText value =
  "\"" <> value <> "\""
