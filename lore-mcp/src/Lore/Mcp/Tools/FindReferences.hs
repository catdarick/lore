module Lore.Mcp.Tools.FindReferences
  ( findReferencesTool,
  )
where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as J
import Data.List (sortOn)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC
import GHC.Generics (Generic)
import Lore
  ( DefinitionSlice,
    LoadTargetsResult,
    MonadLore,
    RootSymbolInfo (..),
    SymbolInfo (..),
    getLastLoadTargetsResult,
    lookupRootSymbolInfoWithChain,
    renderDefinitionModulesText,
    resolveReferenceDefinitionsForNames,
  )
import Lore.Mcp.Internal.Annotated (Description, Example, Field, FieldType (..), WithMeta)
import Lore.Mcp.Internal.Tool (SomeTool (..), ToolWithArgs (..))
import Lore.Mcp.Tools.Shared (appendPartialLoadWarning)

newtype FindReferencesArgs (fieldType :: FieldType) = FindReferencesArgs
  { symbol ::
      Field fieldType Text
        `WithMeta` '[ Description "Exact symbol name to find references for. Module qualification (e.g., Some.Module.someFunction) is supported and can be used to resolve ambiguity or provide specific scope.",
                      Example "lookupOrZero",
                      Example "Some.Module.someFunction"
                    ]
  }
  deriving stock (Generic)

instance J.FromJSON (FindReferencesArgs 'ValueType)

instance ToSchema (FindReferencesArgs 'MetadataType)

findReferencesTool :: (MonadLore m) => SomeTool m
findReferencesTool =
  SomeToolWithArgs
    ToolWithArgs
      { name = "findReferences",
        description = Just "Render all the source definitions that reference the requested symbol, including instance declarations.",
        handler = findReferencesHandler
      }

findReferencesHandler :: (MonadLore m) => FindReferencesArgs 'ValueType -> m Text
findReferencesHandler FindReferencesArgs {symbol} = do
  maybeLoadResult <- getLastLoadTargetsResult
  case maybeLoadResult of
    Nothing ->
      pure "Targets have not been loaded yet. Run reloadHomeModules first."
    Just loadResult -> do
      resolvedRoots <- lookupRootSymbolInfoWithChain symbol
      case resolvedRoots of
        [] ->
          pure $ renderMissingResult loadResult symbol
        [resolvedRoot] -> do
          references <- resolveReferenceDefinitionsForNames resolvedRoot.rootSymbolChain
          renderedReferences <- renderReferenceDefinitions references
          pure $ renderReferencesResult loadResult symbol renderedReferences
        ambiguousRoots ->
          pure $ renderAmbiguousResult loadResult symbol ambiguousRoots

renderReferenceDefinitions :: (MonadLore m) => [DefinitionSlice] -> m (Maybe Text)
renderReferenceDefinitions definitionSlices =
  case definitionSlices of
    [] ->
      pure Nothing
    _ ->
      Just <$> liftIO (renderDefinitionModulesText definitionSlices)

renderMissingResult :: LoadTargetsResult -> Text -> Text
renderMissingResult loadResult symbol =
  appendPartialLoadWarning loadResult "Reference results may be incomplete." $
    "No symbols found for " <> quoteText symbol <> "."

renderReferencesResult :: LoadTargetsResult -> Text -> Maybe Text -> Text
renderReferencesResult loadResult symbol renderedReferences =
  appendPartialLoadWarning loadResult "Reference results may be incomplete." $
    case renderedReferences of
      Nothing ->
        "No references found for " <> quoteText symbol <> "."
      Just renderedDefinitions ->
        renderedDefinitions

renderAmbiguousResult :: LoadTargetsResult -> Text -> [RootSymbolInfo] -> Text
renderAmbiguousResult loadResult symbol ambiguousMatches =
  appendPartialLoadWarning loadResult "Reference results may be incomplete." renderedBody
  where
    renderedBody =
      T.intercalate "\n" $
        [ "The requested name " <> quoteText symbol <> " is ambiguous. More qualification is required:",
          ""
        ]
          <> map (("  - " <>) . renderModuleName) (ambiguousDefinitionModules ambiguousMatches)
          <> ["", "Run the tool again with a qualified symbol name, for example: " <> renderExampleQualification symbol ambiguousMatches]

ambiguousDefinitionModules :: [RootSymbolInfo] -> [GHC.Module]
ambiguousDefinitionModules =
  map head
    . groupModules
    . sortOn renderModuleName
    . map (.rootSymbolInfo.definedIn)
  where
    groupModules [] = []
    groupModules (module_ : modules) =
      let (matchingModules, rest) = span ((== renderModuleName module_) . renderModuleName) modules
       in (module_ : matchingModules) : groupModules rest

renderModuleName :: GHC.Module -> Text
renderModuleName =
  T.pack . GHC.moduleNameString . GHC.moduleName

renderExampleQualification :: Text -> [RootSymbolInfo] -> Text
renderExampleQualification queryText ambiguousMatches =
  case ambiguousDefinitionModules ambiguousMatches of
    module_ : _ ->
      renderModuleName module_ <> "." <> queryOccName queryText
    [] ->
      queryText

queryOccName :: Text -> Text
queryOccName queryText =
  case reverse (T.splitOn "." queryText) of
    occName : _ | not (T.null occName) -> occName
    _ -> queryText

quoteText :: Text -> Text
quoteText value =
  "\"" <> value <> "\""
