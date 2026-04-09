module Lore.Mcp.Tools.LookupSymbolInfo
  ( lookupSymbolInfoTool,
  )
where

import qualified Data.Aeson as J
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC
import GHC.Generics (Generic)
import qualified GHC.Iface.Syntax as Iface
import qualified GHC.Plugins as Plugins
import qualified GHC.Types.TyThing as TyThing
import qualified GHC.Types.TyThing.Ppr as TyThing
import qualified GHC.Utils.Outputable as Outputable
import Lore
  ( LoadTargetsResult (..),
    MonadLore,
    SymbolInfo (..),
    getLastLoadTargetsResult,
    lookupRootSymbolInfo,
  )
import Lore.Mcp.Internal.Annotated (Description, Example, Field, FieldType (..), WithMeta)
import Lore.Mcp.Internal.Tool (SomeTool (..), ToolWithArgs (..))
import Lore.Mcp.Tools.Shared (appendPartialLoadWarning)

data LookupSymbolInfoArgs (fieldType :: FieldType) = LookupSymbolInfoArgs
  { symbol ::
      Field
        fieldType
        ( WithMeta
            Text
            '[ Description "Exact symbol name to look up in the loaded project symbol table.",
               Example "lookupOrZero"
             ]
        )
  }
  deriving stock (Generic)

instance J.FromJSON (LookupSymbolInfoArgs 'ValueType)

instance ToSchema (LookupSymbolInfoArgs 'MetadataType)

lookupSymbolInfoTool :: (MonadLore m) => SomeTool m
lookupSymbolInfoTool =
  SomeToolWithArgs
    ToolWithArgs
      { name = "lookupSymbolInfo",
        description = Just "Look up information about an exported symbol in the currently loaded project. Queries are resolved to root declarations automatically.",
        handler = lookupSymbolInfoHandler
      }

lookupSymbolInfoHandler :: (MonadLore m) => LookupSymbolInfoArgs 'ValueType -> m Text
lookupSymbolInfoHandler LookupSymbolInfoArgs {symbol} = do
  maybeLoadResult <- getLastLoadTargetsResult
  case maybeLoadResult of
    Nothing ->
      pure "Targets have not been loaded yet. Run loadTargets first."
    Just loadResult -> do
      symbolInfos <- lookupRootSymbolInfo symbol
      pure (renderLookupResult loadResult symbol symbolInfos)

renderLookupResult :: LoadTargetsResult -> Text -> [SymbolInfo] -> Text
renderLookupResult loadResult symbol symbolInfos =
  appendPartialLoadWarning loadResult "Lookup results may be incomplete." renderedBody
  where
    renderedBody =
      case symbolInfos of
        [] ->
          "No symbols found for " <> quoteText symbol <> "."
        _ ->
          T.intercalate "\n\n" (map renderSymbolInfo symbolInfos)

renderSymbolInfo :: SymbolInfo -> Text
renderSymbolInfo symbolInfo =
  T.intercalate "\n" $
    [ renderSymbolHeader symbolInfo,
      "  Exported from: " <> renderModules symbolInfo.exportedFrom
    ]
      <> maybe [] (\location -> ["  Defined at: " <> location]) (renderDefinitionLocation symbolInfo.symbolName)
      <> renderClassInstances symbolInfo.associatedClassInstances
      <> renderFamilyInstances symbolInfo.associatedFamilyInstances

renderSymbolHeader :: SymbolInfo -> Text
renderSymbolHeader symbolInfo =
  case symbolInfo.symbolThing of
    Just (TyThing.AnId {}) ->
      renderQualifiedName symbolInfo.symbolName
        <> maybe "" (" :: " <>) (renderType <$> symbolInfo.symbolType)
    Just tyThing ->
      renderTyThing tyThing
    Nothing ->
      renderQualifiedName symbolInfo.symbolName

renderTyThing :: GHC.TyThing -> Text
renderTyThing =
  renderOutputableWith (TyThing.pprTyThingInContext showSub)
  where
    showSub =
      Iface.ShowSub
        { Iface.ss_how_much = Iface.ShowHeader (Iface.AltPpr Nothing),
          Iface.ss_forall = Iface.ShowForAllWhen
        }

renderQualifiedName :: GHC.Name -> Text
renderQualifiedName =
  renderOutputable

renderModuleName :: GHC.Module -> Text
renderModuleName =
  T.pack . GHC.moduleNameString . GHC.moduleName

renderModules :: [GHC.Module] -> Text
renderModules modules =
  case modules of
    [] -> "<none>"
    _ -> T.pack (intercalate ", " (map (T.unpack . renderModuleName) modules))

renderType :: GHC.Type -> Text
renderType =
  renderOutputable

renderClassInstances :: [GHC.ClsInst] -> [Text]
renderClassInstances instances_ =
  renderInstancesSection "  Class instances:" renderClassInstance instances_

renderClassInstance :: GHC.ClsInst -> Text
renderClassInstance =
  compactClassInstance . renderOutputable

renderFamilyInstances :: [GHC.FamInst] -> [Text]
renderFamilyInstances instances_ =
  renderInstancesSection "  Family instances:" renderFamilyInstance instances_

renderFamilyInstance :: GHC.FamInst -> Text
renderFamilyInstance =
  compactRenderedInstance . renderOutputable

renderInstancesSection :: Text -> (a -> Text) -> [a] -> [Text]
renderInstancesSection heading renderInstance instances_ =
  case instances_ of
    [] -> []
    _ ->
      let visibleInstances = take maxRenderedInstances instances_
          hiddenCount = length instances_ - length visibleInstances
       in [heading]
            <> map (("    - " <>) . renderInstance) visibleInstances
            <> [ "    ... and " <> T.pack (show hiddenCount) <> " more instances"
               | hiddenCount > 0
               ]

compactClassInstance :: Text -> Text
compactClassInstance =
  stripInstancePrefix
    . compactRenderedInstance

compactRenderedInstance :: Text -> Text
compactRenderedInstance =
  T.unwords
    . takeWhile (not . isDefinitionCommentLine)
    . filter (not . T.null)
    . map stripTrailingComment
    . map T.strip
    . T.lines

stripInstancePrefix :: Text -> Text
stripInstancePrefix text =
  fromMaybe text (T.stripPrefix "instance " text)

stripTrailingComment :: Text -> Text
stripTrailingComment text =
  T.strip $
    case T.breakOn " -- " text of
      (prefix, suffix)
        | T.null suffix -> text
        | otherwise -> prefix

isDefinitionCommentLine :: Text -> Bool
isDefinitionCommentLine =
  T.isPrefixOf "-- Defined"

maxRenderedInstances :: Int
maxRenderedInstances = 15

renderOutputable :: (Outputable.Outputable a) => a -> Text
renderOutputable =
  T.pack . Outputable.showSDocUnsafe . Outputable.ppr

renderOutputableWith :: (a -> Outputable.SDoc) -> a -> Text
renderOutputableWith render =
  T.pack . Outputable.showSDocUnsafe . render

renderDefinitionLocation :: GHC.Name -> Maybe Text
renderDefinitionLocation name = do
  realSpan <- Plugins.srcSpanToRealSrcSpan (Plugins.nameSrcSpan name)
  pure $
    T.pack (Plugins.unpackFS (Plugins.srcSpanFile realSpan))
      <> ":"
      <> T.pack (show (Plugins.srcSpanStartLine realSpan))
      <> ":"
      <> T.pack (show (Plugins.srcSpanStartCol realSpan))
      <> "-"
      <> renderEndPosition realSpan

renderEndPosition :: GHC.RealSrcSpan -> Text
renderEndPosition realSpan
  | Plugins.srcSpanStartLine realSpan == Plugins.srcSpanEndLine realSpan =
      T.pack (show endColInclusive)
  | otherwise =
      T.pack (show (Plugins.srcSpanEndLine realSpan))
        <> ":"
        <> T.pack (show endColInclusive)
  where
    endColInclusive =
      max (Plugins.srcSpanStartCol realSpan) (Plugins.srcSpanEndCol realSpan - 1)

quoteText :: Text -> Text
quoteText value =
  "\"" <> value <> "\""
