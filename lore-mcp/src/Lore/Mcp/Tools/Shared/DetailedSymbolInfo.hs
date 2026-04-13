module Lore.Mcp.Tools.Shared.DetailedSymbolInfo
  ( DetailedSymbolInfo (..),
  )
where

import Data.List (intercalate)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC
import qualified GHC.Iface.Syntax as Iface
import qualified GHC.Types.TyThing as TyThing
import qualified GHC.Types.TyThing.Ppr as TyThing
import Lore (SymbolInfo (..))
import Lore.Mcp.Internal.Render
  ( ListMarker (BulletMarker),
    RenderList (..),
    Renderable (renderText),
    Truncation (..),
    indented,
    (|>),
  )
import Lore.Mcp.Tools.Shared.CompactClassInstance (CompactClassInstance (CompactClassInstance))
import Lore.Mcp.Tools.Shared.DefinitionLocation (mkDefinitionLocation)
import Lore.Mcp.Tools.Shared.Outputable (renderOutputable, renderOutputableWith)

newtype DetailedSymbolInfo = DetailedSymbolInfo SymbolInfo

instance Renderable DetailedSymbolInfo where
  renderText (DetailedSymbolInfo symbolInfo) =
    renderText $
      renderSymbolHeader symbolInfo
        |> indented
          ( definitionLocation symbolInfo
              |> renderExportedModules symbolInfo
              |> classInstancesSection symbolInfo
          )

renderSymbolHeader :: SymbolInfo -> Text
renderSymbolHeader symbolInfo =
  case symbolInfo.symbolThing of
    TyThing.AnId {} ->
      let symbolName = renderOutputable symbolInfo.symbolName
          symbolType = case symbolInfo.symbolType of
            Nothing -> ""
            Just typ -> " :: " <> renderOutputable typ
       in symbolName <> symbolType
    tyThing ->
      renderTyThing tyThing
  where
    renderTyThing =
      renderOutputableWith (TyThing.pprTyThingInContext showSub)
      where
        showSub =
          Iface.ShowSub
            { Iface.ss_how_much = Iface.ShowHeader (Iface.AltPpr Nothing),
              Iface.ss_forall = Iface.ShowForAllWhen
            }

definitionLocation :: SymbolInfo -> Text
definitionLocation symbolInfo =
  case mkDefinitionLocation symbolInfo.symbolName of
    Nothing -> "Defined in (source is unavailable): " <> T.pack (GHC.moduleNameString (GHC.moduleName symbolInfo.definedIn))
    Just defLoc -> "Defined at: " <> renderText defLoc

renderExportedModules :: SymbolInfo -> Text
renderExportedModules symbolInfo =
  "Exported from: " <> renderedModules
  where
    renderModuleName =
      T.pack . GHC.moduleNameString . GHC.moduleName
    renderedModules = case symbolInfo.exportedFrom of
      [] -> "<none>"
      modules -> T.pack (intercalate ", " (map (T.unpack . renderModuleName) modules))

classInstancesSection :: SymbolInfo -> Maybe RenderList
classInstancesSection symbolInfo =
  case NE.nonEmpty symbolInfo.associatedClassInstances of
    Nothing -> Nothing
    Just nonEmptyInstances -> Just (instancesList nonEmptyInstances)
  where
    instancesList neInstances =
      RenderList
        { renderHeader =
            \_ -> Just "Class instances:",
          contentIndentWidth = 2,
          markerStyle = BulletMarker,
          itemsList = fmap CompactClassInstance neInstances,
          skip = 0,
          truncation =
            Just
              Truncation
                { maxItems = 15,
                  itemName = "instances",
                  skipArgName = Nothing
                }
        }
