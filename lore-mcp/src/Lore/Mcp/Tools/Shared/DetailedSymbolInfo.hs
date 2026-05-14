module Lore.Mcp.Tools.Shared.DetailedSymbolInfo
  ( DetailedSymbolInfo (..),
  )
where

import Data.List (intercalate)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC
import qualified GHC.Iface.Syntax as Iface
import qualified GHC.Types.TyThing as TyThing
import qualified GHC.Types.TyThing.Ppr as TyThing
import Lore (Instances (..), SymbolInfo (..), SymbolVisibility (..))
import Lore.Mcp.Internal.Render (Renderable (renderText), indented, (|>))
import Lore.Mcp.Tools.Shared.CompactClassInstance (CompactClassInstance (CompactClassInstance))
import Lore.Mcp.Tools.Shared.DefinitionLocation (mkDefinitionLocation)
import Lore.Mcp.Tools.Shared.Outputable (renderOutputable, renderOutputableWith)

data DetailedSymbolInfo = DetailedSymbolInfo
  { symbolInfo :: SymbolInfo,
    instancesInfo :: Instances
  }

instance Renderable DetailedSymbolInfo where
  renderText detailedSymbolInfo =
    renderText $
      renderSymbolHeader symbolInfo
        |> indented
          ( definitionLocation symbolInfo
              |> renderExportedModules symbolInfo
              |> classInstancesSection symbolInfo instancesInfo
          )
    where
      symbolInfo = detailedSymbolInfo.symbolInfo
      instancesInfo = detailedSymbolInfo.instancesInfo

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
    renderedModules =
      case symbolInfo.visibility of
        Symbol'Unexported -> "<none>"
        Symbol'ExportedFrom modules ->
          T.pack (intercalate ", " (map (T.unpack . renderModuleName) (Set.toList modules)))

classInstancesSection :: SymbolInfo -> Instances -> Maybe Text
classInstancesSection symbolInfo instancesInfo =
  case instancesInfo.classInstances of
    [] -> Nothing
    classInstances ->
      Just
        ( "Class instances: "
            <> T.intercalate ", " (map (renderText . CompactClassInstance symbolInfo) displayedInstances)
            <> overflowSuffix overflowCount
        )
      where
        displayedInstances = take maxDisplayedClassInstances classInstances
        overflowCount = length classInstances - length displayedInstances

maxDisplayedClassInstances :: Int
maxDisplayedClassInstances = 20

overflowSuffix :: Int -> Text
overflowSuffix overflowCount
  | overflowCount <= 0 = ""
  | otherwise = " ... and " <> T.pack (show overflowCount) <> " more"
