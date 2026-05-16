module Lore.Mcp.Tools.Shared.SymbolSuggestions
  ( maxRenderedSymbolSuggestions,
    maxSearchSymbolSuggestions,
    symbolSuggestionFetchLimit,
    renderMissingSymbol,
    renderSearchSymbolResults,
  )
where

import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC.Plugins as Plugins
import Lore (Symbol (..), SymbolSuggestion (..), SymbolVisibility (..))
import Lore.Mcp.Internal.Render
  ( ListMarker (..),
    RenderList (..),
    Renderable (..),
    totalItems,
  )
import Lore.Mcp.Tools.Shared.Outputable (renderOutputable)

maxRenderedSymbolSuggestions :: Int
maxRenderedSymbolSuggestions = 10

maxSearchSymbolSuggestions :: Int
maxSearchSymbolSuggestions = 10

symbolSuggestionFetchLimit :: Int
symbolSuggestionFetchLimit =
  maxRenderedSymbolSuggestions * 20

data GroupedSymbolSuggestion = GroupedSymbolSuggestion
  { groupedLookupName :: Text,
    groupedRepresentativeSymbolName :: Text,
    groupedDefiningModules :: [Text]
  }

newtype RenderedGroupedSuggestion = RenderedGroupedSuggestion GroupedSymbolSuggestion

instance Renderable RenderedGroupedSuggestion where
  renderText (RenderedGroupedSuggestion groupedSuggestion) =
    let lookupName = groupedSuggestion.groupedLookupName
        symbolName = groupedSuggestion.groupedRepresentativeSymbolName
        definingModules = groupedSuggestion.groupedDefiningModules
        qualifiedSymbolName moduleName = moduleName <> "." <> symbolName
     in case definingModules of
          [moduleName] ->
            if lookupName == symbolName
              then qualifiedSymbolName moduleName
              else lookupName <> " (" <> qualifiedSymbolName moduleName <> ")"
          _ ->
            let baseLabel =
                  if lookupName == symbolName
                    then symbolName
                    else lookupName <> " (" <> symbolName <> ")"
             in baseLabel <> renderDefiningModulesSummary definingModules

renderMissingSymbol :: Text -> [SymbolSuggestion] -> Text
renderMissingSymbol query suggestions =
  case NE.nonEmpty (map RenderedGroupedSuggestion (take maxRenderedSymbolSuggestions (groupSymbolSuggestions suggestions))) of
    Nothing ->
      noSymbolsFound query
    Just nonEmptySuggestions ->
      renderText $
        RenderList
          { renderHeader = \_ -> Just $ noSymbolsFound query <> " Maybe you meant one of these?",
            contentIndentWidth = 0,
            markerStyle = NumberMarker,
            itemsList = nonEmptySuggestions,
            skip = 0,
            truncation = Nothing
          }

renderSearchSymbolResults :: Text -> [SymbolSuggestion] -> Text
renderSearchSymbolResults query suggestions =
  case NE.nonEmpty (map RenderedGroupedSuggestion (take maxSearchSymbolSuggestions (groupSymbolSuggestions suggestions))) of
    Nothing ->
      noSymbolsFound query
    Just nonEmptySuggestions ->
      renderText $
        RenderList
          { renderHeader =
              \ctx ->
                Just $
                  "Found "
                    <> T.pack (show ctx.totalItems)
                    <> " similar symbols for "
                    <> quoteText query
                    <> ":",
            contentIndentWidth = 0,
            markerStyle = NumberMarker,
            itemsList = nonEmptySuggestions,
            skip = 0,
            truncation = Nothing
          }

noSymbolsFound :: Text -> Text
noSymbolsFound query =
  "No symbols found for " <> quoteText query <> "."

quoteText :: Text -> Text
quoteText value =
  "\"" <> value <> "\""

renderDefiningModulesSummary :: [Text] -> Text
renderDefiningModulesSummary moduleNames =
  case moduleNames of
    [] ->
      ""
    [moduleName] ->
      " (defined in: " <> moduleName <> ")"
    [firstModule, secondModule] ->
      " (defined in: " <> firstModule <> ", " <> secondModule <> ")"
    firstModule : secondModule : remainingModules ->
      " (defined in: " <> firstModule <> ", " <> secondModule <> " and " <> T.pack (show (length remainingModules)) <> " other modules)"

symbolDefiningModules :: Symbol -> [Text]
symbolDefiningModules symbol =
  case Plugins.nameModule_maybe symbol.name of
    Just definingModule ->
      [renderModuleName definingModule]
    Nothing ->
      case symbol.visibility of
        Symbol'ExportedFrom modules_ ->
          case shortestModuleName (map renderModuleName (Set.toList modules_)) of
            Just moduleName -> [moduleName]
            Nothing -> []
        Symbol'Unexported ->
          []

renderModuleName :: Plugins.Module -> Text
renderModuleName module_ =
  T.pack (Plugins.moduleNameString (Plugins.moduleName module_))

shortestModuleName :: [Text] -> Maybe Text
shortestModuleName [] = Nothing
shortestModuleName names =
  Just $
    List.minimumBy
      (\left right -> compare (T.length left, left) (T.length right, right))
      names

groupSymbolSuggestions :: [SymbolSuggestion] -> [GroupedSymbolSuggestion]
groupSymbolSuggestions suggestions =
  [ groupedByLookupNameKey Map.! lookupNameKey
  | lookupNameKey <- reverse lookupOrder
  ]
  where
    (lookupOrder, groupedByLookupNameKey) =
      List.foldl' collectGroupedSuggestion ([], Map.empty) suggestions

    collectGroupedSuggestion (order, groupedMap) suggestion =
      let lookupName = suggestion.suggestedLookupName
          lookupNameKey = canonicalLookupNameKey lookupName
       in case Map.lookup lookupNameKey groupedMap of
            Nothing ->
              ( lookupNameKey : order,
                Map.insert
                  lookupNameKey
                  ( mkGroupedSuggestion
                      suggestion
                      (symbolDefiningModules suggestion.suggestedSymbol)
                  )
                  groupedMap
              )
            Just groupedSuggestion ->
              ( order,
                Map.insert
                  lookupNameKey
                  ( groupedSuggestion
                      { groupedDefiningModules =
                          dedupeModulesPreservingOrder
                            ( groupedSuggestion.groupedDefiningModules
                                <> symbolDefiningModules suggestion.suggestedSymbol
                            )
                      }
                  )
                  groupedMap
              )

    mkGroupedSuggestion suggestion definingModules =
      GroupedSymbolSuggestion
        { groupedLookupName = suggestion.suggestedLookupName,
          groupedRepresentativeSymbolName = renderOutputable suggestion.suggestedSymbol.name,
          groupedDefiningModules = dedupeModulesPreservingOrder definingModules
        }

canonicalLookupNameKey :: Text -> Text
canonicalLookupNameKey =
  T.toLower

dedupeModulesPreservingOrder :: [Text] -> [Text]
dedupeModulesPreservingOrder modules_ =
  reverse kept
  where
    (_, kept) =
      List.foldl' keepUnique (Set.empty, []) modules_

    keepUnique (seen, accumulated) moduleName
      | moduleName `Set.member` seen =
          (seen, accumulated)
      | otherwise =
          (Set.insert moduleName seen, moduleName : accumulated)
