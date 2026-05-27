module Lore.Tools.Internal.SymbolSuggestions
  ( GroupedSymbolSuggestion (..),
    groupedSymbolSuggestionLabel,
    groupSymbolSuggestions,
    noSymbolsFound,
    quoteText,
  )
where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC.Plugins as Plugins
import Lore (Symbol (..), SymbolSuggestion (..), SymbolVisibility (..))
import Lore.Tools.Render.Ghc (renderOutputable)
import Lore.Tools.Render.Text (quoteText, renderModuleName)

data GroupedSymbolSuggestion = GroupedSymbolSuggestion
  { groupedLookupName :: Text,
    groupedRepresentativeSymbolName :: Text,
    groupedDefiningModules :: [Text]
  }

groupedSymbolSuggestionLabel :: GroupedSymbolSuggestion -> Text
groupedSymbolSuggestionLabel groupedSuggestion =
  case definingModules of
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
  where
    lookupName = groupedSuggestion.groupedLookupName
    symbolName = groupedSuggestion.groupedRepresentativeSymbolName
    definingModules = groupedSuggestion.groupedDefiningModules
    qualifiedSymbolName moduleName = moduleName <> "." <> symbolName

noSymbolsFound :: Text -> Text
noSymbolsFound query =
  "No symbols found for " <> quoteText query <> "."

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
