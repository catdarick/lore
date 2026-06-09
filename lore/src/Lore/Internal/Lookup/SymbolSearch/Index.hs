module Lore.Internal.Lookup.SymbolSearch.Index
  ( buildSymbolSearchIndex,
    buildSymbolSearchDocuments,
    symbolAssociatedModuleNames,
    fieldTokens,
  )
where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified GHC.Types.Name as GHC
import Lore.Internal.Ghc.ValueTypeHead (ValueTypeHeadNames (..), mergeValueTypeHeadNames)
import Lore.Internal.Lookup.Name (NormalizedModuleName, NormalizedName (moduleName, occName), NormalizedOccName, extractAndNormalizeModuleName, normalizeName, unNormalizedModuleName, unNormalizedOccName)
import Lore.Internal.Lookup.SymbolSearch.Tokenize (tokenizeSearchText)
import Lore.Internal.Lookup.SymbolSearch.Types
  ( IndexedNameVariant (..),
    SearchToken,
    SymbolSearchDocument (..),
    SymbolSearchField (..),
    SymbolSearchIndex (..),
  )
import Lore.Internal.Lookup.Types (Symbol (..), SymbolVisibility (..), SymbolsIndex (..), SymbolsMap (..), symbolExportedFrom)

buildSymbolSearchIndex :: SymbolsMap -> SymbolSearchIndex
buildSymbolSearchIndex symbolsMap =
  SymbolSearchIndex
    { searchDocuments = documents,
      searchPostings = postings,
      searchDocumentFrequencies = Map.map (Map.map Set.size) postings,
      searchFieldDocumentCounts = fieldDocumentCounts,
      searchVocabulary = foldMap Map.keysSet (Map.elems postings)
    }
  where
    documents = buildSymbolSearchDocuments symbolsMap
    postings =
      Map.fromList
        [ (field, buildFieldPostings field documents)
        | field <- allFields
        ]
    fieldDocumentCounts =
      Map.fromList
        [ (field, length [() | document <- Map.elems documents, not (Set.null (fieldTokens field document))])
        | field <- allFields
        ]

buildSymbolSearchDocuments :: SymbolsMap -> Map.Map GHC.Name SymbolSearchDocument
buildSymbolSearchDocuments symbolsMap =
  Map.mapWithKey mkDocument groupedEntries
  where
    combinedSymbolsIndex = combineSymbolsIndexes symbolsMap
    groupedEntries =
      Map.fromListWith
        mergeDocumentInput
        [ ( symbol.name,
            DocumentInput
              { inputSymbolName = symbol.name,
                inputVisibility = symbol.visibility,
                inputAliases = symbol.aliases,
                inputNames = Set.singleton lookupName
              }
          )
        | (lookupName, symbols) <- Map.toList combinedSymbolsIndex.symbolsByLookupName,
          symbol <- Set.toList symbols
        ]
    typeHeadsBySymbol = combinedSymbolsIndex.valueTypeHeadNamesBySymbol

    mkDocument symbolName input =
      SymbolSearchDocument
        { symbolSearchSymbol = inputSymbol input,
          symbolSearchNames = indexedNames,
          symbolSearchModules = modules_,
          symbolSearchModuleTokens = Set.fromList (concatMap (tokenizeSearchText . (.unNormalizedModuleName)) (Set.toList modules_)),
          symbolSearchResultTypeTokens = Set.fromList (concatMap tokenizeSearchText (Set.toList typeHeads.resultTypeHeadNames)),
          symbolSearchArgumentTypeTokens = Set.fromList (concatMap tokenizeSearchText (Set.toList typeHeads.argumentTypeHeadNames))
        }
      where
        actualOccName = (normalizeName symbolName).occName
        names = Set.insert actualOccName input.inputNames
        indexedNames =
          case map mkNameVariant (Set.toList names) of
            firstName : remainingNames -> firstName NE.:| remainingNames
            [] -> error "symbol search document requires at least one lookup name"
        modules_ = symbolAssociatedModuleNames (inputSymbol input)
        typeHeads = Map.findWithDefault emptyValueTypeHeadNames symbolName typeHeadsBySymbol

    mkNameVariant name =
      IndexedNameVariant
        { indexedName = name,
          indexedNameTokens = tokenizeSearchText name.unNormalizedOccName
        }

data DocumentInput = DocumentInput
  { inputSymbolName :: GHC.Name,
    inputVisibility :: SymbolVisibility,
    inputAliases :: Set.Set NormalizedOccName,
    inputNames :: Set.Set NormalizedOccName
  }

mergeDocumentInput :: DocumentInput -> DocumentInput -> DocumentInput
mergeDocumentInput new old =
  DocumentInput
    { inputSymbolName = old.inputSymbolName,
      inputVisibility = mergeSymbolVisibility new.inputVisibility old.inputVisibility,
      inputAliases = new.inputAliases <> old.inputAliases,
      inputNames = new.inputNames <> old.inputNames
    }

inputSymbol :: DocumentInput -> Symbol
inputSymbol input =
  Symbol
    { name = input.inputSymbolName,
      visibility = input.inputVisibility,
      aliases = input.inputAliases
    }

mergeSymbolVisibility :: SymbolVisibility -> SymbolVisibility -> SymbolVisibility
mergeSymbolVisibility left right =
  case (left, right) of
    (Symbol'ExportedFrom leftModules, Symbol'ExportedFrom rightModules) ->
      Symbol'ExportedFrom (leftModules <> rightModules)
    (Symbol'ExportedFrom modules_, Symbol'Unexported) ->
      Symbol'ExportedFrom modules_
    (Symbol'Unexported, Symbol'ExportedFrom modules_) ->
      Symbol'ExportedFrom modules_
    (Symbol'Unexported, Symbol'Unexported) ->
      Symbol'Unexported

combineSymbolsIndexes :: SymbolsMap -> SymbolsIndex
combineSymbolsIndexes SymbolsMap {homeSymbolsMap, externalSymbolsMap} =
  SymbolsIndex
    { symbolsByLookupName = Map.unionWith Set.union homeSymbolsMap.symbolsByLookupName externalSymbolsMap.symbolsByLookupName,
      valueTypeHeadNamesBySymbol =
        Map.unionWith
          mergeValueTypeHeadNames
          homeSymbolsMap.valueTypeHeadNamesBySymbol
          externalSymbolsMap.valueTypeHeadNamesBySymbol
    }

buildFieldPostings :: SymbolSearchField -> Map.Map GHC.Name SymbolSearchDocument -> Map.Map SearchToken (Set.Set GHC.Name)
buildFieldPostings field documents =
  Map.fromListWith
    Set.union
    [ (token, Set.singleton symbolName)
    | (symbolName, document) <- Map.toList documents,
      token <- Set.toList (fieldTokens field document)
    ]

fieldTokens :: SymbolSearchField -> SymbolSearchDocument -> Set.Set SearchToken
fieldTokens field document =
  case field of
    SearchName ->
      Set.fromList (concatMap (.indexedNameTokens) (NE.toList document.symbolSearchNames))
    SearchResultType ->
      document.symbolSearchResultTypeTokens
    SearchArgumentType ->
      document.symbolSearchArgumentTypeTokens
    SearchModule ->
      document.symbolSearchModuleTokens

symbolAssociatedModuleNames :: Symbol -> Set.Set NormalizedModuleName
symbolAssociatedModuleNames symbol =
  maybe Set.empty Set.singleton (symbolDefiningModuleName symbol)
    <> Set.map extractAndNormalizeModuleName (symbolExportedFrom symbol)

symbolDefiningModuleName :: Symbol -> Maybe NormalizedModuleName
symbolDefiningModuleName symbol =
  (normalizeName symbol.name).moduleName

emptyValueTypeHeadNames :: ValueTypeHeadNames
emptyValueTypeHeadNames =
  ValueTypeHeadNames
    { argumentTypeHeadNames = Set.empty,
      resultTypeHeadNames = Set.empty
    }

allFields :: [SymbolSearchField]
allFields =
  [SearchName, SearchResultType, SearchArgumentType, SearchModule]
