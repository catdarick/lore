module LookupSearchSpec
  ( spec,
  )
where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC.Plugins as GHC
import qualified GHC.Types.Unique as GHC.Unique
import Lore.Internal.Lookup.Cache.Types (SimilarSymbolSearchKey (..))
import Lore.Internal.Lookup.Name (NormalizedName (occName), NormalizedOccName, parseAndNormalizeName, unNormalizedOccName)
import Lore.Internal.Lookup.Search.Score (searchOccurrences)
import Lore.Internal.Lookup.Search.Types (SearchResult (..), TokenSearchIndex)
import Lore.Internal.Lookup.SymbolsMap (buildSimilarSymbolsSearchIndex, findSimilarSymbolsCandidatesInMap)
import Lore.Internal.Lookup.Types (Symbol (..), SymbolSuggestion (..), SymbolVisibility (..), SymbolsIndex (..), SymbolsMap (..))
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)

spec :: Spec
spec =
  describe "similar symbol search" do
    it "uses defining module context to break same-name ties" do
      suggestionNames (search "createSubscriptionAccount" [subscriptionsCreate, usersCreate])
        `shouldBe` ["Subscriptions.Database.Account.create", "Users.Database.Account.create"]

    it "scores same-named symbols independently without module leakage" do
      (subscriptionsResult, usersResult) <- expectTwoResults (searchResults "createSubscriptionAccount" [subscriptionsCreate, usersCreate])

      (subscriptionsResult.searchResultKey.searchSymbolName == subscriptionsCreate.name) `shouldBe` True
      (usersResult.searchResultKey.searchSymbolName == usersCreate.name) `shouldBe` True
      subscriptionsResult.searchResultScore `shouldSatisfy` (> usersResult.searchResultScore)

    it "does not let unrelated secondary tokens change primary-name scoring statistics" do
      scoreWithUnrelatedOtherModule <- expectScoreFor listMap "mapThing" [listMap, unrelatedOtherModuleThing]
      scoreWithUnrelatedDataMap <- expectScoreFor listMap "mapThing" [listMap, unrelatedDataMapThing]

      scoreWithUnrelatedDataMap `shouldBe` scoreWithUnrelatedOtherModule

    it "counts primary token frequency by lookup name, not same-named symbol count" do
      baselineScore <- expectScoreFor listMap "mapThing" [listMap]
      scoreWithSameLookupName <- expectScoreFor listMap "mapThing" [listMap, otherMap]

      scoreWithSameLookupName `shouldBe` baselineScore

    it "keeps exact primary names stronger than module-assisted short names" do
      suggestionNames (search "createSubscriptionAccount" [subscriptionsCreate, exactCreateSubscriptionAccount])
        `shouldBe` ["Other.Module.createSubscriptionAccount", "Subscriptions.Database.Account.create"]

    it "keeps closer primary names meaningful against weak module context" do
      suggestionNames (search "createSubscriptionAccount" [subscriptionsCreate, usersCreateAccount])
        `shouldBe` ["Users.Database.Account.createAccount", "Subscriptions.Database.Account.create"]

    it "reuses camel-case and plural normalization for secondary module tokens" do
      suggestionNames (search "createSubscriptionAccount" [subscriptionsCreate, usersCreate])
        `shouldBe` ["Subscriptions.Database.Account.create", "Users.Database.Account.create"]

    it "does not penalize long modules with name-extra-token or whole-distance heuristics" do
      shortResult <- expectOneResult (searchResults "createSubscriptionAccount" [subscriptionsCreate])
      longResult <- expectOneResult (searchResults "createSubscriptionAccount" [longSubscriptionsCreate])

      longResult.searchResultScore `shouldBe` shortResult.searchResultScore

    it "does not discover candidates from module tokens only" do
      suggestionNames (search "SubscriptionAccount" [subscriptionsCreate, usersCreate]) `shouldBe` []

    it "preserves defining-module qualified filtering" do
      suggestionNames (search "Subscriptions.Database.Account.create" [subscriptionsCreate, usersCreate])
        `shouldBe` ["Subscriptions.Database.Account.create"]

    it "preserves re-export-module qualified filtering" do
      suggestionNames (search "Public.Api.create" [reexportedCreate, usersCreate])
        `shouldBe` ["Internal.Database.Account.create"]

    it "keeps alias entries independent so the best lookup name can survive deduplication" do
      let alias = lookupOcc "createSubscriptionAccount"
          aliased = subscriptionsCreate {aliases = Set.singleton alias}
          suggestions = search "createSubscriptionAccount" [aliased]

      map (.suggestedLookupName) suggestions `shouldBe` ["createSubscriptionAccount", "create"]
      map ((== aliased.name) . (.suggestedSymbol.name)) suggestions `shouldBe` [True, True]

    it "orders deterministic ties by lookup name" do
      suggestionNames (search "create" [usersCreate, subscriptionsCreate])
        `shouldBe` ["Subscriptions.Database.Account.create", "Users.Database.Account.create"]

search :: Text -> [Symbol] -> [SymbolSuggestion]
search query symbols =
  findSimilarSymbolsCandidatesInMap (parseAndNormalizeName query) (testSearchIndex symbols)

searchResults :: Text -> [Symbol] -> [SearchResult SimilarSymbolSearchKey Symbol]
searchResults query symbols =
  searchOccurrences (queryOccText query) (testSearchIndex symbols)

expectOneResult :: [result] -> IO result
expectOneResult results =
  case results of
    [result] -> pure result
    _ -> expectationFailure "Expected exactly one result." *> fail "unreachable"

expectTwoResults :: [result] -> IO (result, result)
expectTwoResults results =
  case results of
    [firstResult, secondResult] -> pure (firstResult, secondResult)
    _ -> expectationFailure "Expected exactly two results." *> fail "unreachable"

expectScoreFor :: Symbol -> Text -> [Symbol] -> IO Double
expectScoreFor target query symbols =
  case [result.searchResultScore | result <- searchResults query symbols, result.searchResultValue.name == target.name] of
    [score] -> pure score
    _ -> expectationFailure "Expected exactly one result for target symbol." *> fail "unreachable"

testSearchIndex :: [Symbol] -> TokenSearchIndex SimilarSymbolSearchKey Symbol
testSearchIndex symbols =
  buildSimilarSymbolsSearchIndex (symbolsMap symbols)

symbolsMap :: [Symbol] -> SymbolsMap
symbolsMap symbols =
  SymbolsMap
    { homeSymbolsMap = SymbolsIndex (Map.fromListWith Set.union symbolEntries),
      externalSymbolsMap = SymbolsIndex Map.empty
    }
  where
    symbolEntries =
      concat
        [ (lookupName, Set.singleton symbol) : [(alias, Set.singleton symbol) | alias <- Set.toList symbol.aliases]
        | symbol <- symbols,
          let lookupName = lookupOcc (GHC.occNameString (GHC.nameOccName symbol.name))
        ]

queryOccText :: Text -> Text
queryOccText query =
  unNormalizedOccName (parseAndNormalizeName query).occName

lookupOcc :: String -> NormalizedOccName
lookupOcc text =
  (parseAndNormalizeName (T.pack text)).occName

suggestionNames :: [SymbolSuggestion] -> [Text]
suggestionNames =
  map (qualifiedSymbolName . (.suggestedSymbol))

qualifiedSymbolName :: Symbol -> Text
qualifiedSymbolName symbol =
  T.pack (GHC.moduleNameString (GHC.moduleName (GHC.nameModule symbol.name)) <> "." <> GHC.occNameString (GHC.nameOccName symbol.name))

subscriptionsCreate :: Symbol
subscriptionsCreate =
  testSymbol 1 "Subscriptions.Database.Account" "create"

longSubscriptionsCreate :: Symbol
longSubscriptionsCreate =
  testSymbol 2 "Subscriptions.Database.Internal.Persistence.Account" "create"

usersCreate :: Symbol
usersCreate =
  testSymbol 3 "Users.Database.Account" "create"

usersCreateAccount :: Symbol
usersCreateAccount =
  testSymbol 4 "Users.Database.Account" "createAccount"

exactCreateSubscriptionAccount :: Symbol
exactCreateSubscriptionAccount =
  testSymbol 5 "Other.Module" "createSubscriptionAccount"

reexportedCreate :: Symbol
reexportedCreate =
  (testSymbol 6 "Internal.Database.Account" "create")
    { visibility = Symbol'ExportedFrom (Set.singleton (testModule "Public.Api"))
    }

listMap :: Symbol
listMap =
  testSymbol 7 "Data.List" "map"

unrelatedOtherModuleThing :: Symbol
unrelatedOtherModuleThing =
  testSymbol 8 "Other.Module" "unrelated"

unrelatedDataMapThing :: Symbol
unrelatedDataMapThing =
  testSymbol 9 "Data.Map" "unrelated"

otherMap :: Symbol
otherMap =
  testSymbol 10 "Other.Module" "map"

testSymbol :: Int -> String -> String -> Symbol
testSymbol unique moduleName occName =
  Symbol
    { name = GHC.mkExternalName (GHC.Unique.mkUniqueGrimily unique) (testModule moduleName) (GHC.mkVarOcc occName) GHC.noSrcSpan,
      visibility = Symbol'ExportedFrom (Set.singleton (testModule moduleName)),
      aliases = Set.empty
    }

testModule :: String -> GHC.Module
testModule moduleName =
  GHC.mkModule GHC.mainUnit (GHC.mkModuleName moduleName)
