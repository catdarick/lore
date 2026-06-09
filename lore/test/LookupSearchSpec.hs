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
import Lore.Internal.Ghc.ValueTypeHead (ValueTypeHeadNames (..))
import Lore.Internal.Lookup.Cache.Types (SimilarSymbolSearchKey (..))
import Lore.Internal.Lookup.ModulePattern (ModulePattern, compileModulePattern)
import Lore.Internal.Lookup.Name (NormalizedName (occName), NormalizedOccName, parseAndNormalizeName, unNormalizedOccName)
import Lore.Internal.Lookup.Search.Score (buildSearchIndex, searchOccurrences)
import Lore.Internal.Lookup.Search.Types (SearchDocument (..), SearchResult (..), TokenSearchIndex)
import Lore.Internal.Lookup.SymbolsMap (buildSimilarSymbolsSearchIndex, findSimilarSymbolsCandidatesInMap)
import Lore.Internal.Lookup.Types (Symbol (..), SymbolSuggestion (..), SymbolVisibility (..), SymbolsIndex (..), SymbolsMap (..))
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldMatchList, shouldSatisfy)

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

    it "lets any module pattern qualify a symbol" do
      suggestionNames (searchWithPatterns ["Missing.*", "Users.*"] "create" [subscriptionsCreate, usersCreate])
        `shouldBe` ["Users.Database.Account.create"]

    it "lets any associated module qualify a symbol" do
      suggestionNames (searchWithPatterns ["Public.*"] "create" [reexportedCreate, usersCreate])
        `shouldBe` ["Internal.Database.Account.create"]

    it "combines qualified query modules and module patterns with AND semantics" do
      suggestionNames (searchWithPatterns ["Public.*"] "Public.Api.create" [reexportedCreate, usersCreate])
        `shouldBe` ["Internal.Database.Account.create"]

      suggestionNames (searchWithPatterns ["Users.*", "Missing.*"] "Public.Api.create" [reexportedCreate, usersCreate])
        `shouldBe` []

      suggestionNames (searchWithPatterns ["Missing.*", "Public.*"] "Public.Api.create" [reexportedCreate, usersCreate])
        `shouldBe` ["Internal.Database.Account.create"]

    it "keeps empty module patterns unrestricted" do
      suggestionNames (searchWithPatterns [] "create" [subscriptionsCreate, usersCreate])
        `shouldBe` suggestionNames (search "create" [subscriptionsCreate, usersCreate])

    it "keeps out-of-scope high-ranked results from consuming the requested result budget" do
      take 2 (suggestionNames (searchWithPatterns ["Public.*"] "createUser" [exactPrivateCreateUser, exactOtherCreateUser, publicCreateUserRecord, publicCreateTestUser]))
        `shouldMatchList` ["Public.Api.createUserRecord", "Public.Api.createTestUser"]

    it "keeps alias entries independent so the best lookup name can survive deduplication" do
      let alias = lookupOcc "createSubscriptionAccount"
          aliased = subscriptionsCreate {aliases = Set.singleton alias}
          suggestions = search "createSubscriptionAccount" [aliased]

      map (.suggestedLookupName) suggestions `shouldBe` ["createSubscriptionAccount", "create"]
      map ((== aliased.name) . (.suggestedSymbol.name)) suggestions `shouldBe` [True, True]

    it "orders deterministic ties by lookup name" do
      suggestionNames (search "create" [usersCreate, subscriptionsCreate])
        `shouldBe` ["Subscriptions.Database.Account.create", "Users.Database.Account.create"]

    it "uses result type context without module assistance" do
      suggestionNames (searchWithTypeFacts "createDiscountAccount" [subscriptionsCreateShort, usersCreateShort] discountResultFacts)
        `shouldBe` ["Subscriptions.create", "Users.create"]

    it "keeps exact primary names stronger than result type context" do
      suggestionNames (searchWithTypeFacts "createDiscountAccount" [subscriptionsCreateShort, exactCreateDiscountAccount] discountResultFacts)
        `shouldBe` ["Other.Module.createDiscountAccount", "Subscriptions.create"]

    it "uses argument type context when result type context is absent" do
      suggestionNames (searchWithTypeFacts "createDiscountAccount" [subscriptionsCreateShort, usersCreateShort] discountArgumentFacts)
        `shouldBe` ["Subscriptions.create", "Users.create"]

    it "weights result type context above argument type context when they conflict" do
      suggestionNames (searchWithTypeFacts "createDiscountAccount" [subscriptionsCreateShort, usersCreateShort] conflictingDiscountFacts)
        `shouldBe` ["Subscriptions.create", "Users.create"]

    it "does not discover candidates from type tokens only" do
      suggestionNames (searchWithTypeFacts "DiscountAccount" [subscriptionsCreateShort, usersCreateShort] discountResultFacts)
        `shouldBe` []

    it "keeps same-named symbols on independent type contexts" do
      (firstResult, secondResult) <- expectTwoResults (searchResultsWithTypeFacts "createDiscountAccount" [subscriptionsCreateShort, usersCreateShort] discountResultFacts)

      (firstResult.searchResultValue.name == subscriptionsCreateShort.name) `shouldBe` True
      (secondResult.searchResultValue.name == usersCreateShort.name) `shouldBe` True
      firstResult.searchResultScore `shouldSatisfy` (> secondResult.searchResultScore)

    it "keeps exact common tokens above rare canonical approximations" do
      let results =
            documentSearchResults
              "createUser"
              ( [ "createUserRecord",
                  "createsUserRecord"
                ]
                  <> createFrequencyFixtures
              )

      scoreOfDocument "createUserRecord" results
        `shouldSatisfy` (> scoreOfDocument "createsUserRecord" results)

    it "keeps reported create-user candidates below closer exact-token names" do
      let results =
            documentSearchResults
              "createUser"
              [ "createUser",
                "createUserRecord",
                "interruptCreatesUserInterruptHitlSpec",
                "createTestUsers",
                "manuallyCreateUsersEndpoint",
                "createFromUser",
                "createUserName"
              ]

      rankOfDocument "createUser" results `shouldBe` 1
      rankOfDocument "createUserRecord" results
        `shouldSatisfy` (< rankOfDocument "interruptCreatesUserInterruptHitlSpec" results)
      rankOfDocument "createUserRecord" results
        `shouldSatisfy` (< rankOfDocument "manuallyCreateUsersEndpoint" results)

    it "keeps exact common tokens above rare fuzzy approximations" do
      let results =
            documentSearchResults
              "createUser"
              ( [ "createUserRecord",
                  "cretaeUserRecord"
                ]
                  <> createFrequencyFixtures
              )

      scoreOfDocument "createUserRecord" results
        `shouldSatisfy` (> scoreOfDocument "cretaeUserRecord" results)

    it "keeps generic extra-token penalties stronger for narrower names" do
      let results =
            documentSearchResults
              "createUser"
              [ "createUserRecord",
                "createUserBankAccountRecord"
              ]

      rankOfDocument "createUserRecord" results
        `shouldSatisfy` (< rankOfDocument "createUserBankAccountRecord" results)

search :: Text -> [Symbol] -> [SymbolSuggestion]
search query symbols =
  findSimilarSymbolsCandidatesInMap [] (parseAndNormalizeName query) (testSearchIndex symbols)

searchWithPatterns :: [Text] -> Text -> [Symbol] -> [SymbolSuggestion]
searchWithPatterns rawPatterns query symbols =
  findSimilarSymbolsCandidatesInMap (map compilePattern rawPatterns) (parseAndNormalizeName query) (testSearchIndex symbols)

searchWithTypeFacts :: Text -> [Symbol] -> Map.Map GHC.Name ValueTypeHeadNames -> [SymbolSuggestion]
searchWithTypeFacts query symbols typeFacts =
  findSimilarSymbolsCandidatesInMap [] (parseAndNormalizeName query) (testSearchIndexWithTypeFacts symbols typeFacts)

compilePattern :: Text -> ModulePattern
compilePattern rawPattern =
  case compileModulePattern rawPattern of
    Right pattern' -> pattern'
    Left _ -> error ("Expected valid module pattern " <> T.unpack rawPattern)

searchResults :: Text -> [Symbol] -> [SearchResult SimilarSymbolSearchKey Symbol]
searchResults query symbols =
  searchOccurrences (queryOccText query) (testSearchIndex symbols)

searchResultsWithTypeFacts :: Text -> [Symbol] -> Map.Map GHC.Name ValueTypeHeadNames -> [SearchResult SimilarSymbolSearchKey Symbol]
searchResultsWithTypeFacts query symbols typeFacts =
  searchOccurrences (queryOccText query) (testSearchIndexWithTypeFacts symbols typeFacts)

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

documentSearchResults :: Text -> [Text] -> [SearchResult Text Text]
documentSearchResults query documents =
  searchOccurrences query $
    buildSearchIndex
      [ (document, SearchDocument {primaryText = document, contextTexts = Map.empty}, document)
      | document <- documents
      ]

scoreOfDocument :: Text -> [SearchResult Text Text] -> Double
scoreOfDocument document results =
  case [result.searchResultScore | result <- results, result.searchResultValue == document] of
    [score] -> score
    _ -> error ("Expected exactly one score for document " <> T.unpack document)

rankOfDocument :: Text -> [SearchResult Text Text] -> Int
rankOfDocument document results =
  case [rank | (rank, result) <- zip [1 ..] results, result.searchResultValue == document] of
    [rank] -> rank
    _ -> error ("Expected exactly one rank for document " <> T.unpack document)

createFrequencyFixtures :: [Text]
createFrequencyFixtures =
  [ "createAccount",
    "createProject",
    "createSession",
    "createInvoice",
    "createReport",
    "createToken"
  ]

testSearchIndex :: [Symbol] -> TokenSearchIndex SimilarSymbolSearchKey Symbol
testSearchIndex symbols =
  buildSimilarSymbolsSearchIndex (symbolsMap symbols)

testSearchIndexWithTypeFacts :: [Symbol] -> Map.Map GHC.Name ValueTypeHeadNames -> TokenSearchIndex SimilarSymbolSearchKey Symbol
testSearchIndexWithTypeFacts symbols typeFacts =
  buildSimilarSymbolsSearchIndex (symbolsMapWithTypeFacts symbols typeFacts)

symbolsMap :: [Symbol] -> SymbolsMap
symbolsMap symbols =
  symbolsMapWithTypeFacts symbols Map.empty

symbolsMapWithTypeFacts :: [Symbol] -> Map.Map GHC.Name ValueTypeHeadNames -> SymbolsMap
symbolsMapWithTypeFacts symbols typeFacts =
  SymbolsMap
    { homeSymbolsMap =
        SymbolsIndex
          { symbolsByLookupName = Map.fromListWith Set.union symbolEntries,
            valueTypeHeadNamesBySymbol = typeFacts
          },
      externalSymbolsMap =
        SymbolsIndex
          { symbolsByLookupName = Map.empty,
            valueTypeHeadNamesBySymbol = Map.empty
          }
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

subscriptionsCreateShort :: Symbol
subscriptionsCreateShort =
  testSymbol 11 "Subscriptions" "create"

usersCreateShort :: Symbol
usersCreateShort =
  testSymbol 12 "Users" "create"

exactCreateDiscountAccount :: Symbol
exactCreateDiscountAccount =
  testSymbol 13 "Other.Module" "createDiscountAccount"

exactPrivateCreateUser :: Symbol
exactPrivateCreateUser =
  testSymbol 14 "Private.Internal" "createUser"

exactOtherCreateUser :: Symbol
exactOtherCreateUser =
  testSymbol 15 "Other.Internal" "createUser"

publicCreateUserRecord :: Symbol
publicCreateUserRecord =
  testSymbol 16 "Public.Api" "createUserRecord"

publicCreateTestUser :: Symbol
publicCreateTestUser =
  testSymbol 17 "Public.Api" "createTestUser"

discountResultFacts :: Map.Map GHC.Name ValueTypeHeadNames
discountResultFacts =
  Map.fromList
    [ (subscriptionsCreateShort.name, valueTypeHeads [] ["DiscountAccount"]),
      (usersCreateShort.name, valueTypeHeads [] ["UserAccount"])
    ]

discountArgumentFacts :: Map.Map GHC.Name ValueTypeHeadNames
discountArgumentFacts =
  Map.fromList
    [ (subscriptionsCreateShort.name, valueTypeHeads ["DiscountAccount"] ["Result"]),
      (usersCreateShort.name, valueTypeHeads ["UserAccount"] ["Result"])
    ]

conflictingDiscountFacts :: Map.Map GHC.Name ValueTypeHeadNames
conflictingDiscountFacts =
  Map.fromList
    [ (subscriptionsCreateShort.name, valueTypeHeads ["UserAccount"] ["DiscountAccount"]),
      (usersCreateShort.name, valueTypeHeads ["DiscountAccount"] ["UserAccount"])
    ]

valueTypeHeads :: [Text] -> [Text] -> ValueTypeHeadNames
valueTypeHeads argumentNames resultNames =
  ValueTypeHeadNames
    { argumentTypeHeadNames = Set.fromList argumentNames,
      resultTypeHeadNames = Set.fromList resultNames
    }

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
