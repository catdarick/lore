module LookupSearchSpec
  ( spec,
  )
where

import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC.Plugins as GHC
import qualified GHC.Types.Unique as GHC.Unique
import Lore.Internal.Ghc.ValueTypeHead (ValueTypeHeadNames (..))
import Lore.Internal.Lookup.ModulePattern (ModulePattern, compileModulePattern)
import Lore.Internal.Lookup.Name (NormalizedName (occName), NormalizedOccName, parseAndNormalizeName, unNormalizedModuleName)
import Lore.Internal.Lookup.SymbolSearch.Index (buildSymbolSearchIndex)
import Lore.Internal.Lookup.SymbolSearch.Rank (parseSymbolSearchQuery, tokenIdf)
import Lore.Internal.Lookup.SymbolSearch.Synonyms (SynonymLexicon, SynonymTerm (..), SynonymTermError (..), builtInSynonymLexicon, compileSynonymGroups, compileSynonymTerm, directSynonyms, mergeSynonymLexicons)
import Lore.Internal.Lookup.SymbolSearch.Types
  ( IndexedNameVariant (..),
    SearchToken (..),
    SymbolSearchDocument (..),
    SymbolSearchField (..),
    SymbolSearchIndex (..),
    SymbolSearchQuery (symbolSearchExactModule, symbolSearchTokens),
    TermMatchEvidence (..),
    TokenMatchKind (..),
    TokenSpan (..),
  )
import Lore.Internal.Lookup.SymbolsMap (findSimilarSymbolsCandidatesInMap)
import Lore.Internal.Lookup.Types (Symbol (..), SymbolSuggestion (..), SymbolVisibility (..), SymbolsIndex (..), SymbolsMap (..))
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldMatchList, shouldSatisfy)

spec :: Spec
spec =
  describe "symbol search" do
    it "retrieves a symbol by an argument type token with no name token match" do
      suggestionNames (searchWithTypeFacts "ByteString" [decodePayload] (typeFacts [(decodePayload, ["ByteString"], ["Payload"])]))
        `shouldBe` ["Payload.Codec.decodePayload"]

    it "retrieves a symbol by a result type token with no name token match" do
      suggestionNames (searchWithTypeFacts "ValidationResult" [decodePayload] (typeFacts [(decodePayload, ["Input"], ["ValidationResult"])]))
        `shouldBe` ["Payload.Codec.decodePayload"]

    it "retrieves a symbol by an associated module token with no name or type token match" do
      suggestionNames (search "Argyle" [argyleRequest])
        `shouldBe` ["ExternalProviders.Argyle.Client.request"]

    it "keeps exact qualified modules as hard scope filters" do
      suggestionNames (search "Other.Module.Argyle.request" [argyleRequest])
        `shouldBe` []

      suggestionNames (search "ExternalProviders.Argyle.Client.Argyle" [argyleRequest])
        `shouldBe` ["ExternalProviders.Argyle.Client.request"]

    it "keeps module patterns as OR hard scope filters" do
      suggestionNames (searchWithPatterns ["Missing.*", "ExternalProviders.*"] "Argyle" [argyleRequest])
        `shouldBe` ["ExternalProviders.Argyle.Client.request"]

      suggestionNames (searchWithPatterns ["Missing.*", "Other.*"] "Argyle" [argyleRequest])
        `shouldBe` []

    it "uses direct synonyms without transitive synonym expansion" do
      areDirectSynonyms builtInSynonymLexicon (term ["query"]) (term ["select"]) `shouldBe` True
      areDirectSynonyms builtInSynonymLexicon (term ["select"]) (term ["filter"]) `shouldBe` True
      areDirectSynonyms builtInSynonymLexicon (term ["query"]) (term ["filter"]) `shouldBe` False

    it "applies direct synonyms in full search only for direct neighbors" do
      suggestionNames (search "db" [databaseConnect])
        `shouldBe` ["Storage.Database.connect"]

      suggestionNames (search "query" [filterRows])
        `shouldBe` []

    it "uses built-in phrase synonyms for formerly collapsed compounds" do
      suggestionLookupNames (search "ddd" [domainDrivenDesign])
        `shouldBe` ["domainDrivenDesign"]

    it "applies project synonym groups without replacing built-ins" do
      projectLexicon <- expectRight "Expected project synonym group to compile." (compileSynonymGroups [["enqueue", "schedule"]])
      let effectiveLexicon = mergeSynonymLexicons builtInSynonymLexicon projectLexicon

      suggestionNames (searchWithLexicon projectLexicon "enqueue" [scheduleJob])
        `shouldBe` ["Jobs.scheduleJob"]
      suggestionNames (searchWithLexicon mempty "enqueue" [scheduleJob])
        `shouldBe` []
      suggestionNames (searchWithLexicon effectiveLexicon "db" [databaseConnect])
        `shouldBe` ["Storage.Database.connect"]

    it "keeps overlapping project synonym groups direct and non-transitive" do
      projectLexicon <- expectRight "Expected project synonym groups to compile." (compileSynonymGroups [["alpha", "beta"], ["beta", "gamma"], ["query", "projection"]])
      let effectiveLexicon = mergeSynonymLexicons builtInSynonymLexicon projectLexicon

      areDirectSynonyms projectLexicon (term ["alpha"]) (term ["beta"]) `shouldBe` True
      areDirectSynonyms projectLexicon (term ["beta"]) (term ["gamma"]) `shouldBe` True
      areDirectSynonyms projectLexicon (term ["alpha"]) (term ["gamma"]) `shouldBe` False
      areDirectSynonyms effectiveLexicon (term ["projection"]) (term ["query"]) `shouldBe` True
      areDirectSynonyms effectiveLexicon (term ["query"]) (term ["lookup"]) `shouldBe` True
      areDirectSynonyms effectiveLexicon (term ["projection"]) (term ["lookup"]) `shouldBe` False

    it "normalizes multi-token synonym terms without collapsing token boundaries" do
      compileSynonymTerm "RocketShip" `shouldBe` Right (term ["rocket", "ship"])
      compileSynonymTerm "rocket ship" `shouldBe` Right (term ["rocket", "ship"])
      compileSynonymTerm "rocket-ship" `shouldBe` Right (term ["rocket", "ship"])
      compileSynonymTerm "rocket_ship" `shouldBe` Right (term ["rocket", "ship"])
      compileSynonymTerm "SB" `shouldBe` Right (term ["sb"])
      compileSynonymTerm "" `shouldBe` Left (SynonymTermProducesNoTokens "")

    it "applies multi-token project synonyms bidirectionally and atomically" do
      projectLexicon <- expectRight "Expected phrase synonym groups to compile." (compileSynonymGroups [["RocketShip", "Beacon"], ["SignalBridge", "SB"]])

      suggestionLookupNames (searchWithLexicon projectLexicon "Beacon" [rocketShip])
        `shouldBe` ["RocketShip"]
      suggestionLookupNames (searchWithLexicon projectLexicon "RocketShip" [beacon])
        `shouldBe` ["Beacon"]
      suggestionLookupNames (searchWithLexicon projectLexicon "SB" [signalBridge])
        `shouldBe` ["SignalBridge"]
      suggestionLookupNames (searchWithLexicon projectLexicon "SignalBridge" [sb])
        `shouldBe` ["SB"]
      suggestionLookupNames (searchWithLexicon projectLexicon "createBeacon" [createRocketShip])
        `shouldBe` ["createRocketShip"]
      suggestionLookupNames (searchWithLexicon projectLexicon "createRocketShip" [createBeacon])
        `shouldBe` ["createBeacon"]
      suggestionLookupNames (searchWithLexicon projectLexicon "Beacon" [rocketCount, shipInvoice])
        `shouldBe` []

    it "requires synonym phrase occurrences to be ordered and contiguous within one value" do
      projectLexicon <- expectRight "Expected phrase synonym group to compile." (compileSynonymGroups [["RocketShip", "Beacon"]])

      suggestionLookupNames (searchWithLexicon projectLexicon "Beacon" [createRocketShipRequest])
        `shouldBe` ["createRocketShipRequest"]
      suggestionLookupNames (searchWithLexicon projectLexicon "Beacon" [createRocketLaterShipRequest])
        `shouldBe` []

      let splitAliases =
            splitAliasSymbol
              { aliases = Set.fromList [lookupOcc "rocket", lookupOcc "ship"]
              }
      suggestionLookupNames (searchWithLexicon projectLexicon "Beacon" [splitAliases])
        `shouldBe` []

    it "does not combine synonym phrase tokens across context values or fields" do
      projectLexicon <- expectRight "Expected phrase synonym group to compile." (compileSynonymGroups [["RocketShip", "Beacon"]])
      let splitModules =
            rocketOnly
              { visibility = Symbol'ExportedFrom (Set.singleton (testModule "Ship"))
              }

      suggestionLookupNames (searchWithLexicon projectLexicon "Beacon" [splitModules])
        `shouldBe` []
      suggestionLookupNames (findSimilarSymbolsCandidatesInMap projectLexicon [] "Beacon" (testSearchIndexWithTypeFacts [placeholder] (typeFacts [(placeholder, ["Rocket", "Ship"], [])])))
        `shouldBe` []
      suggestionLookupNames (searchWithLexicon projectLexicon "Beacon" [nameModuleSplit])
        `shouldBe` []

    it "does not multiply score for repeated one-token synonym query terms" do
      let oneToken = search "db" [databaseConnect]
          repeated = search "db db" [databaseConnect]

      suggestionLookupNames repeated `shouldBe` suggestionLookupNames oneToken
      map (.suggestionScore) repeated `shouldBe` map (.suggestionScore) oneToken
      map (length . (.suggestionEvidence)) repeated `shouldBe` map (length . (.suggestionEvidence)) oneToken

    it "selects repeated stored token occurrences that preserve query order" do
      nameEvidenceStoredStarts "foo bar" fooBarFoo `shouldBe` [0, 1]
      nameEvidenceStoredStarts "bar foo" fooBarFoo `shouldBe` [1, 2]

    it "indexes one document per actual symbol including all aliases" do
      let alias = lookupOcc "createSubscriptionAccount"
          aliased = subscriptionsCreate {aliases = Set.singleton alias}
          index = testSearchIndex [aliased]
          document = expectDocument aliased index

      Map.size index.searchDocuments `shouldBe` 1
      map (.indexedName) (NE.toList document.symbolSearchNames) `shouldMatchList` [lookupOcc "create", alias]
      suggestionLookupNames (search "createSubscriptionAccount" [aliased]) `shouldBe` ["createSubscriptionAccount"]

    it "merges symbol metadata across aliases for one document" do
      let exportedFromA = sharedLookup {visibility = Symbol'ExportedFrom (Set.singleton (testModule "Public.A")), aliases = Set.singleton (lookupOcc "lookupA")}
          exportedFromB = sharedLookup {visibility = Symbol'ExportedFrom (Set.singleton (testModule "Public.B")), aliases = Set.singleton (lookupOcc "lookupB")}
          index = testSearchIndex [exportedFromA, exportedFromB]
          document = expectDocument sharedLookup index

      moduleTexts document `shouldMatchList` ["Internal.Lookup", "Public.A", "Public.B"]
      suggestionNames (searchWithPatterns ["Public.B"] "lookup" [exportedFromA, exportedFromB])
        `shouldBe` ["Internal.Lookup.lookup"]

    it "counts field document frequency once per symbol despite aliases" do
      let aliasA = lookupOcc "createAccount"
          aliasB = lookupOcc "createUserAccount"
          aliased = subscriptionsCreate {aliases = Set.fromList [aliasA, aliasB]}
          index = testSearchIndexWithTypeFacts [aliased] (typeFacts [(aliased, ["ByteString"], ["ValidationResult"])])

      Map.size index.searchDocuments `shouldBe` 1
      fieldFrequency SearchName (SearchToken "create") index `shouldBe` 1
      fieldFrequency SearchModule (SearchToken "subscriptions") index `shouldBe` 1
      fieldFrequency SearchArgumentType (SearchToken "byte") index `shouldBe` 1
      fieldFrequency SearchResultType (SearchToken "validation") index `shouldBe` 1

    it "populates postings for every field" do
      let index = testSearchIndexWithTypeFacts [decodePayload] (typeFacts [(decodePayload, ["ByteString"], ["ValidationResult"])])

      postingNames SearchName (SearchToken "decode") index `shouldBe` ["Payload.Codec.decodePayload"]
      postingNames SearchArgumentType (SearchToken "byte") index `shouldBe` ["Payload.Codec.decodePayload"]
      postingNames SearchResultType (SearchToken "validation") index `shouldBe` ["Payload.Codec.decodePayload"]
      postingNames SearchModule (SearchToken "payload") index `shouldBe` ["Payload.Codec.decodePayload"]

    it "ranks exact lookup-name equality before semantic non-exact results" do
      let suggestions =
            searchWithTypeFacts
              "createUser"
              [createUsers, createUserRequest, highIdfContext, exactCreateUser]
              (typeFacts [(highIdfContext, [], ["createUser"])])

      suggestionLookupNames suggestions `shouldBe` ["createUser", "createUsers", "createUserRequest", "load"]
      (head suggestions).suggestionExactLookupNameMatch `shouldBe` True

    it "lets rare secondary evidence materially influence ranking" do
      suggestionNames (searchWithTypeFacts "create DiscountAccount" [commonCreate, createOtherAccount] (typeFacts [(commonCreate, [], ["DiscountAccount"]), (createOtherAccount, [], ["Account"])]))
        `shouldBe` ["Subscriptions.create", "Users.create"]

    it "keeps exact name evidence stronger than the same token in type context by default" do
      suggestionNames (searchWithTypeFacts "payload" [payloadName, decodePayload] (typeFacts [(decodePayload, ["Payload"], ["Result"])]))
        `shouldBe` ["Payload.Codec.payload", "Payload.Codec.decodePayload"]

    it "caps approximate IDF at query-token IDF instead of rare stored-token IDF" do
      let index = testSearchIndex [commonUser, rarePrincipal]
          queryToken = SearchToken "user"
      evidence <- expectJust "Expected synonym evidence for rare principal." (selectedEvidenceFor queryToken rarePrincipal index)

      evidence.evidenceMatchKind `shouldBe` TokenMatchSynonym
      evidence.evidenceIdf `shouldBe` tokenIdf index SearchName queryToken

    it "uses name specificity ratio for otherwise equal name matches" do
      suggestionLookupNames (search "create user" [threeTokenCreateUser, fiveTokenCreateUser])
        `shouldBe` ["createUserRecord", "createUserBankAccountRecord"]

    it "preserves repeated query tokens for phrase boundaries" do
      parseSymbolSearchQuery "user user lookup"
        `shouldSatisfy` ((== [SearchToken "user", SearchToken "user", SearchToken "lookup"]) . (.symbolSearchTokens))

    it "strips owner hints before tokenizing symbol-search queries" do
      let unqualified = parseSymbolSearchQuery "lookup@Map"
          qualified = parseSymbolSearchQuery "Data.Map.lookup@Map"

      unqualified.symbolSearchTokens `shouldBe` [SearchToken "lookup"]
      fmap (.unNormalizedModuleName) unqualified.symbolSearchExactModule `shouldBe` Nothing
      qualified.symbolSearchTokens `shouldBe` [SearchToken "lookup"]
      fmap (.unNormalizedModuleName) qualified.symbolSearchExactModule `shouldBe` Just "Data.Map"

    it "does not combine name evidence across mutually exclusive aliases" do
      let splitAliases =
            splitAliasSymbol
              { aliases = Set.fromList [lookupOcc "fooBar", lookupOcc "bazQux"]
              }

      suggestionNames (search "fooQux" [splitAliases, genuineFooQux])
        `shouldBe` ["Alias.Real.fooQux", "Alias.Split.placeholder"]

    it "preserves capitalization bias for otherwise comparable candidates" do
      suggestionLookupNames (search "User" [lowerUser, upperUser])
        `shouldBe` ["User", "user"]

    it "orders equal-score candidates deterministically by lookup name" do
      suggestionLookupNames (search "create" [usersCreate, subscriptionsCreate])
        `shouldBe` ["create", "create"]

search :: Text -> [Symbol] -> [SymbolSuggestion]
search query symbols =
  findSimilarSymbolsCandidatesInMap builtInSynonymLexicon [] query (testSearchIndex symbols)

searchWithLexicon :: SynonymLexicon -> Text -> [Symbol] -> [SymbolSuggestion]
searchWithLexicon lexicon query symbols =
  findSimilarSymbolsCandidatesInMap lexicon [] query (testSearchIndex symbols)

searchWithPatterns :: [Text] -> Text -> [Symbol] -> [SymbolSuggestion]
searchWithPatterns rawPatterns query symbols =
  findSimilarSymbolsCandidatesInMap builtInSynonymLexicon (map compilePattern rawPatterns) query (testSearchIndex symbols)

searchWithTypeFacts :: Text -> [Symbol] -> Map.Map GHC.Name ValueTypeHeadNames -> [SymbolSuggestion]
searchWithTypeFacts query symbols facts =
  findSimilarSymbolsCandidatesInMap builtInSynonymLexicon [] query (testSearchIndexWithTypeFacts symbols facts)

testSearchIndex :: [Symbol] -> SymbolSearchIndex
testSearchIndex symbols =
  buildSymbolSearchIndex (symbolsMap symbols)

testSearchIndexWithTypeFacts :: [Symbol] -> Map.Map GHC.Name ValueTypeHeadNames -> SymbolSearchIndex
testSearchIndexWithTypeFacts symbols facts =
  buildSymbolSearchIndex (symbolsMapWithTypeFacts symbols facts)

expectDocument :: Symbol -> SymbolSearchIndex -> SymbolSearchDocument
expectDocument symbol index =
  case Map.lookup symbol.name index.searchDocuments of
    Just document -> document
    Nothing -> error "Expected indexed document"

expectJust :: String -> Maybe a -> IO a
expectJust _ (Just value) =
  pure value
expectJust message Nothing =
  expectationFailure message *> fail "unreachable"

expectRight :: String -> Either err a -> IO a
expectRight _ (Right value) =
  pure value
expectRight message (Left _) =
  expectationFailure message *> fail "unreachable"

fieldFrequency :: SymbolSearchField -> SearchToken -> SymbolSearchIndex -> Int
fieldFrequency field token index =
  Map.findWithDefault 0 token (Map.findWithDefault Map.empty field index.searchDocumentFrequencies)

postingsFor :: SymbolSearchField -> SearchToken -> SymbolSearchIndex -> Set.Set GHC.Name
postingsFor field token index =
  Map.findWithDefault Set.empty token (Map.findWithDefault Map.empty field index.searchPostings)

postingNames :: SymbolSearchField -> SearchToken -> SymbolSearchIndex -> [Text]
postingNames field token index =
  map renderName (Set.toList (postingsFor field token index))

renderName :: GHC.Name -> Text
renderName name =
  T.pack (GHC.moduleNameString (GHC.moduleName (GHC.nameModule name)) <> "." <> GHC.occNameString (GHC.nameOccName name))

moduleTexts :: SymbolSearchDocument -> [Text]
moduleTexts document =
  map (.unNormalizedModuleName) (Set.toList document.symbolSearchModules)

selectedEvidenceFor :: SearchToken -> Symbol -> SymbolSearchIndex -> Maybe TermMatchEvidence
selectedEvidenceFor queryToken symbol index = do
  suggestion <- List.find ((== symbol.name) . (.name) . (.suggestedSymbol)) (findSimilarSymbolsCandidatesInMap builtInSynonymLexicon [] queryToken.unSearchToken index)
  List.find ((== queryToken) . NE.head . (.evidenceQueryTokens)) suggestion.suggestionEvidence

term :: [Text] -> SynonymTerm
term tokens =
  SynonymTerm (NE.fromList (map SearchToken tokens))

areDirectSynonyms :: SynonymLexicon -> SynonymTerm -> SynonymTerm -> Bool
areDirectSynonyms lexicon left right =
  right `Set.member` directSynonyms lexicon left

nameEvidenceStoredStarts :: Text -> Symbol -> [Int]
nameEvidenceStoredStarts query symbol =
  case search query [symbol] of
    suggestion : _ ->
      [ item.evidenceStoredSpan.tokenSpanStart
      | item <- List.sortOn (.evidenceQuerySpan.tokenSpanStart) suggestion.suggestionEvidence,
        item.evidenceField == SearchName
      ]
    [] -> []

compilePattern :: Text -> ModulePattern
compilePattern rawPattern =
  case compileModulePattern rawPattern of
    Right pattern' -> pattern'
    Left _ -> error ("Expected valid module pattern " <> T.unpack rawPattern)

symbolsMap :: [Symbol] -> SymbolsMap
symbolsMap symbols =
  symbolsMapWithTypeFacts symbols Map.empty

symbolsMapWithTypeFacts :: [Symbol] -> Map.Map GHC.Name ValueTypeHeadNames -> SymbolsMap
symbolsMapWithTypeFacts symbols facts =
  SymbolsMap
    { homeSymbolsMap =
        SymbolsIndex
          { symbolsByLookupName = Map.fromListWith Set.union symbolEntries,
            valueTypeHeadNamesBySymbol = facts
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

typeFacts :: [(Symbol, [Text], [Text])] -> Map.Map GHC.Name ValueTypeHeadNames
typeFacts facts =
  Map.fromList [(symbol.name, valueTypeHeads arguments results) | (symbol, arguments, results) <- facts]

valueTypeHeads :: [Text] -> [Text] -> ValueTypeHeadNames
valueTypeHeads argumentNames resultNames =
  ValueTypeHeadNames
    { argumentTypeHeadNames = Set.fromList argumentNames,
      resultTypeHeadNames = Set.fromList resultNames
    }

lookupOcc :: String -> NormalizedOccName
lookupOcc text =
  (parseAndNormalizeName (T.pack text)).occName

suggestionNames :: [SymbolSuggestion] -> [Text]
suggestionNames =
  map (qualifiedSymbolName . (.suggestedSymbol))

suggestionLookupNames :: [SymbolSuggestion] -> [Text]
suggestionLookupNames =
  map (.suggestedLookupName)

qualifiedSymbolName :: Symbol -> Text
qualifiedSymbolName symbol =
  renderName symbol.name

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

decodePayload :: Symbol
decodePayload = testSymbol 1 "Payload.Codec" "decodePayload"

argyleRequest :: Symbol
argyleRequest = testSymbol 2 "ExternalProviders.Argyle.Client" "request"

databaseConnect :: Symbol
databaseConnect = testSymbol 3 "Storage.Database" "connect"

filterRows :: Symbol
filterRows = testSymbol 4 "Rows" "filterRows"

subscriptionsCreate :: Symbol
subscriptionsCreate = testSymbol 5 "Subscriptions" "create"

usersCreate :: Symbol
usersCreate = testSymbol 6 "Users" "create"

createUsers :: Symbol
createUsers = testSymbol 7 "Users" "createUsers"

createUserRequest :: Symbol
createUserRequest = testSymbol 8 "Users" "createUserRequest"

highIdfContext :: Symbol
highIdfContext = testSymbol 9 "Users" "load"

exactCreateUser :: Symbol
exactCreateUser = testSymbol 10 "Users" "createUser"

commonCreate :: Symbol
commonCreate = testSymbol 11 "Subscriptions" "create"

createOtherAccount :: Symbol
createOtherAccount = testSymbol 12 "Users" "create"

payloadName :: Symbol
payloadName = testSymbol 13 "Payload.Codec" "payload"

commonUser :: Symbol
commonUser = testSymbol 14 "Common" "user"

rarePrincipal :: Symbol
rarePrincipal = testSymbol 15 "Rare" "principal"

threeTokenCreateUser :: Symbol
threeTokenCreateUser = testSymbol 16 "Users" "createUserRecord"

fiveTokenCreateUser :: Symbol
fiveTokenCreateUser = testSymbol 17 "Users" "createUserBankAccountRecord"

lowerUser :: Symbol
lowerUser = testSymbol 18 "Users" "user"

upperUser :: Symbol
upperUser = testSymbol 19 "Users" "User"

sharedLookup :: Symbol
sharedLookup = testSymbol 20 "Internal.Lookup" "lookup"

splitAliasSymbol :: Symbol
splitAliasSymbol = testSymbol 21 "Alias.Split" "placeholder"

genuineFooQux :: Symbol
genuineFooQux = testSymbol 22 "Alias.Real" "fooQux"

scheduleJob :: Symbol
scheduleJob = testSymbol 23 "Jobs" "scheduleJob"

rocketShip :: Symbol
rocketShip = testSymbol 24 "Synthetic" "RocketShip"

beacon :: Symbol
beacon = testSymbol 25 "Synthetic" "Beacon"

signalBridge :: Symbol
signalBridge = testSymbol 26 "Synthetic" "SignalBridge"

sb :: Symbol
sb = testSymbol 27 "Synthetic" "SB"

createRocketShip :: Symbol
createRocketShip = testSymbol 28 "Synthetic" "createRocketShip"

createBeacon :: Symbol
createBeacon = testSymbol 29 "Synthetic" "createBeacon"

rocketCount :: Symbol
rocketCount = testSymbol 30 "Synthetic" "rocketCount"

shipInvoice :: Symbol
shipInvoice = testSymbol 31 "Synthetic" "shipInvoice"

createRocketShipRequest :: Symbol
createRocketShipRequest = testSymbol 32 "Synthetic" "createRocketShipRequest"

createRocketLaterShipRequest :: Symbol
createRocketLaterShipRequest = testSymbol 33 "Synthetic" "createRocketLaterShipRequest"

rocketOnly :: Symbol
rocketOnly = testSymbol 34 "Rocket" "placeholder"

placeholder :: Symbol
placeholder = testSymbol 35 "Synthetic" "placeholder"

nameModuleSplit :: Symbol
nameModuleSplit = testSymbol 36 "Ship" "rocket"

fooBarFoo :: Symbol
fooBarFoo = testSymbol 37 "Synthetic" "fooBarFoo"

domainDrivenDesign :: Symbol
domainDrivenDesign = testSymbol 38 "Synthetic" "domainDrivenDesign"
