module LookupSearchSpec
  ( spec,
  )
where

import Lore.Lookup.Search (rankSearchTexts, tokenizeSearchTextValues)
import Test.Hspec

spec :: Spec
spec =
  describe "lookup search" do
    describe "tokenizeSearchTextValues" do
      it "splits camel case and keeps acronyms together" do
        tokenizeSearchTextValues "getAPI" `shouldBe` ["get", "api"]
        tokenizeSearchTextValues "getAPIResponse" `shouldBe` ["get", "api", "response"]
        tokenizeSearchTextValues "getRegularDefinition" `shouldBe` ["get", "regular", "definition"]
        tokenizeSearchTextValues "parseHTTPRequest" `shouldBe` ["parse", "http", "request"]

      it "splits snake, kebab, and qualified names" do
        tokenizeSearchTextValues "lookup_symbol-info" `shouldBe` ["lookup", "symbol", "info"]
        tokenizeSearchTextValues "Demo.lookupSymbolInfo" `shouldBe` ["demo", "lookup", "symbol", "info"]

      it "does not use one-letter alphanumeric tokens as search words" do
        tokenizeSearchTextValues "xValue" `shouldBe` ["value"]

    describe "rankSearchTexts" do
      it "ranks candidates with inserted words above candidates that only share a common word" do
        rankSearchTexts
          3
          "getDefinition"
          [ "resolveDefinition",
            "getRegularDefinition",
            "getModuleInfo"
          ]
          `shouldBe` ["getRegularDefinition", "resolveDefinition", "getModuleInfo"]

      it "uses similar stored tokens to handle query typos" do
        take
          1
          ( rankSearchTexts
              3
              "getDefinit"
              [ "resolveDefinition",
                "getRegularDefinition",
                "getModuleInfo"
              ]
          )
          `shouldBe` ["getRegularDefinition"]

      it "weights rare matching tokens more than common matching tokens" do
        take
          1
          ( rankSearchTexts
              3
              "getSymbol"
              [ "getModule",
                "setSymbol",
                "getValue",
                "getName"
              ]
          )
          `shouldBe` ["setSymbol"]

      it "penalizes candidates whose first-letter capitalization mismatches the query" do
        rankSearchTexts
          2
          "lookup"
          [ "Lookup",
            "lookup"
          ]
          `shouldBe` ["lookup", "Lookup"]

      it "keeps uppercase candidates first when query starts uppercase" do
        rankSearchTexts
          2
          "Lookup"
          [ "lookup",
            "Lookup"
          ]
          `shouldBe` ["Lookup", "lookup"]
