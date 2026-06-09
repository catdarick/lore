module ConfigSpec
  ( spec,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import qualified Data.ByteString.Char8 as BS
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import qualified Data.Yaml as Y
import qualified Lore
import Lore.Config
  ( DeadCodeConfig (..),
    LoreConfig (..),
    SymbolSearchConfig (..),
    defaultLoreConfig,
    defaultSymbolSearchConfig,
    loadLoreConfig,
  )
import Lore.Internal.Lookup.SymbolSearch.Synonyms
  ( SynonymGroupError (..),
    SynonymLexicon,
    SynonymTerm (..),
    compileSynonymGroups,
    directSynonyms,
  )
import Lore.Internal.Lookup.SymbolSearch.Types (SearchToken (..))
import System.FilePath ((</>))
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import TestSupport (fixtureLore, fixtureProjectRoot, withFixtureSpec)

spec :: Spec
spec = do
  withFixtureSpec do
    describe "lore.yaml loading" do
      it "uses the default config when lore.yaml is missing" \fixture -> do
        config <- fixtureLore fixture loadLoreConfig
        config `shouldBe` Right defaultLoreConfig

      it "applies project synonym edits on the next search without reloading modules" \fixture -> do
        let configPath = fixtureProjectRoot fixture </> "lore.yaml"
            findFoobar =
              Lore.findSimilarSymbols
                Lore.FindSimilarSymbolsOptions
                  { Lore.similarSymbolsQuery = "foobar",
                    Lore.similarSymbolsModulePatterns = []
                  }

        (beforeConfig, afterConfig) <-
          fixtureLore fixture do
            _ <- Lore.loadHomeModules Lore.defaultLoadHomeModulesOptions
            beforeConfig <- findFoobar
            liftIO (TIO.writeFile configPath "symbol-search:\n  synonym-groups:\n    - [\"foobar\", \"zero\"]\n")
            afterConfig <- findFoobar
            pure (beforeConfig, afterConfig)

        fmap (map (.suggestedLookupName)) beforeConfig `shouldBe` Right []
        fmap (map (.suggestedLookupName)) afterConfig `shouldSatisfy` either (const False) ("lookupOrZero" `elem`)

  describe "lore.yaml configuration" do
    it "parses dead-code alive roots under dead-code" do
      parseConfig "dead-code:\n  alive-modules:\n    - Main\n  alive-symbols:\n    - runApplication\n"
        `shouldBe` Right
          defaultLoreConfig
            { loreConfigDeadCode =
                DeadCodeConfig
                  { deadCodeConfigAliveModules = ["Main"],
                    deadCodeConfigAliveSymbols = ["runApplication"]
                  }
            }

    it "rejects legacy top-level alive roots" do
      parseConfig "alive-modules:\n  - Main\n"
        `shouldSatisfy` isLeft

    it "parses missing synonym-groups as no project synonym groups" do
      parseConfig "symbol-search: {}\n"
        `shouldBe` Right
          defaultLoreConfig
            { loreConfigSymbolSearch = defaultSymbolSearchConfig
            }

    it "parses valid list-of-lists synonym groups" do
      fmap configSynonymGroups (parseConfig "symbol-search:\n  synonym-groups:\n    - [\"customer\", \"client\"]\n    - [\"enqueue\", \"schedule\", \"submit\"]\n")
        `shouldBe` Right ([["customer", "client"], ["enqueue", "schedule", "submit"]] :: [[Text]])

    it "parses comments and quoted operator terms" do
      fmap configSynonymGroups (parseConfig "symbol-search:\n  synonym-groups:\n    # operator terms must stay strings\n    - [\"<|>\", \"alternative\", \"choice\"]\n")
        `shouldBe` Right ([["<|>", "alternative", "choice"]] :: [[Text]])

    it "rejects a scalar instead of a synonym group" do
      parseConfig "symbol-search:\n  synonym-groups:\n    - customer\n"
        `shouldSatisfy` isLeft

    it "rejects a non-string synonym term" do
      parseConfig "symbol-search:\n  synonym-groups:\n    - [\"customer\", null]\n"
        `shouldSatisfy` isLeft

  describe "symbol-search synonym group validation" do
    it "rejects empty groups" do
      compileSynonymGroups [[]]
        `shouldSatisfy` hasError \case
          SynonymGroupHasTooFewTerms 1 -> True
          _ -> False

    it "rejects one-term groups" do
      compileSynonymGroups [["customer"]]
        `shouldSatisfy` hasError \case
          SynonymGroupHasTooFewTerms 1 -> True
          _ -> False

    it "rejects duplicate terms that collapse to one token" do
      compileSynonymGroups [["user", "user"]]
        `shouldSatisfy` hasError \case
          SynonymGroupHasTooFewDistinctTerms 1 [SynonymTerm (SearchToken "user" NE.:| [])] -> True
          _ -> False

    it "collapses case-equivalent terms before rejecting distinctness" do
      compileSynonymGroups [["Customer", "customer"]]
        `shouldSatisfy` hasError \case
          SynonymGroupHasTooFewDistinctTerms _ [SynonymTerm (SearchToken "customer" NE.:| [])] -> True
          _ -> False

    it "collapses canonically equivalent plurals before rejecting distinctness" do
      compileSynonymGroups [["users", "user"]]
        `shouldSatisfy` hasError \case
          SynonymGroupHasTooFewDistinctTerms _ [SynonymTerm (SearchToken "user" NE.:| [])] -> True
          _ -> False

    it "rejects duplicate multi-token terms that normalize equivalently" do
      compileSynonymGroups [["RocketShip", "rocket ship"]]
        `shouldSatisfy` hasError \case
          SynonymGroupHasTooFewDistinctTerms _ [SynonymTerm (SearchToken "rocket" NE.:| [SearchToken "ship"])] -> True
          _ -> False

    it "accepts multi-token terms" do
      lexicon <- expectRight (compileSynonymGroups [["RocketShip", "Beacon"]])
      areDirectSynonyms lexicon (SynonymTerm (SearchToken "rocket" NE.:| [SearchToken "ship"])) (SynonymTerm (SearchToken "beacon" NE.:| [])) `shouldBe` True

    it "accepts operator tokens that produce one token" do
      lexicon <- expectRight (compileSynonymGroups [["<|>", "alternative"]])
      areDirectSynonyms lexicon (SynonymTerm (SearchToken "<|>" NE.:| [])) (SynonymTerm (SearchToken "alternative" NE.:| [])) `shouldBe` True

    it "treats duplicate valid groups as harmless" do
      lexicon <- expectRight (compileSynonymGroups [["enqueue", "schedule"], ["enqueue", "schedule"]])
      areDirectSynonyms lexicon (SynonymTerm (SearchToken "enqueue" NE.:| [])) (SynonymTerm (SearchToken "schedule" NE.:| [])) `shouldBe` True

parseConfig :: BS.ByteString -> Either String LoreConfig
parseConfig =
  first show . Y.decodeEither'

configSynonymGroups :: LoreConfig -> [[Text]]
configSynonymGroups config =
  config.loreConfigSymbolSearch.symbolSearchSynonymGroups

isLeft :: Either err value -> Bool
isLeft = \case
  Left _ -> True
  Right _ -> False

hasError :: (SynonymGroupError -> Bool) -> Either (NE.NonEmpty SynonymGroupError) value -> Bool
hasError predicate = \case
  Left errors -> any predicate (NE.toList errors)
  Right _ -> False

expectRight :: Either err value -> IO value
expectRight = \case
  Right value -> pure value
  Left _ -> fail "Expected Right"

areDirectSynonyms :: SynonymLexicon -> SynonymTerm -> SynonymTerm -> Bool
areDirectSynonyms lexicon left right =
  right `Set.member` directSynonyms lexicon left
