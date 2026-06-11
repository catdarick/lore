module McpMonadSpec
  ( spec,
  )
where

import qualified Data.Set as Set
import Lore.Mcp.Monad
  ( DefinitionCacheReplacement (..),
    clearSentDefinitionHashes,
    getSentDefinitionHashes,
    replaceSentDefinitionHashes,
  )
import McpTestSupport (fixtureLoreMcpWithCache)
import Test.Hspec

spec :: Spec
spec =
  describe "Lore.Mcp.Monad definition cache helpers" do
    it "reads an empty cache" do
      hashes <-
        fixtureLoreMcpWithCache True do
          getSentDefinitionHashes

      hashes `shouldBe` Set.empty

    it "reads a populated cache" do
      hashes <-
        fixtureLoreMcpWithCache True do
          _ <- replaceSentDefinitionHashes (Set.fromList ["a", "b"])
          getSentDefinitionHashes

      hashes `shouldBe` Set.fromList ["a", "b"]

    it "replaces the complete cache" do
      hashes <-
        fixtureLoreMcpWithCache True do
          _ <- replaceSentDefinitionHashes (Set.fromList ["old"])
          _ <- replaceSentDefinitionHashes (Set.fromList ["new"])
          getSentDefinitionHashes

      hashes `shouldBe` Set.fromList ["new"]

    it "replacement does not union with old hashes" do
      hashes <-
        fixtureLoreMcpWithCache True do
          _ <- replaceSentDefinitionHashes (Set.fromList ["a", "b"])
          _ <- replaceSentDefinitionHashes (Set.fromList ["b", "c"])
          getSentDefinitionHashes

      hashes `shouldBe` Set.fromList ["b", "c"]

    it "replacing with an empty set clears the cache" do
      hashes <-
        fixtureLoreMcpWithCache True do
          _ <- replaceSentDefinitionHashes (Set.fromList ["a", "b"])
          _ <- replaceSentDefinitionHashes Set.empty
          getSentDefinitionHashes

      hashes `shouldBe` Set.empty

    it "replacement returns previous and current sizes" do
      replacement <-
        fixtureLoreMcpWithCache True do
          _ <- replaceSentDefinitionHashes (Set.fromList ["a", "b"])
          replaceSentDefinitionHashes (Set.fromList ["c"])

      replacement.previousCachedDefinitionCount `shouldBe` 2
      replacement.currentCachedDefinitionCount `shouldBe` 1

    it "clearSentDefinitionHashes follows replacement semantics" do
      (clearedCount, replacementAfterClear, hashes) <-
        fixtureLoreMcpWithCache True do
          _ <- replaceSentDefinitionHashes (Set.fromList ["a", "b"])
          clearedCount <- clearSentDefinitionHashes
          replacementAfterClear <- replaceSentDefinitionHashes (Set.fromList ["z"])
          hashes <- getSentDefinitionHashes
          pure (clearedCount, replacementAfterClear, hashes)

      clearedCount `shouldBe` 2
      replacementAfterClear.previousCachedDefinitionCount `shouldBe` 0
      replacementAfterClear.currentCachedDefinitionCount `shouldBe` 1
      hashes `shouldBe` Set.fromList ["z"]
