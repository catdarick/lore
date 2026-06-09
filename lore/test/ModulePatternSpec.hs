module ModulePatternSpec
  ( spec,
  )
where

import Data.Text (Text)
import Lore.Internal.Lookup.ModulePattern (ModulePattern, compileModulePattern, matchesModulePattern)
import Lore.Internal.Lookup.Name (mkNormalizedModuleName)
import Test.Hspec

spec :: Spec
spec =
  describe "module pattern matching" do
    it "matches exact module names" do
      "Foo.Bar" `shouldMatch` "Foo.Bar"
      "Foo.Bar" `shouldNotMatch` "Foo.Bar.Baz"
      "Foo" `shouldNotMatch` "FooFoo"
      "Foo.Bar" `shouldNotMatch` "Foo.Bar.Middle.Foo.Bar"

    it "matches prefix wildcards" do
      "Foo.Bar*" `shouldMatch` "Foo.Bar"
      "Foo.Bar*" `shouldMatch` "Foo.Bar.Baz"

    it "matches suffix wildcards" do
      "*.Commands.User" `shouldMatch` "Product.Database.Commands.User"

    it "matches middle wildcards" do
      "Foo.*.User" `shouldMatch` "Foo.Database.User"
      "Foo.*.User" `shouldMatch` "Foo.Internal.Database.User"
      "Foo.*.User" `shouldNotMatch` "Foo.UserActions"

    it "matches multiple stars" do
      "*.Database.*.User" `shouldMatch` "Product.Database.Commands.User"

    it "matches all modules with only wildcards" do
      "*" `shouldMatch` "Every.Module"
      "**" `shouldMatch` "Every.Module"

    it "treats repeated stars like one star" do
      matches "Foo.**.User" "Foo.Internal.Database.User"
        `shouldBe` matches "Foo.*.User" "Foo.Internal.Database.User"

    it "is case-sensitive" do
      "foo.*" `shouldNotMatch` "Foo.Bar"

    it "rejects empty input" do
      compileModulePattern "" `shouldSatisfy` either (const True) (const False)

    it "uses OR semantics for multiple patterns" do
      matchesAnyPattern ["Placid.Gateways.*", "ExternalProviders.*"] "Placid.Gateways.Database.User" `shouldBe` True
      matchesAnyPattern ["Placid.Gateways.*", "ExternalProviders.*"] "ExternalProviders.Argyle.Client" `shouldBe` True
      matchesAnyPattern ["Placid.Gateways.*", "ExternalProviders.*"] "Product.Array.Server" `shouldBe` False
      matchesAnyPattern [] "Product.Array.Server" `shouldBe` True

shouldMatch :: Text -> Text -> Expectation
shouldMatch patternText moduleText =
  matches patternText moduleText `shouldBe` True

shouldNotMatch :: Text -> Text -> Expectation
shouldNotMatch patternText moduleText =
  matches patternText moduleText `shouldBe` False

matchesAnyPattern :: [Text] -> Text -> Bool
matchesAnyPattern [] _ =
  True
matchesAnyPattern patternTexts moduleText =
  any (\pattern' -> matchesModulePattern pattern' (mkNormalizedModuleName moduleText)) (map compilePattern patternTexts)

matches :: Text -> Text -> Bool
matches patternText moduleText =
  compilePattern patternText `matchesModulePattern` mkNormalizedModuleName moduleText

compilePattern :: Text -> ModulePattern
compilePattern patternText =
  case compileModulePattern patternText of
    Right pattern' -> pattern'
    Left _ -> error "Expected valid module pattern"
