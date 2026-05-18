module FindReferencesSpec
  ( spec,
  )
where

import qualified Data.Aeson as J
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Lore.Mcp.Tools.FindReferences (findReferencesTool)
import McpTestSupport
  ( callToolWithArgs,
    fixtureLoreMcpAtWithCache,
    loadFixtureTargets,
    withFixtureCopy,
  )
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec = do
  describe "findReferences" do
    it "renders the full useful context for multiline application references" do
      result <-
        renderFindReferencesFixture
          "RenderedCall"
          multilineApplicationReferenceModuleSource
          "TestRefs.RenderedCall.targetSymbol"

      result `shouldContainText` "build a b c ="
      result `shouldContainText` "  callSomeFunction"
      result `shouldContainText` "    a"
      result `shouldContainText` "    b"
      result `shouldContainText` "    targetSymbol"
      result `shouldContainText` "    c"

    it "renders useful context for do-block references" do
      result <-
        renderFindReferencesFixture
          "RenderedDo"
          doBlockReferenceModuleSource
          "TestRefs.RenderedDo.targetSymbol"

      result `shouldContainText` "build value = do"
      result `shouldContainText` "  y <- pure value"
      result `shouldContainText` "  pure (targetSymbol y)"

    it "renders useful context for case alternative references" do
      result <-
        renderFindReferencesFixture
          "RenderedCase"
          caseAlternativeReferenceModuleSource
          "TestRefs.RenderedCase.targetSymbol"

      result `shouldContainText` "build flag ="
      result `shouldContainText` "  case flag of"
      result `shouldContainText` "    True -> targetSymbol"
      result `shouldContainText` "    False -> 0"

    it "renders signature-only references intentionally" do
      result <-
        renderFindReferencesFixture
          "RenderedSignature"
          signatureReferenceModuleSource
          "TestRefs.RenderedSignature.TargetType"

      result `shouldContainText` "foo :: TargetType -> Int"
      result `shouldContainText` "foo _ = 1"

    it "keeps record-field references scoped to the queried selector alias" do
      result <-
        renderFindReferencesFixture
          "RenderedFieldAlias"
          recordFieldAliasReferenceModuleSource
          "TestRefs.RenderedFieldAlias.suggestedLookupName"

      result `shouldContainText` "pickNames suggestions ="
      result `shouldContainText` "map suggestedLookupName suggestions"
      result `shouldNotContainText` "countSuggestions suggestions = length suggestions"

    it "renders references for imported record selectors used in function-style" do
      result <-
        renderFindReferencesFixtureModules
          [ ("RecordModel", importedSelectorModelModuleSource),
            ("RecordConsumer", importedSelectorConsumerModuleSource)
          ]
          "TestRefs.RecordModel.symbolName"

      result `shouldContainText` "collectSymbolNames infos ="
      result `shouldContainText` "map symbolName infos"
      result `shouldNotContainText` "collectSymbolScores infos ="

    it "renders references for imported record selectors used via overloaded record dot" do
      result <-
        renderFindReferencesFixtureModules
          [ ("RecordDotModel", importedRecordDotModelModuleSource),
            ("RecordDotConsumer", importedRecordDotConsumerModuleSource)
          ]
          "TestRefs.RecordDotModel.unPathToRoot"

      shouldContainRecordDotSelectorUsages
        result
        "collectPathWithDot path ="
        "collectPathWithSection path ="
        "collectPathScore path ="

    it "renders references for strict record selectors" do
      result <-
        renderFindReferencesFixture
          "RenderedStrictField"
          strictFieldReferenceModuleSource
          "TestRefs.RenderedStrictField.symbolScore"

      result `shouldContainText` "collectScores infos ="
      result `shouldContainText` "map symbolScore infos"
      result `shouldNotContainText` "collectNames infos ="

    it "renders references for record selectors used via overloaded record dot" do
      result <-
        renderFindReferencesFixture
          "RenderedRecordDotField"
          recordDotFieldReferenceModuleSource
          "TestRefs.RenderedRecordDotField.unPathToRoot"

      shouldContainRecordDotSelectorUsages
        result
        "readPathWithDot path ="
        "readPathWithSection path ="
        "readScore path ="

    it "renders references for newtype selectors used via overloaded record dot" do
      result <-
        renderFindReferencesFixture
          "RenderedRecordDotNewtype"
          recordDotNewtypeReferenceModuleSource
          "TestRefs.RenderedRecordDotNewtype.unPathToRoot"

      shouldContainRecordDotSelectorUsages
        result
        "readPathWithDot path ="
        "readPathWithSection path ="
        "renderConstantPath _ ="

    it "renders references from modules using DuplicateRecordFields" do
      result <-
        renderFindReferencesFixture
          "RenderedDuplicateRecordFields"
          duplicateRecordFieldsReferenceModuleSource
          "TestRefs.RenderedDuplicateRecordFields.userScore"

      result `shouldContainText` "collectUserScores users ="
      result `shouldContainText` "map userScore users"
      result `shouldNotContainText` "collectTeamScores teams ="

    it "renders owner-qualified disambiguation hints for same-module duplicate record fields" do
      result <-
        renderFindReferencesFixture
          "RenderedDuplicateFieldNames"
          duplicateFieldNamesReferenceModuleSource
          "TestRefs.RenderedDuplicateFieldNames.fieldOne"

      result `shouldContainText` "is ambiguous. More qualification is required"
      result `shouldContainText` "TestRefs.RenderedDuplicateFieldNames.fieldOne@RecordOne"
      result `shouldContainText` "TestRefs.RenderedDuplicateFieldNames.fieldOne@RecordTwo"

    it "uses owner-qualified selector names to resolve same-module duplicate record fields" do
      result <-
        renderFindReferencesFixture
          "RenderedDuplicateFieldNames"
          duplicateFieldNamesReferenceModuleSource
          "TestRefs.RenderedDuplicateFieldNames.fieldOne@RecordOne"

      result `shouldContainText` "mkRecordOne value ="
      result `shouldContainText` "RecordOne {fieldOne = value"
      result `shouldNotContainText` "mkRecordTwo value ="

    it "does not require redundant owner hints for fully qualified ticked constructors" do
      result <-
        renderFindReferencesFixture
          "RenderedTickedConstructor"
          tickedConstructorReferenceModuleSource
          "TestRefs.RenderedTickedConstructor.Ctor'"

      result `shouldNotContainText` "is ambiguous. More qualification is required"
      result `shouldContainText` "useCtor value ="
      result `shouldContainText` "case Ctor' value of"

    it "does not require redundant owner hints for fully qualified constructors" do
      result <-
        renderFindReferencesFixture
          "RenderedConstructor"
          constructorReferenceModuleSource
          "TestRefs.RenderedConstructor.Ctor"

      result `shouldNotContainText` "is ambiguous. More qualification is required"
      result `shouldContainText` "useCtor value ="
      result `shouldContainText` "case Ctor value of"

renderFindReferencesFixture :: FilePath -> Text -> Text -> IO Text
renderFindReferencesFixture moduleFileName moduleSource symbol =
  renderFindReferencesFixtureModules [(moduleFileName, moduleSource)] symbol

renderFindReferencesFixtureModules :: [(FilePath, Text)] -> Text -> IO Text
renderFindReferencesFixtureModules modulesToWrite symbol =
  withFixtureCopy \fixtureRoot -> do
    writeFixtureModules fixtureRoot modulesToWrite

    fixtureLoreMcpAtWithCache False fixtureRoot do
      loadFixtureTargets
      callToolWithArgs findReferencesTool (findReferencesArgs symbol)

writeFixtureModules :: FilePath -> [(FilePath, Text)] -> IO ()
writeFixtureModules fixtureRoot modulesToWrite =
  createDirectoryIfMissing True moduleDir
    *> mapM_ writeModule modulesToWrite
  where
    moduleDir = fixtureRoot </> "src" </> "TestRefs"

    writeModule (moduleFileName, moduleSource) = do
      let moduleFile = moduleDir </> moduleFileName <> ".hs"
      TIO.writeFile moduleFile moduleSource

findReferencesArgs :: Text -> J.Value
findReferencesArgs symbol =
  J.object
    [ "symbol" J..= symbol
    ]

multilineApplicationReferenceModuleSource :: Text
multilineApplicationReferenceModuleSource =
  T.unlines
    [ "module TestRefs.RenderedCall",
      "  ( targetSymbol,",
      "    build",
      "  ) where",
      "",
      "targetSymbol :: Int",
      "targetSymbol = 1",
      "",
      "build :: Int -> Int -> Int -> Int",
      "build a b c =",
      "  callSomeFunction",
      "    a",
      "    b",
      "    targetSymbol",
      "    c",
      "",
      "callSomeFunction :: Int -> Int -> Int -> Int -> Int",
      "callSomeFunction a b c d = a + b + c + d"
    ]

doBlockReferenceModuleSource :: Text
doBlockReferenceModuleSource =
  T.unlines
    [ "module TestRefs.RenderedDo",
      "  ( targetSymbol,",
      "    build",
      "  ) where",
      "",
      "targetSymbol :: Int -> Int",
      "targetSymbol value = value + 1",
      "",
      "build :: Int -> IO Int",
      "build value = do",
      "  y <- pure value",
      "  pure (targetSymbol y)"
    ]

caseAlternativeReferenceModuleSource :: Text
caseAlternativeReferenceModuleSource =
  T.unlines
    [ "module TestRefs.RenderedCase",
      "  ( targetSymbol,",
      "    build",
      "  ) where",
      "",
      "targetSymbol :: Int",
      "targetSymbol = 1",
      "",
      "build :: Bool -> Int",
      "build flag =",
      "  case flag of",
      "    True -> targetSymbol",
      "    False -> 0"
    ]

signatureReferenceModuleSource :: Text
signatureReferenceModuleSource =
  T.unlines
    [ "module TestRefs.RenderedSignature",
      "  ( TargetType(..),",
      "    foo",
      "  ) where",
      "",
      "data TargetType = TargetType",
      "",
      "foo :: TargetType -> Int",
      "foo _ = 1"
    ]

recordFieldAliasReferenceModuleSource :: Text
recordFieldAliasReferenceModuleSource =
  T.unlines
    [ "module TestRefs.RenderedFieldAlias",
      "  ( SymbolSuggestion(..),",
      "    pickNames,",
      "    countSuggestions",
      "  ) where",
      "",
      "data SymbolSuggestion = SymbolSuggestion",
      "  { suggestedLookupName :: String,",
      "    suggestionScore :: Double",
      "  }",
      "",
      "pickNames :: [SymbolSuggestion] -> [String]",
      "pickNames suggestions =",
      "  map suggestedLookupName suggestions",
      "",
      "countSuggestions :: [SymbolSuggestion] -> Int",
      "countSuggestions suggestions = length suggestions"
    ]

importedSelectorModelModuleSource :: Text
importedSelectorModelModuleSource =
  T.unlines
    [ "module TestRefs.RecordModel",
      "  ( SymbolInfo(..) ) where",
      "",
      "data SymbolInfo = SymbolInfo",
      "  { symbolName :: String,",
      "    symbolScore :: Int",
      "  }"
    ]

importedSelectorConsumerModuleSource :: Text
importedSelectorConsumerModuleSource =
  T.unlines
    [ "module TestRefs.RecordConsumer",
      "  ( collectSymbolNames,",
      "    collectSymbolScores",
      "  ) where",
      "",
      "import TestRefs.RecordModel (SymbolInfo(..), symbolName, symbolScore)",
      "",
      "collectSymbolNames :: [SymbolInfo] -> [String]",
      "collectSymbolNames infos =",
      "  map symbolName infos",
      "",
      "collectSymbolScores :: [SymbolInfo] -> [Int]",
      "collectSymbolScores infos =",
      "  map symbolScore infos"
    ]

importedRecordDotModelModuleSource :: Text
importedRecordDotModelModuleSource =
  T.unlines
    [ "module TestRefs.RecordDotModel",
      "  ( PathToRoot(..) ) where",
      "",
      "newtype PathToRoot = PathToRoot",
      "  { unPathToRoot :: String",
      "  }"
    ]

importedRecordDotConsumerModuleSource :: Text
importedRecordDotConsumerModuleSource =
  T.unlines
    [ "{-# LANGUAGE OverloadedRecordDot #-}",
      "",
      "module TestRefs.RecordDotConsumer",
      "  ( collectPathWithDot,",
      "    collectPathWithSection,",
      "    collectPathScore",
      "  ) where",
      "",
      "import TestRefs.RecordDotModel (PathToRoot(..))",
      "",
      "collectPathWithDot :: PathToRoot -> String",
      "collectPathWithDot path =",
      "  path.unPathToRoot",
      "",
      "collectPathWithSection :: PathToRoot -> String",
      "collectPathWithSection path =",
      "  (.unPathToRoot) path",
      "",
      "collectPathScore :: PathToRoot -> Int",
      "collectPathScore _ =",
      "  1"
    ]

strictFieldReferenceModuleSource :: Text
strictFieldReferenceModuleSource =
  T.unlines
    [ "module TestRefs.RenderedStrictField",
      "  ( SymbolInfo(..),",
      "    collectNames,",
      "    collectScores",
      "  ) where",
      "",
      "data SymbolInfo = SymbolInfo",
      "  { symbolName :: !String,",
      "    symbolScore :: !Double",
      "    }",
      "",
      "collectNames :: [SymbolInfo] -> [String]",
      "collectNames infos =",
      "  map symbolName infos",
      "",
      "collectScores :: [SymbolInfo] -> [Double]",
      "collectScores infos =",
      "  map symbolScore infos"
    ]

recordDotFieldReferenceModuleSource :: Text
recordDotFieldReferenceModuleSource =
  T.unlines
    [ "{-# LANGUAGE OverloadedRecordDot #-}",
      "",
      "module TestRefs.RenderedRecordDotField",
      "  ( PathInfo(..),",
      "    readPathWithDot,",
      "    readPathWithSection,",
      "    readScore",
      "  ) where",
      "",
      "data PathInfo = PathInfo",
      "  { unPathToRoot :: String,",
      "    pathScore :: Int",
      "  }",
      "",
      "readPathWithDot :: PathInfo -> String",
      "readPathWithDot path =",
      "  path.unPathToRoot",
      "",
      "readPathWithSection :: PathInfo -> String",
      "readPathWithSection path =",
      "  (.unPathToRoot) path",
      "",
      "readScore :: PathInfo -> Int",
      "readScore path =",
      "  path.pathScore"
    ]

recordDotNewtypeReferenceModuleSource :: Text
recordDotNewtypeReferenceModuleSource =
  T.unlines
    [ "{-# LANGUAGE OverloadedRecordDot #-}",
      "",
      "module TestRefs.RenderedRecordDotNewtype",
      "  ( PathToRoot(..),",
      "    readPathWithDot,",
      "    readPathWithSection,",
      "    readPathLength,",
      "    renderConstantPath",
      "  ) where",
      "",
      "newtype PathToRoot = PathToRoot",
      "  { unPathToRoot :: String",
      "  }",
      "",
      "readPathWithDot :: PathToRoot -> String",
      "readPathWithDot path =",
      "  path.unPathToRoot",
      "",
      "readPathWithSection :: PathToRoot -> String",
      "readPathWithSection path =",
      "  (.unPathToRoot) path",
      "",
      "readPathLength :: PathToRoot -> Int",
      "readPathLength path =",
      "  length path.unPathToRoot",
      "",
      "renderConstantPath :: PathToRoot -> String",
      "renderConstantPath _ =",
      "  \"static\""
    ]

duplicateRecordFieldsReferenceModuleSource :: Text
duplicateRecordFieldsReferenceModuleSource =
  T.unlines
    [ "{-# LANGUAGE DuplicateRecordFields #-}",
      "",
      "module TestRefs.RenderedDuplicateRecordFields",
      "  ( User(..),",
      "    Team(..),",
      "    collectUserScores,",
      "    collectTeamScores",
      "  ) where",
      "",
      "data User = User",
      "  { sharedName :: String,",
      "    userScore :: !Int",
      "    }",
      "",
      "data Team = Team",
      "  { sharedName :: String,",
      "    teamScore :: !Int",
      "    }",
      "",
      "collectUserScores :: [User] -> [Int]",
      "collectUserScores users =",
      "  map userScore users",
      "",
      "collectTeamScores :: [Team] -> [Int]",
      "collectTeamScores teams =",
      "  map teamScore teams"
    ]

duplicateFieldNamesReferenceModuleSource :: Text
duplicateFieldNamesReferenceModuleSource =
  T.unlines
    [ "{-# LANGUAGE DuplicateRecordFields #-}",
      "",
      "module TestRefs.RenderedDuplicateFieldNames",
      "  ( RecordOne(..),",
      "    RecordTwo(..),",
      "    mkRecordOne,",
      "    mkRecordTwo",
      "  ) where",
      "",
      "data RecordOne = RecordOne",
      "  { fieldOne :: !Int,",
      "    fieldTwo :: !Int",
      "    }",
      "",
      "data RecordTwo = RecordTwo",
      "  { fieldOne :: !Int,",
      "    fieldTwo :: !Int",
      "    }",
      "",
      "mkRecordOne :: Int -> RecordOne",
      "mkRecordOne value =",
      "  RecordOne {fieldOne = value, fieldTwo = value + 1}",
      "",
      "mkRecordTwo :: Int -> RecordTwo",
      "mkRecordTwo value =",
      "  RecordTwo {fieldOne = value, fieldTwo = value + 2}"
    ]

tickedConstructorReferenceModuleSource :: Text
tickedConstructorReferenceModuleSource =
  T.unlines
    [ "module TestRefs.RenderedTickedConstructor",
      "  ( Wrap(..),",
      "    useCtor",
      "  ) where",
      "",
      "data Wrap = Ctor' Int",
      "",
      "useCtor :: Int -> Int",
      "useCtor value =",
      "  case Ctor' value of",
      "    Ctor' inner -> inner"
    ]

constructorReferenceModuleSource :: Text
constructorReferenceModuleSource =
  T.unlines
    [ "module TestRefs.RenderedConstructor",
      "  ( Wrap(..),",
      "    useCtor",
      "  ) where",
      "",
      "data Wrap = Ctor Int",
      "",
      "useCtor :: Int -> Int",
      "useCtor value =",
      "  case Ctor value of",
      "    Ctor inner -> inner"
    ]

shouldContainText :: Text -> Text -> Expectation
shouldContainText actual expected =
  T.unpack actual `shouldContain` T.unpack expected

shouldNotContainText :: Text -> Text -> Expectation
shouldNotContainText actual expected =
  T.unpack actual `shouldNotContain` T.unpack expected

shouldContainRecordDotSelectorUsages :: Text -> Text -> Text -> Text -> Expectation
shouldContainRecordDotSelectorUsages actual withDotHeader withSectionHeader unrelatedHeader = do
  actual `shouldContainText` withDotHeader
  actual `shouldContainText` "path.unPathToRoot"
  actual `shouldContainText` withSectionHeader
  actual `shouldContainText` "(.unPathToRoot) path"
  actual `shouldNotContainText` unrelatedHeader
