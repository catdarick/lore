{-# LANGUAGE OverloadedStrings #-}

module ImportCleanupResolveSpec (spec) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Lore.Diagnostics (Span (..))
import Lore.Refactor.ImportCleanup.Internal
  ( ImportCleanupAction (..),
    ImportCleanupWarning (..),
    ImportId (..),
    ParsedImport (..),
    ParsedImportListKind (..),
    RedundantImportIssue (..),
    RedundantImportedOccurrence (..),
    resolveImportCleanupGroups,
  )
import Test.Hspec

spec :: Spec
spec =
  describe "import cleanup resolve" do
    it "groups occurrences by import span" do
      let parsedImport = explicitImport
          issue =
            RedundantImportOccurrencesIssue
              (Span "Demo.hs" 1 1 1 28)
              (RedundantImportedOccurrence "baz" Nothing :| [])
          (groups, warnings) =
            resolveImportCleanupGroups [parsedImport] (issue :| [])

      warnings `shouldBe` []
      groups
        `shouldBe` Map.fromList
          [ ( ImportId 1,
              RemoveImportOccurrences
                parsedImport
                (RedundantImportedOccurrence "baz" Nothing :| [])
            )
          ]

    it "prefers whole-import deletion when requested for same import" do
      let parsedImport = explicitImport
          wholeIssue =
            RedundantWholeImportIssue
              (Span "Demo.hs" 1 1 1 28)
          occurrenceIssue =
            RedundantImportOccurrencesIssue
              (Span "Demo.hs" 1 1 1 28)
              (RedundantImportedOccurrence "baz" Nothing :| [])
          (groups, warnings) =
            resolveImportCleanupGroups [parsedImport] (wholeIssue :| [occurrenceIssue])

      warnings `shouldBe` []
      Map.lookup (ImportId 1) groups `shouldBe` Just (DeleteImport parsedImport)

    it "warns when issue points to no import" do
      let issue =
            RedundantWholeImportIssue
              (Span "Demo.hs" 20 1 20 10)
          (_groups, warnings) =
            resolveImportCleanupGroups [explicitImport] (issue :| [])

      warnings `shouldBe` [NoMatchingImportForDiagnostic (Span "Demo.hs" 20 1 20 10)]

    it "warns when issue matches multiple imports" do
      let issue =
            RedundantWholeImportIssue
              (Span "Demo.hs" 1 1 1 28)
          (_groups, warnings) =
            resolveImportCleanupGroups [explicitImport, explicitImport {parsedImportId = ImportId 2}] (issue :| [])

      warnings `shouldBe` [AmbiguousDiagnosticImportMatch (Span "Demo.hs" 1 1 1 28)]

    it "warns when occurrence cleanup targets open import" do
      let issue =
            RedundantImportOccurrencesIssue
              (Span "Demo.hs" 1 1 1 16)
              (RedundantImportedOccurrence "foo" Nothing :| [])
          (_groups, warnings) =
            resolveImportCleanupGroups [openImport] (issue :| [])

      warnings `shouldBe` [ImportListRequiredForItemCleanup (ImportId 1)]

    it "warns when occurrence cleanup targets hiding import" do
      let issue =
            RedundantImportOccurrencesIssue
              (Span "Demo.hs" 1 1 1 24)
              (RedundantImportedOccurrence "foo" Nothing :| [])
          (_groups, warnings) =
            resolveImportCleanupGroups [hidingImport] (issue :| [])

      warnings `shouldBe` [HidingImportItemCleanupUnsupported (ImportId 1)]

explicitImport :: ParsedImport
explicitImport =
  ParsedImport
    { parsedImportId = ImportId 1,
      parsedImportSpan = Span "Demo.hs" 1 1 1 28,
      parsedImportModuleName = "Foo",
      parsedImportListKind = ParsedExplicitImport
    }

openImport :: ParsedImport
openImport =
  explicitImport
    { parsedImportSpan = Span "Demo.hs" 1 1 1 16,
      parsedImportListKind = ParsedOpenImport
    }

hidingImport :: ParsedImport
hidingImport =
  explicitImport
    { parsedImportSpan = Span "Demo.hs" 1 1 1 24,
      parsedImportListKind = ParsedHidingImport
    }
