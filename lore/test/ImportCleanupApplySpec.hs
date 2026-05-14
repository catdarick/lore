{-# LANGUAGE OverloadedStrings #-}

module ImportCleanupApplySpec (spec) where

import Data.List.NonEmpty (NonEmpty (..))
import Lore.Diagnostics (Span (..))
import Lore.Refactor.ImportCleanup.Internal
  ( ImportCleanupFileReport (..),
    ImportCleanupWarning (..),
    ImportId (..),
    ParsedImport (..),
    ParsedImportListKind (..),
    PlannedFileEdit (..),
    RedundantImportIssue (..),
    RedundantImportedOccurrence (..),
    planFileImportCleanup,
  )
import Lore.SourceEdit (applyReplacementEdits)
import Test.Hspec

spec :: Spec
spec =
  describe "import cleanup apply integration" do
    it "uses flat occurrence matching to avoid dropping parent head" do
      let source = "import UnliftIO (SomeException (SomeException), handle)\n"
          parsedImport =
            (explicitImport (Span "Demo.hs" 1 1 1 55))
              { parsedImportModuleName = "UnliftIO"
              }
          issue =
            RedundantImportOccurrencesIssue
              (Span "Demo.hs" 1 1 1 55)
              (RedundantImportedOccurrence "SomeException" Nothing :| [])
          report =
            planFileImportCleanup
              "Demo.hs"
              source
              [parsedImport]
              (issue :| [])
          (plannedEdits, warnings) =
            case report of
              ImportCleanupFileReport {importCleanupFileEdits, importCleanupFileWarnings} ->
                ( map
                    (\PlannedFileEdit {plannedFileEdit} -> plannedFileEdit)
                    importCleanupFileEdits,
                  importCleanupFileWarnings
                )

      applyReplacementEdits source plannedEdits `shouldBe` "import UnliftIO (SomeException, handle)\n"
      warnings `shouldBe` []

    it "makes planning warnings file-fatal" do
      let source = "import Foo (Bar(A), A)\n"
          parsedImport = explicitImport (Span "Demo.hs" 1 1 1 23)
          issue =
            RedundantImportOccurrencesIssue
              (Span "Demo.hs" 1 1 1 23)
              (RedundantImportedOccurrence "A" Nothing :| [])
          report =
            planFileImportCleanup
              "Demo.hs"
              source
              [parsedImport]
              (issue :| [])

      report.importCleanupFileEdits `shouldBe` []
      report.importCleanupFileWarnings `shouldBe` [AmbiguousImportBinding (ImportId 1) "A"]

    it "does not allow whole-import deletion when declaration has comments" do
      let source = "import Foo -- keep\n"
          parsedImport =
            (explicitImport (Span "Demo.hs" 1 1 1 11))
              { parsedImportListKind = ParsedOpenImport
              }
          issue =
            RedundantWholeImportIssue
              (Span "Demo.hs" 1 1 1 11)
          report =
            planFileImportCleanup
              "Demo.hs"
              source
              [parsedImport]
              (issue :| [])

      report.importCleanupFileEdits `shouldBe` []
      report.importCleanupFileWarnings `shouldBe` [ImportDeclarationContainsComments (ImportId 1)]

explicitImport :: Span -> ParsedImport
explicitImport importSpan =
  ParsedImport
    { parsedImportId = ImportId 1,
      parsedImportSpan = importSpan,
      parsedImportModuleName = "Foo",
      parsedImportListKind = ParsedExplicitImport
    }
