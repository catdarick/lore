{-# LANGUAGE OverloadedStrings #-}

module ImportCleanupEditSpec (spec) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Lore.Diagnostics (Span (..))
import Lore.Refactor.ImportCleanup.Internal
  ( ImportCleanupAction (..),
    ImportCleanupWarning (..),
    ImportId (..),
    ParsedImport (..),
    ParsedImportListKind (..),
    PlannedFileEdit (..),
    RedundantImportedOccurrence (..),
    renderImportCleanupEdits,
  )
import Lore.SourceEdit (applyReplacementEdits)
import Test.Hspec

spec :: Spec
spec =
  describe "import cleanup edits" do
    it "removes middle top-level item" do
      let source = "import Foo (A, B, C)\n"
          parsedImport = explicitImport (Span "Demo.hs" 1 1 1 21)
          action =
            RemoveImportOccurrences
              parsedImport
              (RedundantImportedOccurrence "B" Nothing :| [])
          (plannedEdits, warnings) =
            renderImportCleanupEdits "Demo.hs" source (Map.fromList [(ImportId 1, action)])

      warnings `shouldBe` []
      applyPlannedEdits source plannedEdits `shouldBe` "import Foo (A, C)\n"

    it "keeps empty list when removing only item" do
      let source = "import Foo (A)\n"
          parsedImport = explicitImport (Span "Demo.hs" 1 1 1 15)
          action =
            RemoveImportOccurrences
              parsedImport
              (RedundantImportedOccurrence "A" Nothing :| [])
          (plannedEdits, warnings) =
            renderImportCleanupEdits "Demo.hs" source (Map.fromList [(ImportId 1, action)])

      warnings `shouldBe` []
      applyPlannedEdits source plannedEdits `shouldBe` "import Foo ()\n"

    it "removes child from explicit children" do
      let source = "import Foo (Bar(A, B), baz)\n"
          parsedImport = explicitImport (Span "Demo.hs" 1 1 1 28)
          action =
            RemoveImportOccurrences
              parsedImport
              (RedundantImportedOccurrence "A" Nothing :| [])
          (plannedEdits, warnings) =
            renderImportCleanupEdits "Demo.hs" source (Map.fromList [(ImportId 1, action)])

      warnings `shouldBe` []
      applyPlannedEdits source plannedEdits `shouldBe` "import Foo (Bar(B), baz)\n"

    it "collapses single-child parent to head" do
      let source = "import Foo (Bar(A), baz)\n"
          parsedImport = explicitImport (Span "Demo.hs" 1 1 1 25)
          action =
            RemoveImportOccurrences
              parsedImport
              (RedundantImportedOccurrence "A" Nothing :| [])
          (plannedEdits, warnings) =
            renderImportCleanupEdits "Demo.hs" source (Map.fromList [(ImportId 1, action)])

      warnings `shouldBe` []
      applyPlannedEdits source plannedEdits `shouldBe` "import Foo (Bar, baz)\n"

    it "handles SomeException(SomeException) by removing child first" do
      let source = "import UnliftIO (SomeException (SomeException), handle)\n"
          parsedImport =
            (explicitImport (Span "Demo.hs" 1 1 1 55))
              { parsedImportModuleName = "UnliftIO"
              }
          action =
            RemoveImportOccurrences
              parsedImport
              (RedundantImportedOccurrence "SomeException" Nothing :| [])
          (plannedEdits, warnings) =
            renderImportCleanupEdits "Demo.hs" source (Map.fromList [(ImportId 1, action)])

      warnings `shouldBe` []
      applyPlannedEdits source plannedEdits `shouldBe` "import UnliftIO (SomeException, handle)\n"

    it "skips ambiguous occurrence matches" do
      let source = "import Foo (Bar(A), A)\n"
          parsedImport = explicitImport (Span "Demo.hs" 1 1 1 23)
          action =
            RemoveImportOccurrences
              parsedImport
              (RedundantImportedOccurrence "A" Nothing :| [])
          (plannedEdits, warnings) =
            renderImportCleanupEdits "Demo.hs" source (Map.fromList [(ImportId 1, action)])

      plannedEdits `shouldBe` []
      warnings `shouldBe` [AmbiguousImportBinding (ImportId 1) "A"]

    it "deletes whole import only when whole-import requested" do
      let source = "import Foo\n"
          parsedImport =
            (explicitImport (Span "Demo.hs" 1 1 1 11))
              { parsedImportListKind = ParsedOpenImport
              }
          action =
            DeleteImport parsedImport
          (plannedEdits, warnings) =
            renderImportCleanupEdits "Demo.hs" source (Map.fromList [(ImportId 1, action)])

      warnings `shouldBe` []
      applyPlannedEdits source plannedEdits `shouldBe` ""

    it "skips edits when import declaration contains comments" do
      let source = "import Foo (A, B) -- keep\n"
          parsedImport = explicitImport (Span "Demo.hs" 1 1 1 17)
          action =
            RemoveImportOccurrences
              parsedImport
              (RedundantImportedOccurrence "A" Nothing :| [])
          (plannedEdits, warnings) =
            renderImportCleanupEdits "Demo.hs" source (Map.fromList [(ImportId 1, action)])

      plannedEdits `shouldBe` []
      warnings `shouldBe` [ImportDeclarationContainsComments (ImportId 1)]

explicitImport :: Span -> ParsedImport
explicitImport importSpan =
  ParsedImport
    { parsedImportId = ImportId 1,
      parsedImportSpan = importSpan,
      parsedImportModuleName = "Foo",
      parsedImportListKind = ParsedExplicitImport
    }

applyPlannedEdits :: T.Text -> [PlannedFileEdit] -> T.Text
applyPlannedEdits source plannedEdits =
  applyReplacementEdits source (map (.plannedFileEdit) plannedEdits)
