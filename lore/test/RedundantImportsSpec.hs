{-# LANGUAGE OverloadedStrings #-}

module RedundantImportsSpec (spec) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified GHC.Driver.Flags as DriverFlags
import Lore.Diagnostics
  ( Diagnostic (..),
    DiagnosticClass (..),
    DiagnosticSpan (..),
    Span (..),
  )
import Lore.Refactor.Imports
  ( ImportNamespace (..),
    RedundantImportIssue (..),
    RedundantImportedOccurrence (..),
    redundantImportIssueFromDiagnostic,
  )
import Test.Hspec

spec :: Spec
spec =
  describe "redundant import issue classifier" do
    it "classifies whole-import diagnostics" do
      let diagnostic =
            mkDiagnostic "The import of `Data.List' is redundant"
      redundantImportIssueFromDiagnostic diagnostic
        `shouldBe` Just (RedundantWholeImportIssue mkSpan)

    it "classifies qualified whole-import diagnostics" do
      let diagnostic =
            mkDiagnostic "The qualified import of `Data.Sequence' is redundant"
      redundantImportIssueFromDiagnostic diagnostic
        `shouldBe` Just (RedundantWholeImportIssue mkSpan)

    it "classifies specific-binding diagnostics" do
      let diagnostic =
            mkDiagnostic "The import of ‘foo’ from module ‘Data.Maybe’ is redundant"
      redundantImportIssueFromDiagnostic diagnostic
        `shouldBe` Just
          ( RedundantImportOccurrencesIssue
              mkSpan
              (RedundantImportedOccurrence "foo" Nothing NE.:| [])
          )

    it "classifies comma-separated occurrence diagnostics as flat occurrences" do
      let diagnostic =
            mkDiagnostic "The import of ‘foo, bar’ from module ‘Foo’ is redundant"
      redundantImportIssueFromDiagnostic diagnostic
        `shouldBe` Just
          ( RedundantImportOccurrencesIssue
              mkSpan
              ( RedundantImportedOccurrence "foo" Nothing
                  NE.:| [RedundantImportedOccurrence "bar" Nothing]
              )
          )

    it "classifies type namespace occurrence diagnostics" do
      let diagnostic =
            mkDiagnostic "The import of ‘type T’ from module ‘Foo’ is redundant"
      redundantImportIssueFromDiagnostic diagnostic
        `shouldBe` Just
          ( RedundantImportOccurrencesIssue
              mkSpan
              (RedundantImportedOccurrence "T" (Just TypeNamespace) NE.:| [])
          )

    it "classifies pattern namespace occurrence diagnostics" do
      let diagnostic =
            mkDiagnostic "The import of ‘pattern P’ from module ‘Foo’ is redundant"
      redundantImportIssueFromDiagnostic diagnostic
        `shouldBe` Just
          ( RedundantImportOccurrencesIssue
              mkSpan
              (RedundantImportedOccurrence "P" (Just PatternNamespace) NE.:| [])
          )

    it "does not classify diagnostics without the unused-imports warning flag" do
      let diagnostic =
            (mkDiagnostic "The import of `Data.List' is redundant")
              { diagnosticWarningFlag = Nothing
              }
      redundantImportIssueFromDiagnostic diagnostic
        `shouldBe` Nothing

    it "does not classify malformed specific-binding diagnostics" do
      let diagnostic =
            mkDiagnostic "The import of foo from module Data.Maybe is redundant"
      redundantImportIssueFromDiagnostic diagnostic
        `shouldBe` Nothing

mkDiagnostic :: T.Text -> Diagnostic
mkDiagnostic diagnosticMessage =
  Diagnostic
    { diagnosticClass = DiagCompiler,
      diagnosticSeverity = Nothing,
      diagnosticReason = Nothing,
      diagnosticWarningFlag = Just DriverFlags.Opt_WarnUnusedImports,
      diagnosticCode = Nothing,
      diagnosticSpan = RealDiagnosticSpan mkSpan,
      diagnosticMessage,
      diagnosticHints = []
    }

mkSpan :: Span
mkSpan =
  Span
    { spanFile = "Demo.hs",
      spanStartLine = 1,
      spanStartCol = 1,
      spanEndLine = 1,
      spanEndCol = 2
    }
