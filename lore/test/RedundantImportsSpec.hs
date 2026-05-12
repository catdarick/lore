module RedundantImportsSpec (spec) where

import Data.Maybe (isJust)
import qualified Data.Text as T
import Lore.Diagnostics
  ( Diagnostic (..),
    DiagnosticClass (..),
    DiagnosticSpan (..),
    Span (..),
  )
import Lore.Refactor.Imports
  ( redundantImportRequestFromDiagnostic,
  )
import Test.Hspec

spec :: Spec
spec =
  describe "redundant import diagnostic parser" do
    it "does not downgrade specific-binding diagnostics to whole-import removals" do
      let diagnostic =
            mkDiagnostic "The import of foo from module Data.Maybe is redundant"
      redundantImportRequestFromDiagnostic diagnostic
        `shouldBe` Nothing

    it "still parses whole-import redundant diagnostics" do
      let diagnostic =
            mkDiagnostic "The qualified import of `Data.Sequence' is redundant"
      redundantImportRequestFromDiagnostic diagnostic
        `shouldSatisfy` isJust

mkDiagnostic :: T.Text -> Diagnostic
mkDiagnostic diagnosticMessage =
  Diagnostic
    { diagnosticClass = DiagCompiler,
      diagnosticSeverity = Nothing,
      diagnosticReason = Nothing,
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
