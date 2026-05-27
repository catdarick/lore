module Lore.Tools.GetTypeOfExpression
  ( GetTypeOfExpressionOptions (..),
    GetTypeOfExpressionResult,
    TypeExpressionOutput (..),
    getTypeOfExpression,
    renderTypeExpressionOutput,
  )
where

import qualified Control.Exception as Exception
import Control.Monad.Catch (Handler (..), catches)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC
import qualified GHC.Types.SourceError as GHC.SourceError
import qualified GHC.Utils.Outputable as Outputable
import qualified Lore as Core
import Lore.Diagnostics
  ( Diagnostic (..),
    DiagnosticClass (..),
    DiagnosticSpan (..),
    ghcMessagesToDiagnostics,
  )
import Lore.Tools.Render.Diagnostics (diagnosticSummaryDoc)
import Lore.Tools.Render.Doc (LoreDoc, heading2, paragraph)
import Lore.Tools.Result
  ( PartialLoadWarning,
    ToolRun,
    loadedSessionPartialWarning,
    withInterpreterSession,
    withPartialLoadWarning,
  )

newtype GetTypeOfExpressionOptions = GetTypeOfExpressionOptions
  { typeOfExpressionInput :: Text
  }
  deriving stock (Eq, Show)

type GetTypeOfExpressionResult = ToolRun TypeExpressionOutput

data TypeExpressionOutput
  = TypeExpressionSucceeded (Maybe PartialLoadWarning) Text
  | TypeExpressionFailed (Maybe PartialLoadWarning) [Diagnostic]

getTypeOfExpression :: (Core.MonadLore m) => GetTypeOfExpressionOptions -> m GetTypeOfExpressionResult
getTypeOfExpression options =
  withInterpreterSession \session -> do
    typeResult <-
      catches
        (Right <$> Core.getTypeOfExpression options.typeOfExpressionInput)
        [ Handler \sourceError ->
            pure (Left (ghcMessagesToDiagnostics (GHC.SourceError.srcErrorMessages sourceError))),
          Handler \runtimeException ->
            pure (Left [runtimeExceptionDiagnostic runtimeException])
        ]
    let partialWarning = loadedSessionPartialWarning session "Type inference may be incomplete."
    pure $
      case typeResult of
        Right inferredType ->
          TypeExpressionSucceeded partialWarning (T.pack (Outputable.showSDocUnsafe (Outputable.ppr inferredType)))
        Left diagnostics ->
          TypeExpressionFailed partialWarning diagnostics

renderTypeExpressionOutput :: TypeExpressionOutput -> LoreDoc
renderTypeExpressionOutput = \case
  TypeExpressionSucceeded warning renderedType ->
    withPartialLoadWarning warning $
      heading2 "Type"
        <> paragraph renderedType
  TypeExpressionFailed warning diagnostics ->
    withPartialLoadWarning warning $
      heading2 "Type inference failed"
        <> diagnosticSummaryDoc diagnostics

runtimeExceptionDiagnostic :: Exception.SomeException -> Diagnostic
runtimeExceptionDiagnostic runtimeException =
  Diagnostic
    { diagnosticClass = DiagInteractive,
      diagnosticSeverity = Just GHC.SevError,
      diagnosticReason = Nothing,
      diagnosticWarningFlag = Nothing,
      diagnosticCode = Nothing,
      diagnosticSpan = UnhelpfulDiagnosticSpan "getTypeOfExpression",
      diagnosticMessage = T.pack (show runtimeException),
      diagnosticHints = []
    }
