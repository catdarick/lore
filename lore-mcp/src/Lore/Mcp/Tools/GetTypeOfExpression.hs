module Lore.Mcp.Tools.GetTypeOfExpression
  ( getTypeOfExpressionTool,
  )
where

import qualified Control.Exception as Exception
import Control.Monad.Catch (Handler (..), catches)
import qualified Data.Aeson as J
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC
import GHC.Generics (Generic)
import qualified GHC.Types.SourceError as GHC.SourceError
import qualified GHC.Utils.Outputable as Outputable
import Lore
  ( LoadTargetsResult (..),
    MonadLore,
    getLastLoadTargetsResult,
    getTypeOfExpression,
    interpreterContextIsReady,
  )
import Lore.Diagnostics
  ( Diagnostic (..),
    DiagnosticClass (..),
    DiagnosticSpan (..),
    ghcMessagesToDiagnostics,
  )
import Lore.Mcp.Internal.Annotated (Description, Example, Field, FieldType (..), WithMeta)
import Lore.Mcp.Internal.Tool (SomeTool (..), ToolWithArgs (..))
import Lore.Mcp.Tools.Shared (appendPartialLoadWarning, renderFailureWithPartialLoadWarning)

newtype GetTypeOfExpressionArgs (fieldType :: FieldType) = GetTypeOfExpressionArgs
  { expression ::
      Field fieldType Text
        `WithMeta` '[ Description "Haskell expression to infer in the current interpreter context.",
                      Example "map (+1) [1, 2, 3]"
                    ]
  }
  deriving stock (Generic)

instance J.FromJSON (GetTypeOfExpressionArgs 'ValueType)

instance ToSchema (GetTypeOfExpressionArgs 'MetadataType)

getTypeOfExpressionTool :: (MonadLore m) => SomeTool m
getTypeOfExpressionTool =
  SomeToolWithArgs
    ToolWithArgs
      { name = "getTypeOfExpression",
        description = Just "Infer the Haskell type of an expression in the current project interpreter context.",
        handler = getTypeOfExpressionHandler
      }

getTypeOfExpressionHandler :: (MonadLore m) => GetTypeOfExpressionArgs 'ValueType -> m Text
getTypeOfExpressionHandler GetTypeOfExpressionArgs {expression} = do
  maybeLoadResult <- getLastLoadTargetsResult
  contextReady <- interpreterContextIsReady
  case maybeLoadResult of
    Nothing ->
      pure "Targets have not been loaded yet. Run reloadHomeModules first."
    Just loadResult
      | not contextReady ->
          pure "Interpreter context is not ready. Run reloadHomeModules again."
      | otherwise -> do
          typeResult <-
            catches
              (Right <$> getTypeOfExpression expression)
              [ Handler \sourceError ->
                  pure (Left (ghcMessagesToDiagnostics (GHC.SourceError.srcErrorMessages sourceError))),
                Handler \runtimeException ->
                  pure (Left [runtimeExceptionDiagnostic runtimeException])
              ]
          pure $
            case typeResult of
              Right inferredType ->
                renderTypeResult loadResult inferredType
              Left diagnostics ->
                renderTypeFailure loadResult diagnostics

renderTypeResult :: LoadTargetsResult -> GHC.Type -> Text
renderTypeResult loadResult inferredType =
  appendPartialLoadWarning loadResult "Type inference may be incomplete." renderedType
  where
    renderedType =
      T.pack (Outputable.showSDocUnsafe (Outputable.ppr inferredType))

renderTypeFailure :: LoadTargetsResult -> [Diagnostic] -> Text
renderTypeFailure loadResult diagnostics =
  renderFailureWithPartialLoadWarning loadResult "Type inference may be incomplete." "Type inference failed:" diagnostics

runtimeExceptionDiagnostic :: Exception.SomeException -> Diagnostic
runtimeExceptionDiagnostic runtimeException =
  Diagnostic
    { diagnosticClass = DiagInteractive,
      diagnosticSeverity = Just GHC.SevError,
      diagnosticReason = Nothing,
      diagnosticCode = Nothing,
      diagnosticSpan = UnhelpfulDiagnosticSpan "getTypeOfExpression",
      diagnosticMessage = T.pack (show runtimeException),
      diagnosticHints = []
    }
