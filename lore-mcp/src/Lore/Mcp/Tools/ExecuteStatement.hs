module Lore.Mcp.Tools.ExecuteStatement
  ( executeStatementTool,
  )
where

import qualified Data.Aeson as J
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Lore
  ( Diagnostic (..),
    LoadTargetsResult (..),
    MonadLore,
    executeStatement,
    getLastLoadTargetsResult,
    interpreterContextIsReady,
  )
import Lore.Mcp.Internal.Annotated (Description, Example, Field, FieldType (..), WithMeta)
import Lore.Mcp.Internal.Tool (SomeTool (..), ToolWithArgs (..))

newtype ExecuteStatementArgs (fieldType :: FieldType) = ExecuteStatementArgs
  { statement ::
      Field
        fieldType
        ( WithMeta
            Text
            '[ Description "Haskell statement to execute in the current interpreter context.",
               Example "print (map (+1) [1, 2, 3])"
             ]
        )
  }
  deriving stock (Generic)

instance J.FromJSON (ExecuteStatementArgs 'ValueType)

instance ToSchema (ExecuteStatementArgs 'MetadataType)

executeStatementTool :: (MonadLore m) => SomeTool m
executeStatementTool =
  SomeToolWithArgs
    ToolWithArgs
      { name = "executeStatement",
        description = Just "Execute a Haskell statement in the current project interpreter context.",
        handler = executeStatementHandler
      }

executeStatementHandler :: (MonadLore m) => ExecuteStatementArgs 'ValueType -> m Text
executeStatementHandler ExecuteStatementArgs {statement} = do
  maybeLoadResult <- getLastLoadTargetsResult
  contextReady <- interpreterContextIsReady
  case maybeLoadResult of
    Nothing ->
      pure "Targets have not been loaded yet. Run loadTargets first."
    Just loadResult
      | not contextReady ->
          pure "Interpreter context is not ready. Run loadTargets again."
      | otherwise -> do
          executionResult <- executeStatement statement
          pure $
            case executionResult of
              Right rendered ->
                renderExecutionResult loadResult rendered
              Left diagnostics ->
                renderExecutionFailure loadResult diagnostics

renderExecutionResult :: LoadTargetsResult -> String -> Text
renderExecutionResult loadResult result
  | loadResult.loadTargetsModulesFailed > 0 =
      renderPartialLoadWarning loadResult
        <> "\n"
        <> renderedResult
  | otherwise =
      renderedResult
  where
    renderedResult =
      if T.null outputText
        then "No output."
        else outputText

    outputText =
      T.pack result

renderPartialLoadWarning :: LoadTargetsResult -> Text
renderPartialLoadWarning loadResult =
  "Warning: only "
    <> T.pack (show loadResult.loadTargetsModulesLoaded)
    <> " of "
    <> T.pack (show loadResult.loadTargetsModulesTotal)
    <> " modules loaded successfully. Evaluation may be incomplete."

renderExecutionFailure :: LoadTargetsResult -> [Diagnostic] -> Text
renderExecutionFailure loadResult diagnostics =
  T.unlines $
    warningLines
      <> ["Execution failed:"]
      <> map renderDiagnosticSummary diagnostics
  where
    warningLines =
      [ renderPartialLoadWarning loadResult
      | loadResult.loadTargetsModulesFailed > 0
      ]

renderDiagnosticSummary :: Diagnostic -> Text
renderDiagnosticSummary Diagnostic {diagnosticMessage} =
  "- " <> firstMessageLine
  where
    firstMessageLine =
      case filter (not . T.null) (map T.strip (T.lines diagnosticMessage)) of
        line : _ -> stripBulletPrefix line
        [] -> "<empty>"

stripBulletPrefix :: Text -> Text
stripBulletPrefix text =
  case T.stripPrefix "* " text of
    Just stripped -> stripped
    Nothing -> text
