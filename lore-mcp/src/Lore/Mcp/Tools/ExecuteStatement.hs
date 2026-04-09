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
import Lore.Mcp.Tools.Shared (appendPartialLoadWarning, renderFailureWithPartialLoadWarning)

newtype ExecuteStatementArgs (fieldType :: FieldType) = ExecuteStatementArgs
  { statement ::
      Field
        fieldType
        ( WithMeta
            Text
            '[ Description "Haskell statement to execute in the current interpreter context. Supports GHCi style variable bindings, function definitions, and IO actions.",
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
renderExecutionResult loadResult result =
  appendPartialLoadWarning loadResult "Evaluation may be incomplete." renderedResult
  where
    renderedResult =
      if T.null outputText
        then "No output."
        else outputText

    outputText =
      T.pack result

renderExecutionFailure :: LoadTargetsResult -> [Diagnostic] -> Text
renderExecutionFailure loadResult diagnostics =
  renderFailureWithPartialLoadWarning loadResult "Evaluation may be incomplete." "Execution failed:" diagnostics
