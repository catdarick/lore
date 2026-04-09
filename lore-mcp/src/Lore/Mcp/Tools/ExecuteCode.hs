module Lore.Mcp.Tools.ExecuteCode
  ( executeCodeTool,
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

newtype ExecuteCodeArgs (fieldType :: FieldType) = ExecuteCodeArgs
  { code ::
      Field fieldType Text
        `WithMeta` '[ Description "Haskell code to execute in the current interpreter context. Supports expressions, variable bindings, function definitions, and IO actions. For multi-line statements, use `do` or `let ... in` syntax.",
                      Example
                        "do\
                        \  let x = 1 + 2\
                        \  print x",
                      Example "let add a b = a + b",
                      Example "5 * 10"
                    ]
  }
  deriving stock (Generic)

instance J.FromJSON (ExecuteCodeArgs 'ValueType)

instance ToSchema (ExecuteCodeArgs 'MetadataType)

executeCodeTool :: (MonadLore m) => SomeTool m
executeCodeTool =
  SomeToolWithArgs
    ToolWithArgs
      { name = "executeCode",
        description = Just "Execute Haskell code in the current project interpreter context. Interpreter bindings are reset when reloadHomeModules runs.",
        handler = executeCodeHandler
      }

executeCodeHandler :: (MonadLore m) => ExecuteCodeArgs 'ValueType -> m Text
executeCodeHandler ExecuteCodeArgs {code} = do
  maybeLoadResult <- getLastLoadTargetsResult
  contextReady <- interpreterContextIsReady
  case maybeLoadResult of
    Nothing ->
      pure "Targets have not been loaded yet. Run reloadHomeModules first."
    Just loadResult
      | not contextReady ->
          pure "Interpreter context is not ready. Run reloadHomeModules again."
      | otherwise -> do
          executionResult <- executeStatement code
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
