module Lore.Mcp.Tools.ExecuteCode
  ( executeCodeTool,
  )
where

import qualified Data.Aeson as J
import Data.List (isInfixOf)
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
        `WithMeta` '[ Description "Haskell code to execute in the current interpreter context. Supports expressions, variable bindings, function definitions, and IO actions. For multi-line statements, use `do` or `let ... in` syntax. Do not use `where` blocks as they may cause parse errors.",
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
        description = Just "Execute Haskell code in the current project interpreter context, not a forgiving scratchpad. Normal import, ambiguity, shadowing, type-defaulting, and `Show` constraints apply. Import declarations are not supported inside `executeCode` snippets; use the existing interpreter context and fully qualified names when needed. Avoid `where` in snippets because it may produce parse errors; use `let` bindings inside `do` blocks instead. Polymorphic expressions may require explicit type annotations. Interpreter bindings are reset whenever `reloadHomeModules` runs.",
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
    <> renderExecutionHints diagnostics

renderExecutionHints :: [Diagnostic] -> Text
renderExecutionHints diagnostics =
  case suggestedHints of
    [] -> ""
    _ ->
      "\n\nLikely fixes:\n"
        <> T.unlines (map ("- " <>) suggestedHints)
  where
    diagnosticMessages =
      map (T.unpack . diagnosticMessage) diagnostics
    suggestedHints =
      concat
        [ [ "For multi-line code, wrap statements in `do` or use `let ... in` instead of relying on top-level layout."
          | any isParseErrorMessage diagnosticMessages
          ],
          [ "Avoid `where` blocks in `executeCode` snippets. Move local helpers into `let` bindings inside `do`, or inline them."
          | any isParseErrorMessage diagnosticMessages || any (" where" `isInfixOf`) diagnosticMessages
          ],
          [ "Import declarations are not supported in `executeCode`. Use names already available in the interpreter context, or switch to fully qualified names for modules already in scope."
          | any isImportParseErrorMessage diagnosticMessages
          ],
          [ "Add an explicit type annotation when the expression is polymorphic or the monad/result type is ambiguous."
          | any isTypeAmbiguityMessage diagnosticMessages
          ],
          [ "If the failure mentions `m0`, `Monad`, or `IO`, pin the expression to a concrete monad or result type, for example with `:: IO ()` or by binding an intermediate value."
          | any isMonadAmbiguityMessage diagnosticMessages
          ],
          [ "Printing requires a `Show` instance. If the value has no `Show`, inspect a field or pattern-match on the result instead."
          | any (\message -> "No instance for (Show" `isInfixOf` message) diagnosticMessages
          ],
          [ "This runs in interpreter context, so ordinary shadowing and duplicate-binding rules still apply."
          | any isShadowingMessage diagnosticMessages
          ],
          [ "If a reused name such as `state` or a record-field label causes conflicts, rename the local binding to something unambiguous before evaluating the snippet."
          | any isRecordFieldConflictMessage diagnosticMessages
          ]
        ]

isParseErrorMessage :: String -> Bool
isParseErrorMessage message =
  "parse error" `isInfixOf` message
    || "parse error on input" `isInfixOf` message

isImportParseErrorMessage :: String -> Bool
isImportParseErrorMessage message =
  "parse error on input `import'" `isInfixOf` message

isTypeAmbiguityMessage :: String -> Bool
isTypeAmbiguityMessage message =
  "Ambiguous type variable" `isInfixOf` message
    || "Couldn't match expected type" `isInfixOf` message
    || "Couldn't match type" `isInfixOf` message

isMonadAmbiguityMessage :: String -> Bool
isMonadAmbiguityMessage message =
  "Ambiguous type variable" `isInfixOf` message
    && ("Monad" `isInfixOf` message || "IO" `isInfixOf` message || "m0" `isInfixOf` message)
    || "Could not deduce" `isInfixOf` message
      && ("Monad" `isInfixOf` message || "MonadIO" `isInfixOf` message || "MonadReader" `isInfixOf` message)

isShadowingMessage :: String -> Bool
isShadowingMessage message =
  "Multiple declarations of" `isInfixOf` message
    || "Conflicting definitions for" `isInfixOf` message
    || "already in scope" `isInfixOf` message

isRecordFieldConflictMessage :: String -> Bool
isRecordFieldConflictMessage message =
  "record field" `isInfixOf` message
    || "selector" `isInfixOf` message
    || "already in scope" `isInfixOf` message
