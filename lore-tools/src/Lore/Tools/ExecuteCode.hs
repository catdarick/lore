module Lore.Tools.ExecuteCode
  ( ExecuteCodeOptions (..),
    ExecuteCodeResult,
    ExecuteCodeOutput (..),
    executeCode,
    renderExecuteCode,
  )
where

import Data.List (isInfixOf)
import Data.Text (Text)
import qualified Data.Text as T
import Lore (Diagnostic (..))
import qualified Lore as Core
import Lore.Tools.Render.Diagnostics (diagnosticSummaryDoc)
import Lore.Tools.Render.Doc (LoreDoc, bulletList, heading2, paragraph)
import Lore.Tools.Result
  ( PartialLoadWarning,
    ToolRun,
    loadedSessionPartialWarning,
    withInterpreterSession,
    withPartialLoadWarning,
  )

newtype ExecuteCodeOptions = ExecuteCodeOptions
  { executeCodeInput :: Text
  }
  deriving stock (Eq, Show)

type ExecuteCodeResult = ToolRun ExecuteCodeOutput

data ExecuteCodeOutput
  = ExecuteCodeSucceeded (Maybe PartialLoadWarning) Text
  | ExecuteCodeFailed (Maybe PartialLoadWarning) [Diagnostic] [Text]

executeCode :: (Core.MonadLore m) => ExecuteCodeOptions -> m ExecuteCodeResult
executeCode options =
  withInterpreterSession \session -> do
    executionResult <- Core.executeStatement options.executeCodeInput
    let partialWarning = loadedSessionPartialWarning session "Evaluation may be incomplete."
    pure $
      case executionResult of
        Right rendered ->
          ExecuteCodeSucceeded partialWarning (T.pack rendered)
        Left diagnostics ->
          ExecuteCodeFailed partialWarning diagnostics (executionHints diagnostics)

renderExecuteCode :: ExecuteCodeOutput -> LoreDoc
renderExecuteCode = \case
  ExecuteCodeSucceeded warning output ->
    withPartialLoadWarning warning $
      paragraph $
        if T.null output
          then "No output."
          else output
  ExecuteCodeFailed warning diagnostics hints ->
    withPartialLoadWarning warning $
      heading2 "Execution failed"
        <> diagnosticSummaryDoc diagnostics
        <> likelyFixesDoc hints

likelyFixesDoc :: [Text] -> LoreDoc
likelyFixesDoc hints =
  case hints of
    [] -> mempty
    _ ->
      heading2 "Likely fixes"
        <> bulletList (map paragraph hints)

executionHints :: [Diagnostic] -> [Text]
executionHints diagnostics =
  concat
    [ [ "For multi-line code, complex bindings, or local helpers, do not use `executeCode`. Define them first using the `createTemporalModule` tool, reload, and then evaluate the result here."
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
  where
    diagnosticMessages =
      map (T.unpack . diagnosticMessage) diagnostics

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
