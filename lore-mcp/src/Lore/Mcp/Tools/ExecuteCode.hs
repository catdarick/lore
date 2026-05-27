module Lore.Mcp.Tools.ExecuteCode
  ( executeCodeTool,
  )
where

import qualified Data.Aeson as J
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import Lore (MonadLore)
import Lore.Mcp.Internal.Annotated (Description, Example, Field, FieldType (..), WithMeta)
import Lore.Mcp.Internal.Tool (SomeTool (..), ToolWithArgs (..))
import Lore.Tools.ExecuteCode
  ( ExecuteCodeOptions (..),
    executeCode,
    renderExecuteCode,
  )
import Lore.Tools.Render.Doc (LoreDoc, ToLoreDoc (toLoreDoc))
import Lore.Tools.Result
  ( ToolRun (..),
  )

newtype ExecuteCodeArgs (fieldType :: FieldType) = ExecuteCodeArgs
  { code ::
      Field fieldType Text
        `WithMeta` '[ Description "The Haskell expression or quick IO action to evaluate. Must be a single line. The result type must be either IO or a pure value with a Show instance.",
                      Example "print (1 + 2)",
                      Example "let add a b = a + b in add 5 10",
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
        description = Just "Execute a one-line Haskell expression in the interpreter context. If you need multiple lines, local helpers, or complex logic, you MUST use `createTemporalModule` FIRST to define them, reload, and then call them via this tool. Normal evaluation rules apply (ambiguity, type-defaulting, shadowing). Import declarations are not supported; use fully qualified names. Returns the stdout output and the Show rendering of the result.",
        handler = executeCodeHandler
      }

executeCodeHandler :: (MonadLore m) => ExecuteCodeArgs 'ValueType -> m LoreDoc
executeCodeHandler ExecuteCodeArgs {code} = do
  result <- executeCode ExecuteCodeOptions {executeCodeInput = code}
  pure $
    case result of
      ToolRunBlocked blocked ->
        toLoreDoc blocked
      ToolRunReady output ->
        renderExecuteCode output
