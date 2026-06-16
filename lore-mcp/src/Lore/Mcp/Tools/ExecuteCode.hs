module Lore.Mcp.Tools.ExecuteCode
  ( executeCodeTool,
  )
where

import qualified Data.Aeson as J
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import Lore (MonadLore)
import Lore.Mcp.Internal.Annotated (Description, Field, FieldType (..), WithMeta)
import Lore.Mcp.Internal.Tool (SomeTool (..), ToolWithArgs (..), renderToolRun)
import Lore.Tools.ExecuteCode
  ( ExecuteCodeOptions (..),
    executeCode,
    renderExecuteCode,
  )
import Lore.Tools.Render.Doc (LoreDoc)
import Lore.Tools.Result
  (
  )

newtype ExecuteCodeArgs (fieldType :: FieldType) = ExecuteCodeArgs
  { code ::
      Field fieldType Text
        `WithMeta` '[ Description "The Haskell expression or quick IO action to evaluate. Must be a single line. The result type must be either IO or a pure value with a Show instance. Examples: \"print (1 + 2)\", \"let add a b = a + b in add 5 10\", \"5 * 10\"."
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
        description = Just "Execute a one-line Haskell expression in the interpreter context. If you need multiple lines, local helpers, or complex logic, you must use `createTemporalModule` first to define them, reload, and then call them via this tool. Import declarations are not supported; use fully qualified names. Returns the stdout output and the Show rendering of the result.",
        handler = executeCodeHandler
      }

executeCodeHandler :: (MonadLore m) => ExecuteCodeArgs 'ValueType -> m LoreDoc
executeCodeHandler ExecuteCodeArgs {code} = do
  result <- executeCode ExecuteCodeOptions {executeCodeInput = code}
  pure $ renderToolRun renderExecuteCode result
