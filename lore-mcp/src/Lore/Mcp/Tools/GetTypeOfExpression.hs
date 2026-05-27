module Lore.Mcp.Tools.GetTypeOfExpression
  ( getTypeOfExpressionTool,
  )
where

import qualified Data.Aeson as J
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import Lore (MonadLore)
import Lore.Mcp.Internal.Annotated (Description, Example, Field, FieldType (..), WithMeta)
import Lore.Mcp.Internal.Tool (SomeTool (..), ToolWithArgs (..))
import Lore.Tools.GetTypeOfExpression
  ( GetTypeOfExpressionOptions (..),
    getTypeOfExpression,
    renderTypeExpressionOutput,
  )
import Lore.Tools.Render.Doc (LoreDoc, ToLoreDoc (toLoreDoc))
import Lore.Tools.Result
  ( ToolRun (..),
  )

newtype GetTypeOfExpressionArgs (fieldType :: FieldType) = GetTypeOfExpressionArgs
  { expression ::
      Field fieldType Text
        `WithMeta` '[ Description "Haskell expression to infer in the current interpreter context (expressions only; declarations and statements are not supported).",
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
        description = Just "Infer the type of a Haskell expression in the current project interpreter context. Run reloadHomeModules first.",
        handler = getTypeOfExpressionHandler
      }

getTypeOfExpressionHandler :: (MonadLore m) => GetTypeOfExpressionArgs 'ValueType -> m LoreDoc
getTypeOfExpressionHandler GetTypeOfExpressionArgs {expression} = do
  result <- getTypeOfExpression GetTypeOfExpressionOptions {typeOfExpressionInput = expression}
  pure $
    case result of
      ToolRunBlocked blocked ->
        toLoreDoc blocked
      ToolRunReady output ->
        renderTypeExpressionOutput output
