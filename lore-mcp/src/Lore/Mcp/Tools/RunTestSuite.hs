module Lore.Mcp.Tools.RunTestSuite
  ( runTestSuiteTool,
  )
where

import qualified Data.Aeson as J
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import Lore (MonadLore)
import Lore.Mcp.Internal.Annotated (Description, Example, Field, FieldType (..), WithMeta)
import Lore.Mcp.Internal.Tool (SomeTool (..), ToolWithArgs (..))
import Lore.Tools.Render.Doc (LoreDoc, ToLoreDoc (toLoreDoc))
import Lore.Tools.Result
  ( ToolRun (..),
  )
import Lore.Tools.RunTestSuite
  ( RunTestSuiteToolOptions (..),
  )
import qualified Lore.Tools.RunTestSuite as ToolsRunTestSuite

data RunTestSuiteArgs (fieldType :: FieldType) = RunTestSuiteArgs
  { package ::
      Field fieldType (Maybe Text)
        `WithMeta` '[ Description "Optional package name to limit test execution. If omitted, tests from all discovered packages are executed."
                    ],
    testArgs ::
      Field fieldType (Maybe Text)
        `WithMeta` '[ Description "Optional arguments to be forwarded to the test suite.",
                      Example "--match \"some test name\""
                    ]
  }
  deriving stock (Generic)

instance J.FromJSON (RunTestSuiteArgs 'ValueType)

instance ToSchema (RunTestSuiteArgs 'MetadataType)

runTestSuiteTool :: (MonadLore m) => SomeTool m
runTestSuiteTool =
  SomeToolWithArgs
    ToolWithArgs
      { name = "runTestSuite",
        description = Just "Runs the test suite. Equivalent to invoking 'cabal test' or 'stack test' in the terminal.",
        handler = runTestSuiteHandler
      }

runTestSuiteHandler :: (MonadLore m) => RunTestSuiteArgs 'ValueType -> m LoreDoc
runTestSuiteHandler RunTestSuiteArgs {package, testArgs} = do
  result <-
    ToolsRunTestSuite.runTestSuite
      RunTestSuiteToolOptions
        { runTestSuitePackageFilter = package,
          runTestSuiteRawArgs = testArgs
        }
  pure $
    case result of
      ToolRunBlocked blocked ->
        toLoreDoc blocked
      ToolRunReady output ->
        output
