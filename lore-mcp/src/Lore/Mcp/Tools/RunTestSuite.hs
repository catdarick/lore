module Lore.Mcp.Tools.RunTestSuite
  ( runTestSuiteTool,
  )
where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as J
import Data.Maybe (catMaybes)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Lore (MonadLore)
import Lore.Mcp.Internal.Annotated (Description, Example, Field, FieldType (..), WithMeta)
import Lore.Mcp.Internal.Tool (SomeTool (..), ToolWithArgs (..), renderToolRun)
import Lore.Tools.Render.Doc (LoreDoc)
import Lore.Tools.Result
  (   )
import Lore.Tools.RunTestSuite
  ( RunTestSuiteToolOptions (..),
  )
import qualified Lore.Tools.RunTestSuite as ToolsRunTestSuite
import System.Environment (lookupEnv)

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
  defaultRawArgs <- liftIO (lookupEnv "LORE_MCP_DEFAULT_TEST_ARGS")
  result <-
    ToolsRunTestSuite.runTestSuite
      RunTestSuiteToolOptions
        { runTestSuitePackageFilter = package,
          runTestSuiteRawArgs = mergeRawArgs defaultRawArgs testArgs
        }
  pure $ renderToolRun id result

mergeRawArgs :: Maybe String -> Maybe Text -> Maybe Text
mergeRawArgs maybeDefaultRawArgs maybeExplicitRawArgs =
  case filteredArgs of
    [] -> Nothing
    args -> Just (T.intercalate " " args)
  where
    filteredArgs =
      filter (not . T.null) $
        T.strip <$> catMaybes [T.pack <$> maybeDefaultRawArgs, maybeExplicitRawArgs]
