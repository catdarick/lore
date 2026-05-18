module CreateTemporalModuleSpec
  ( spec,
  )
where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as J
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Lore.Mcp.Tools.CreateTemporalModule (createTemporalModuleTool)
import Lore.Mcp.Tools.ExecuteCode (executeCodeTool)
import Lore.Mcp.Tools.ReloadHomeModules (reloadHomeModulesTool)
import McpTestSupport (callToolWithArgs, callToolWithoutArgs, fixtureLoreMcpAtWithCache, withFixtureCopy)
import System.Directory (doesFileExist)
import System.FilePath (takeBaseName)
import Test.Hspec

spec :: Spec
spec = do
  describe "createTemporalModule" do
    it "creates unique temporary modules and returns existing paths" do
      withFixtureCopy \fixtureRoot -> do
        (firstPath, secondPath) <-
          fixtureLoreMcpAtWithCache False fixtureRoot do
            firstPathText <- callToolWithoutArgs createTemporalModuleTool
            secondPathText <- callToolWithoutArgs createTemporalModuleTool
            pure
              ( temporalModulePathFromResponse firstPathText,
                temporalModulePathFromResponse secondPathText
              )

        firstPath `shouldNotBe` secondPath
        doesFileExist firstPath `shouldReturn` True
        doesFileExist secondPath `shouldReturn` True

    it "persists created modules in session and reloads them as targets" do
      withFixtureCopy \fixtureRoot -> do
        executionResult <-
          fixtureLoreMcpAtWithCache False fixtureRoot do
            modulePathText <- callToolWithoutArgs createTemporalModuleTool
            let modulePath = temporalModulePathFromResponse modulePathText
            liftIO $ writeModule modulePath "temporalValue" "99"
            _ <- callToolWithoutArgs reloadHomeModulesTool
            callToolWithArgs executeCodeTool (J.object ["code" J..= ("temporalValue" :: String)])

        executionResult `shouldSatisfy` T.isInfixOf "99"

writeModule :: FilePath -> String -> String -> IO ()
writeModule modulePath bindingName bindingValue =
  TIO.writeFile modulePath content
  where
    moduleName = takeBaseName modulePath
    content =
      T.unlines
        [ "module " <> T.pack moduleName <> " where",
          "",
          T.pack bindingName <> " :: Int",
          T.pack bindingName <> " = " <> T.pack bindingValue
        ]

temporalModulePathFromResponse :: T.Text -> FilePath
temporalModulePathFromResponse response =
  case T.lines response of
    firstLine : _ ->
      case T.stripPrefix "Temporal module initialized at: " firstLine of
        Just path -> T.unpack (T.strip path)
        Nothing -> error ("Unexpected createTemporalModule response header: " <> T.unpack firstLine)
    [] -> error "Unexpected empty createTemporalModule response"
