module ResolveInstanceSpec
  ( spec,
  )
where

import qualified Data.Aeson as J
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Lore.Mcp.Tools.ResolveInstance (resolveInstanceTool)
import McpTestSupport
  ( callToolWithArgs,
    fixtureLoreMcp,
    fixtureLoreMcpAtWithCache,
    loadFixtureHomeModules,
    withFixtureCopy,
  )
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec =
  describe "resolveInstance" do
    it "resolves direct instance queries" do
      withFixtureCopy \fixtureRoot -> do
        addDirectInstanceFixture fixtureRoot
        result <-
          fixtureLoreMcpAtWithCache False fixtureRoot do
            loadFixtureHomeModules
            callToolWithArgs resolveInstanceTool (resolveInstanceArgs "TestInstance.Direct.Render TestInstance.Direct.Foo")

        result `shouldContainText` "Selected instance:"
        result `shouldContainText` "instance Render Foo where"

    it "accepts optional instance prefix in query text" do
      withFixtureCopy \fixtureRoot -> do
        addDirectInstanceFixture fixtureRoot
        result <-
          fixtureLoreMcpAtWithCache False fixtureRoot do
            loadFixtureHomeModules
            callToolWithArgs resolveInstanceTool (resolveInstanceArgs "instance TestInstance.Direct.Render TestInstance.Direct.Foo")

        result `shouldContainText` "Selected instance:"

    it "resolves selected generic instances for concrete type queries" do
      withFixtureCopy \fixtureRoot -> do
        addGenericInstanceFixture fixtureRoot
        result <-
          fixtureLoreMcpAtWithCache False fixtureRoot do
            loadFixtureHomeModules
            callToolWithArgs resolveInstanceTool (resolveInstanceArgs "TestInstance.Generic.Render TestInstance.Generic.Foo")

        result `shouldContainText` "Selected instance:"
        result `shouldContainText` "instance Show a => Render a where"
        result `shouldContainText` "render a = show a"
        result `shouldContainText` "Instantiated as:\n- "
        result `shouldContainText` "~ Foo"

    it "returns selected instance even when context cannot be resolved" do
      withFixtureCopy \fixtureRoot -> do
        addUnsatisfiedGenericInstanceFixture fixtureRoot
        result <-
          fixtureLoreMcpAtWithCache False fixtureRoot do
            loadFixtureHomeModules
            callToolWithArgs resolveInstanceTool (resolveInstanceArgs "TestInstance.Unsatisfied.Render TestInstance.Unsatisfied.Foo")

        result `shouldContainText` "Selected instance:"
        result `shouldContainText` "instance Show a => Render a where"
        result `shouldContainText` "Required constraints:\n- Show Foo"
        result `shouldContainText` "Unresolved:\n- Show Foo"

    it "resolves parenthesized type applications" do
      withFixtureCopy \fixtureRoot -> do
        addGenericInstanceFixture fixtureRoot
        result <-
          fixtureLoreMcpAtWithCache False fixtureRoot do
            loadFixtureHomeModules
            callToolWithArgs resolveInstanceTool (resolveInstanceArgs "TestInstance.Generic.Render (Maybe TestInstance.Generic.Foo)")

        result `shouldContainText` "Selected instance:"
        result `shouldContainText` "instance Show a => Render a where"
        result `shouldContainText` "render a = show a"

    it "resolves multiparam class instances" do
      withFixtureCopy \fixtureRoot -> do
        addMultiParamInstanceFixture fixtureRoot
        result <-
          fixtureLoreMcpAtWithCache False fixtureRoot do
            loadFixtureHomeModules
            callToolWithArgs resolveInstanceTool (resolveInstanceArgs "TestInstance.MultiParam.Convert TestInstance.MultiParam.TypeOne TestInstance.MultiParam.TypeTwo")

        result `shouldContainText` "Selected instance:"
        result `shouldContainText` "instance Convert TypeOne TypeTwo where"
        result `shouldContainText` "convert _ = TypeTwo"

    it "selects the more specific OVERLAPPING instance when multiple heads match" do
      withFixtureCopy \fixtureRoot -> do
        addOverlappingInstanceFixture fixtureRoot
        result <-
          fixtureLoreMcpAtWithCache False fixtureRoot do
            loadFixtureHomeModules
            callToolWithArgs resolveInstanceTool (resolveInstanceArgs "TestInstance.Overlap.Render TestInstance.Overlap.Foo")

        result `shouldContainText` "Selected instance:"
        result `shouldContainText` "instance {-# OVERLAPPING #-} Render Foo where"
        result `shouldContainText` "render _ = \"foo-specific\""

    it "selects the OVERLAPPABLE fallback when no specific overlap exists" do
      withFixtureCopy \fixtureRoot -> do
        addOverlappingInstanceFixture fixtureRoot
        result <-
          fixtureLoreMcpAtWithCache False fixtureRoot do
            loadFixtureHomeModules
            callToolWithArgs resolveInstanceTool (resolveInstanceArgs "TestInstance.Overlap.Render TestInstance.Overlap.Bar")

        result `shouldContainText` "Selected instance:"
        result `shouldContainText` "instance {-# OVERLAPPABLE #-} Show a => Render a where"
        result `shouldContainText` "render a = show a"

    it "resolves qualified type names across modules" do
      withFixtureCopy \fixtureRoot -> do
        addQualifiedInstanceFixture fixtureRoot
        result <-
          fixtureLoreMcpAtWithCache False fixtureRoot do
            loadFixtureHomeModules
            callToolWithArgs resolveInstanceTool (resolveInstanceArgs "Instances.Render Types.Foo")

        result `shouldContainText` "Selected instance:"
        result `shouldContainText` "instance Render Types.Foo where"
        result `shouldContainText` "render _ = \"foo\""

    it "resolves non-exported home-module symbols when uniquely qualified" do
      withFixtureCopy \fixtureRoot -> do
        addNonExportedHomeSymbolFixture fixtureRoot
        result <-
          fixtureLoreMcpAtWithCache False fixtureRoot do
            loadFixtureHomeModules
            callToolWithArgs resolveInstanceTool (resolveInstanceArgs "TestInstance.NonExported.Render TestInstance.NonExported.Hidden")

        result `shouldContainText` "Selected instance:"
        result `shouldContainText` "instance Render Hidden where"
        result `shouldContainText` "render _ = \"hidden\""

    it "returns a clear error for malformed queries" do
      result <-
        fixtureLoreMcp do
          loadFixtureHomeModules
          callToolWithArgs resolveInstanceTool (resolveInstanceArgs "Render (Maybe Foo")

      result `shouldContainText` "Unsupported instance query \"instance Render (Maybe Foo\":"

    it "returns selected head and module when source is unavailable" do
      result <-
        fixtureLoreMcp do
          loadFixtureHomeModules
          callToolWithArgs resolveInstanceTool (resolveInstanceArgs "Show Int")

      result `shouldContainText` "Selected instance:"
      result `shouldContainText` "instance Show Int"

    it "resolves external-package types outside base/prelude" do
      result <-
        fixtureLoreMcp do
          loadFixtureHomeModules
          callToolWithArgs resolveInstanceTool (resolveInstanceArgs "Ord (Data.Set.Set Int)")

      result `shouldContainText` "Selected instance:"
      result `shouldContainText` "Ord (Set a)"

    it "reports unresolved class symbols before GHC typechecking" do
      result <-
        fixtureLoreMcp do
          loadFixtureHomeModules
          callToolWithArgs resolveInstanceTool (resolveInstanceArgs "ToJSON Integer")

      result `shouldContainText` "No symbols found for \"ToJSON\"."

    it "keeps qualified out-of-scope symbol names in GHC diagnostics" do
      result <-
        fixtureLoreMcp do
          loadFixtureHomeModules
          callToolWithArgs resolveInstanceTool (resolveInstanceArgs "Ord (Data.Set.Missing Int)")

      result `shouldContainText` "GHC rejected the resolved class application type:"
      result `shouldContainText` "Not in scope"
      result `shouldContainText` "Data.Set.Missing"

    it "returns GHC errors for missing qualified modules" do
      result <-
        fixtureLoreMcp do
          loadFixtureHomeModules
          callToolWithArgs resolveInstanceTool (resolveInstanceArgs "Ord (Missing.Module.Type Int)")

      result `shouldContainText` "GHC rejected the resolved class application type:"
      result `shouldContainText` "Could not find module"

    it "returns GHC type errors after successful name resolution" do
      withFixtureCopy \fixtureRoot -> do
        addGenericInstanceFixture fixtureRoot
        result <-
          fixtureLoreMcpAtWithCache False fixtureRoot do
            loadFixtureHomeModules
            callToolWithArgs resolveInstanceTool (resolveInstanceArgs "TestInstance.Generic.Render (Maybe)")

        result `shouldContainText` "GHC rejected the resolved class application type:"

    it "fails unresolved unqualified names before GHC typechecking" do
      withFixtureCopy \fixtureRoot -> do
        addDirectInstanceFixture fixtureRoot
        result <-
          fixtureLoreMcpAtWithCache False fixtureRoot do
            loadFixtureHomeModules
            callToolWithArgs resolveInstanceTool (resolveInstanceArgs "TestInstance.Direct.Render MissingType")

        result `shouldContainText` "No symbols found for \"MissingType\"."

    it "auto-imports unambiguous home modules for class and type args" do
      withFixtureCopy \fixtureRoot -> do
        addAutoImportInstanceFixture fixtureRoot
        result <-
          fixtureLoreMcpAtWithCache False fixtureRoot do
            loadFixtureHomeModules
            callToolWithArgs resolveInstanceTool (resolveInstanceArgs "AutoRender AutoFoo")

        result `shouldContainText` "Selected instance:"
        result `shouldContainText` "instance AutoRender AutoFoo where"

    it "reports ambiguity for unqualified type args like getDefinition does" do
      withFixtureCopy \fixtureRoot -> do
        addAmbiguousTypeNameFixture fixtureRoot
        result <-
          fixtureLoreMcpAtWithCache False fixtureRoot do
            loadFixtureHomeModules
            callToolWithArgs resolveInstanceTool (resolveInstanceArgs "Ambiguous.One.Render Text")

        result `shouldContainText` "The requested name \"Text\" is ambiguous."

    it "reports each unresolved type query candidate" do
      withFixtureCopy \fixtureRoot -> do
        addAmbiguousTypeNameFixture fixtureRoot
        result <-
          fixtureLoreMcpAtWithCache False fixtureRoot do
            loadFixtureHomeModules
            callToolWithArgs resolveInstanceTool (resolveInstanceArgs "Ambiguous.One.Render Text NonExistingSymbol")

        result `shouldContainText` "The requested name \"Text\" is ambiguous."
        result `shouldContainText` "No symbols found for \"NonExistingSymbol\"."

    it "reports ambiguity for Text without requiring blanket internal-module filtering" do
      -- This intentionally remains ambiguous. We do not globally hide internal
      -- package candidates here because that can make diagnostics misleading.
      withFixtureCopy \fixtureRoot -> do
        addTextDependencyToFixture fixtureRoot
        result <-
          fixtureLoreMcpAtWithCache False fixtureRoot do
            loadFixtureHomeModules
            callToolWithArgs resolveInstanceTool (resolveInstanceArgs "Show Text")

        result `shouldContainText` "The requested name \"Text\" is ambiguous."

resolveInstanceArgs :: Text -> J.Value
resolveInstanceArgs query =
  J.object
    [ "query" J..= query
    ]

addDirectInstanceFixture :: FilePath -> IO ()
addDirectInstanceFixture fixtureRoot = do
  let moduleDir = fixtureRoot </> "src" </> "TestInstance"
      moduleFile = moduleDir </> "Direct.hs"
  createDirectoryIfMissing True moduleDir
  TIO.writeFile moduleFile directInstanceFixtureModuleSource

addGenericInstanceFixture :: FilePath -> IO ()
addGenericInstanceFixture fixtureRoot = do
  let moduleDir = fixtureRoot </> "src" </> "TestInstance"
      moduleFile = moduleDir </> "Generic.hs"
  createDirectoryIfMissing True moduleDir
  TIO.writeFile moduleFile genericInstanceFixtureModuleSource

addUnsatisfiedGenericInstanceFixture :: FilePath -> IO ()
addUnsatisfiedGenericInstanceFixture fixtureRoot = do
  let moduleDir = fixtureRoot </> "src" </> "TestInstance"
      moduleFile = moduleDir </> "Unsatisfied.hs"
  createDirectoryIfMissing True moduleDir
  TIO.writeFile moduleFile unsatisfiedGenericInstanceFixtureModuleSource

addMultiParamInstanceFixture :: FilePath -> IO ()
addMultiParamInstanceFixture fixtureRoot = do
  let moduleDir = fixtureRoot </> "src" </> "TestInstance"
      moduleFile = moduleDir </> "MultiParam.hs"
  createDirectoryIfMissing True moduleDir
  TIO.writeFile moduleFile multiParamInstanceFixtureModuleSource

addOverlappingInstanceFixture :: FilePath -> IO ()
addOverlappingInstanceFixture fixtureRoot = do
  let moduleDir = fixtureRoot </> "src" </> "TestInstance"
      moduleFile = moduleDir </> "Overlap.hs"
  createDirectoryIfMissing True moduleDir
  TIO.writeFile moduleFile overlappingInstanceFixtureModuleSource

addQualifiedInstanceFixture :: FilePath -> IO ()
addQualifiedInstanceFixture fixtureRoot = do
  let typesFile = fixtureRoot </> "src" </> "Types.hs"
      instancesFile = fixtureRoot </> "src" </> "Instances.hs"
  TIO.writeFile typesFile qualifiedTypeFixtureTypesModuleSource
  TIO.writeFile instancesFile qualifiedTypeFixtureInstancesModuleSource

addNonExportedHomeSymbolFixture :: FilePath -> IO ()
addNonExportedHomeSymbolFixture fixtureRoot = do
  let moduleDir = fixtureRoot </> "src" </> "TestInstance"
      moduleFile = moduleDir </> "NonExported.hs"
  createDirectoryIfMissing True moduleDir
  TIO.writeFile moduleFile nonExportedHomeSymbolFixtureModuleSource

addAutoImportInstanceFixture :: FilePath -> IO ()
addAutoImportInstanceFixture fixtureRoot = do
  let typesFile = fixtureRoot </> "src" </> "AutoTypes.hs"
      instancesFile = fixtureRoot </> "src" </> "AutoInstances.hs"
  TIO.writeFile typesFile autoImportFixtureTypesModuleSource
  TIO.writeFile instancesFile autoImportFixtureInstancesModuleSource

addAmbiguousTypeNameFixture :: FilePath -> IO ()
addAmbiguousTypeNameFixture fixtureRoot = do
  let moduleOneFile = fixtureRoot </> "src" </> "Ambiguous" </> "One.hs"
      moduleTwoFile = fixtureRoot </> "src" </> "Ambiguous" </> "Two.hs"
  createDirectoryIfMissing True (fixtureRoot </> "src" </> "Ambiguous")
  TIO.writeFile moduleOneFile ambiguousFixtureModuleOneSource
  TIO.writeFile moduleTwoFile ambiguousFixtureModuleTwoSource

addTextDependencyToFixture :: FilePath -> IO ()
addTextDependencyToFixture fixtureRoot = do
  let packageYamlPath = fixtureRoot </> "package.yaml"
  original <- TIO.readFile packageYamlPath
  let withTextDependency =
        if "- text" `T.isInfixOf` original
          then original
          else T.replace "- containers" "- containers\n- text" original
  TIO.writeFile packageYamlPath withTextDependency

directInstanceFixtureModuleSource :: Text
directInstanceFixtureModuleSource =
  T.unlines
    [ "module TestInstance.Direct",
      "  ( Render (..),",
      "    Foo (..)",
      "  ) where",
      "",
      "class Render a where",
      "  render :: a -> String",
      "",
      "data Foo = Foo",
      "",
      "instance Render Foo where",
      "  render _ = \"foo\""
    ]

genericInstanceFixtureModuleSource :: Text
genericInstanceFixtureModuleSource =
  T.unlines
    [ "{-# LANGUAGE FlexibleInstances #-}",
      "{-# LANGUAGE UndecidableInstances #-}",
      "",
      "module TestInstance.Generic",
      "  ( Render (..),",
      "    Foo (..)",
      "  ) where",
      "",
      "class Render a where",
      "  render :: a -> String",
      "",
      "data Foo = Foo",
      "  deriving (Show)",
      "",
      "instance Show a => Render a where",
      "  render a = show a"
    ]

unsatisfiedGenericInstanceFixtureModuleSource :: Text
unsatisfiedGenericInstanceFixtureModuleSource =
  T.unlines
    [ "{-# LANGUAGE FlexibleInstances #-}",
      "{-# LANGUAGE UndecidableInstances #-}",
      "",
      "module TestInstance.Unsatisfied",
      "  ( Render (..),",
      "    Foo (..)",
      "  ) where",
      "",
      "class Render a where",
      "  render :: a -> String",
      "",
      "data Foo = Foo",
      "",
      "instance Show a => Render a where",
      "  render a = show a"
    ]

multiParamInstanceFixtureModuleSource :: Text
multiParamInstanceFixtureModuleSource =
  T.unlines
    [ "{-# LANGUAGE MultiParamTypeClasses #-}",
      "",
      "module TestInstance.MultiParam",
      "  ( Convert (..),",
      "    TypeOne (..),",
      "    TypeTwo (..)",
      "  ) where",
      "",
      "class Convert a b where",
      "  convert :: a -> b",
      "",
      "data TypeOne = TypeOne",
      "",
      "data TypeTwo = TypeTwo",
      "",
      "instance Convert TypeOne TypeTwo where",
      "  convert _ = TypeTwo"
    ]

overlappingInstanceFixtureModuleSource :: Text
overlappingInstanceFixtureModuleSource =
  T.unlines
    [ "{-# LANGUAGE FlexibleInstances #-}",
      "{-# LANGUAGE UndecidableInstances #-}",
      "",
      "module TestInstance.Overlap",
      "  ( Render (..),",
      "    Foo (..),",
      "    Bar (..)",
      "  ) where",
      "",
      "class Render a where",
      "  render :: a -> String",
      "",
      "data Foo = Foo",
      "",
      "data Bar = Bar",
      "  deriving (Show)",
      "",
      "instance {-# OVERLAPPABLE #-} Show a => Render a where",
      "  render a = show a",
      "",
      "instance {-# OVERLAPPING #-} Render Foo where",
      "  render _ = \"foo-specific\""
    ]

qualifiedTypeFixtureTypesModuleSource :: Text
qualifiedTypeFixtureTypesModuleSource =
  T.unlines
    [ "module Types",
      "  ( Foo (..)",
      "  ) where",
      "",
      "data Foo = Foo"
    ]

qualifiedTypeFixtureInstancesModuleSource :: Text
qualifiedTypeFixtureInstancesModuleSource =
  T.unlines
    [ "module Instances",
      "  ( Render (..)",
      "  ) where",
      "",
      "import qualified Types",
      "",
      "class Render a where",
      "  render :: a -> String",
      "",
      "instance Render Types.Foo where",
      "  render _ = \"foo\""
    ]

nonExportedHomeSymbolFixtureModuleSource :: Text
nonExportedHomeSymbolFixtureModuleSource =
  T.unlines
    [ "module TestInstance.NonExported",
      "  ( Render (..)",
      "  ) where",
      "",
      "class Render a where",
      "  render :: a -> String",
      "",
      "data Hidden = Hidden",
      "",
      "instance Render Hidden where",
      "  render _ = \"hidden\""
    ]

autoImportFixtureTypesModuleSource :: Text
autoImportFixtureTypesModuleSource =
  T.unlines
    [ "module AutoTypes",
      "  ( AutoFoo (..)",
      "  ) where",
      "",
      "data AutoFoo = AutoFoo"
    ]

autoImportFixtureInstancesModuleSource :: Text
autoImportFixtureInstancesModuleSource =
  T.unlines
    [ "module AutoInstances",
      "  ( AutoRender (..)",
      "  ) where",
      "",
      "import AutoTypes",
      "",
      "class AutoRender a where",
      "  autoRender :: a -> String",
      "",
      "instance AutoRender AutoFoo where",
      "  autoRender _ = \"autof\""
    ]

ambiguousFixtureModuleOneSource :: Text
ambiguousFixtureModuleOneSource =
  T.unlines
    [ "module Ambiguous.One",
      "  ( Render (..),",
      "    Text (..)",
      "  ) where",
      "",
      "class Render a where",
      "  render :: a -> String",
      "",
      "data Text = Text",
      "",
      "instance Render Text where",
      "  render _ = \"one\""
    ]

ambiguousFixtureModuleTwoSource :: Text
ambiguousFixtureModuleTwoSource =
  T.unlines
    [ "module Ambiguous.Two",
      "  ( Text (..)",
      "  ) where",
      "",
      "data Text = Text"
    ]

shouldContainText :: Text -> Text -> Expectation
shouldContainText haystack needle =
  haystack `shouldSatisfy` T.isInfixOf needle
