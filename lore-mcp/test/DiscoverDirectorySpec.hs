module DiscoverDirectorySpec
  ( spec,
  )
where

import qualified Data.Aeson as J
import qualified Data.Text as T
import Lore.Mcp.Tools.DiscoverDirectory (discoverDirectoryTool)
import McpTestSupport (callToolWithArgs, fixtureLoreMcp, fixtureLoreMcpAtWithCache, withFixtureCopy)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec =
  describe "discoverDirectory" do
    it "renders a nested ASCII tree for a relative project path" do
      discoveryResult <-
        fixtureLoreMcp do
          callToolWithArgs discoverDirectoryTool (directoryTreeArgs ".")

      discoveryResult `shouldContainText` "./"
      discoveryResult `shouldContainText` "├── .lore-work-test-mcp/"
      discoveryResult `shouldNotContainText` "│   ├── hi/"
      discoveryResult `shouldContainText` "├── src/"
      discoveryResult `shouldContainText` "│   ├── Demo/Support.hs"
      discoveryResult `shouldContainText` "│   └── Demo.hs"

    it "renders a nested tree when rooted in a subdirectory" do
      discoveryResult <-
        fixtureLoreMcp do
          callToolWithArgs discoverDirectoryTool (directoryTreeArgs "src/Demo")

      discoveryResult `shouldContainText` "src/Demo/"
      discoveryResult `shouldContainText` "└── Support.hs"

    it "preserves exact unicode tree rendering for a small fixture" do
      discoveryResult <-
        withFixtureCopy \fixtureRoot -> do
          createDirectoryIfMissing True (fixtureRoot </> "sample-tree" </> "src")
          createDirectoryIfMissing True (fixtureRoot </> "sample-tree" </> "test")
          writeFile (fixtureRoot </> "sample-tree" </> "src" </> "Foo.hs") "module Foo where\n"
          fixtureLoreMcpAtWithCache False fixtureRoot do
            callToolWithArgs discoverDirectoryTool (directoryTreeArgs "sample-tree")

      discoveryResult
        `shouldBe` T.unlines
          [ "sample-tree/",
            "├── test/",
            "└── src/Foo.hs"
          ]

    it "returns directory validation errors for missing paths" do
      discoveryResult <-
        fixtureLoreMcp do
          callToolWithArgs discoverDirectoryTool (directoryTreeArgs "does-not-exist")

      discoveryResult `shouldContainText` "Directory does not exist: does-not-exist"

    it "does not expand gitignored directories by default" do
      discoveryResult <-
        withFixtureCopy \fixtureRoot -> do
          appendFile (fixtureRoot </> ".gitignore") "\ngenerated/\n"
          createDirectoryIfMissing True (fixtureRoot </> "generated")
          writeFile (fixtureRoot </> "generated" </> "inside.txt") "x\n"
          fixtureLoreMcpAtWithCache False fixtureRoot do
            callToolWithArgs discoverDirectoryTool (directoryTreeArgs ".")

      discoveryResult `shouldContainText` "├── generated/"
      discoveryResult `shouldNotContainText` "│   └── inside.txt"

    it "still expands children when requested root itself is gitignored" do
      discoveryResult <-
        withFixtureCopy \fixtureRoot -> do
          appendFile (fixtureRoot </> ".gitignore") "\nsrc/dir/\n"
          createDirectoryIfMissing True (fixtureRoot </> "src" </> "dir" </> "non-plan" </> "child-dir")
          writeFile (fixtureRoot </> "src" </> "dir" </> "non-plan" </> "child-file") "x\n"
          createDirectoryIfMissing True (fixtureRoot </> "src" </> "dir" </> "plain" </> "child-dir")
          writeFile (fixtureRoot </> "src" </> "dir" </> "plain" </> "child-dir" </> "child-file-1") "x\n"
          writeFile (fixtureRoot </> "src" </> "dir" </> "plain" </> "child-dir" </> "child-file-2") "x\n"
          fixtureLoreMcpAtWithCache False fixtureRoot do
            callToolWithArgs discoverDirectoryTool (directoryTreeArgs "src/dir")

      discoveryResult `shouldContainText` "src/dir/"
      discoveryResult `shouldContainText` "├── non-plan/"
      discoveryResult `shouldContainText` "│   ├── child-dir/"
      discoveryResult `shouldContainText` "│   └── child-file"
      discoveryResult `shouldContainText` "└── plain/child-dir/"
      discoveryResult `shouldContainText` "    ├── child-file-1"
      discoveryResult `shouldContainText` "    └── child-file-2"
      discoveryResult `shouldNotContainText` "non-plan/ ("
      discoveryResult `shouldNotContainText` "plain/child-dir/ ("

    it "collapses non-branching directory paths into single rendered elements" do
      discoveryResult <-
        withFixtureCopy \fixtureRoot -> do
          createDirectoryIfMissing True (fixtureRoot </> "sample-tree" </> "non-plan" </> "child-dir")
          writeFile (fixtureRoot </> "sample-tree" </> "non-plan" </> "child-file") "x\n"
          createDirectoryIfMissing True (fixtureRoot </> "sample-tree" </> "plain" </> "child-dir")
          writeFile (fixtureRoot </> "sample-tree" </> "plain" </> "child-dir" </> "child-file-1") "x\n"
          writeFile (fixtureRoot </> "sample-tree" </> "plain" </> "child-dir" </> "child-file-2") "x\n"
          fixtureLoreMcpAtWithCache False fixtureRoot do
            callToolWithArgs discoverDirectoryTool (directoryTreeArgs "sample-tree")

      discoveryResult `shouldContainText` "sample-tree/"
      discoveryResult `shouldContainText` "├── non-plan/"
      discoveryResult `shouldContainText` "│   ├── child-dir/"
      discoveryResult `shouldContainText` "│   └── child-file"
      discoveryResult `shouldContainText` "└── plain/child-dir/"
      discoveryResult `shouldContainText` "    ├── child-file-1"
      discoveryResult `shouldContainText` "    └── child-file-2"

    it "renders singleton omitted file names directly instead of omission markers" do
      discoveryResult <-
        withFixtureCopy \fixtureRoot -> do
          createDirectoryIfMissing True (fixtureRoot </> "skills" </> "ab-tests")
          writeFile (fixtureRoot </> "skills" </> "ab-tests" </> "readme.md") "hi\n"
          createDirectoryIfMissing True (fixtureRoot </> "zzz")
          mapM_
            (\index -> writeFile (fixtureRoot </> "zzz" </> ("filler-" <> show index <> ".txt")) "x\n")
            [1 .. 400 :: Int]
          fixtureLoreMcpAtWithCache False fixtureRoot do
            callToolWithArgs discoverDirectoryTool (directoryTreeArgs ".")

      discoveryResult `shouldContainText` "skills/ab-tests/readme.md"
      discoveryResult `shouldNotContainText` "... omitted: 1 files"

    it "applies hardcoded noisy directory trimming while keeping root fully expanded" do
      discoveryResult <-
        withFixtureCopy \fixtureRoot -> do
          createDirectoryIfMissing True (fixtureRoot </> "noise-root" </> "nested-noisy")
          mapM_
            (\index -> writeFile (fixtureRoot </> "noise-root" </> ("root-file-" <> pad index <> ".txt")) "x\n")
            [1 .. 25 :: Int]
          mapM_
            (\index -> writeFile (fixtureRoot </> "noise-root" </> "nested-noisy" </> ("nested-file-" <> pad index <> ".txt")) "x\n")
            [1 .. 25 :: Int]
          fixtureLoreMcpAtWithCache False fixtureRoot do
            callToolWithArgs discoverDirectoryTool (directoryTreeArgs "noise-root")

      discoveryResult `shouldContainText` "noise-root/"
      discoveryResult `shouldContainText` "├── nested-noisy/"
      discoveryResult `shouldContainText` "│   ├── nested-file-001.txt"
      discoveryResult `shouldContainText` "│   ├── nested-file-002.txt"
      discoveryResult `shouldContainText` "│   ├── nested-file-003.txt"
      discoveryResult `shouldContainText` "│   ├── ... omitted: 19 files"
      discoveryResult `shouldContainText` "│   ├── nested-file-023.txt"
      discoveryResult `shouldContainText` "│   ├── nested-file-024.txt"
      discoveryResult `shouldContainText` "│   └── nested-file-025.txt"
      discoveryResult `shouldContainText` "├── root-file-010.txt"
      discoveryResult `shouldContainText` "└── root-file-025.txt"

    it "prioritizes useful directories for expansion under tight remaining budget" do
      discoveryResult <-
        withFixtureCopy \fixtureRoot -> do
          createDirectoryIfMissing True (fixtureRoot </> "priority-root")

          mapM_
            ( \dirIndex -> do
                let dirName = "a" <> pad dirIndex
                createDirectoryIfMissing True (fixtureRoot </> "priority-root" </> dirName)
                mapM_
                  (\fileIndex -> writeFile (fixtureRoot </> "priority-root" </> dirName </> ("a-file-" <> pad fileIndex <> ".txt")) "x\n")
                  [1 .. 19 :: Int]
            )
            [1 .. 10 :: Int]

          createDirectoryIfMissing True (fixtureRoot </> "priority-root" </> "src")
          mapM_
            (\fileIndex -> writeFile (fixtureRoot </> "priority-root" </> "src" </> ("src-file-" <> pad fileIndex <> ".hs")) "x\n")
            [1 .. 19 :: Int]

          createDirectoryIfMissing True (fixtureRoot </> "priority-root" </> "migrations")
          mapM_
            (\fileIndex -> writeFile (fixtureRoot </> "priority-root" </> "migrations" </> ("migration-" <> pad fileIndex <> ".sql")) "x\n")
            [1 .. 19 :: Int]

          fixtureLoreMcpAtWithCache False fixtureRoot do
            callToolWithArgs discoverDirectoryTool (directoryTreeArgs "priority-root")

      discoveryResult `shouldContainText` "src-file-001.hs"
      discoveryResult `shouldContainText` "migration-001.sql"
      discoveryResult `shouldContainText` "a008/ (0 dirs, 19 files)"

    it "renders non-noisy budget omissions inline on directory labels" do
      discoveryResult <-
        withFixtureCopy \fixtureRoot -> do
          createDirectoryIfMissing True (fixtureRoot </> "budget-root")
          mapM_
            ( \dirIndex -> do
                let dirName = "d" <> pad dirIndex
                createDirectoryIfMissing True (fixtureRoot </> "budget-root" </> dirName)
                mapM_
                  (\fileIndex -> writeFile (fixtureRoot </> "budget-root" </> dirName </> ("f" <> pad fileIndex <> ".txt")) "x\n")
                  [1 .. 19 :: Int]
            )
            [1 .. 200 :: Int]
          fixtureLoreMcpAtWithCache False fixtureRoot do
            callToolWithArgs discoverDirectoryTool (directoryTreeArgs "budget-root")

      discoveryResult `shouldContainText` "├── d001/ (0 dirs, 19 files)"
      discoveryResult `shouldContainText` "├── d150/ (0 dirs, 19 files)"
      discoveryResult `shouldContainText` "└── ... omitted: 50 dirs"
      discoveryResult `shouldNotContainText` "d150/\n│   ├── ... omitted:"

    it "renders noisy omissions inline when no noisy children are rendered" do
      discoveryResult <-
        withFixtureCopy \fixtureRoot -> do
          createDirectoryIfMissing True (fixtureRoot </> "hybrid-root")
          mapM_
            ( \dirIndex -> do
                let dirName = "a" <> pad dirIndex
                createDirectoryIfMissing True (fixtureRoot </> "hybrid-root" </> dirName)
                mapM_
                  (\fileIndex -> writeFile (fixtureRoot </> "hybrid-root" </> dirName </> ("f" <> pad fileIndex <> ".txt")) "x\n")
                  [1 .. 10 :: Int]
            )
            [1 .. 149 :: Int]
          createDirectoryIfMissing True (fixtureRoot </> "hybrid-root" </> "zzz-noisy")
          mapM_
            (\fileIndex -> writeFile (fixtureRoot </> "hybrid-root" </> "zzz-noisy" </> ("n" <> pad fileIndex <> ".txt")) "x\n")
            [1 .. 30 :: Int]
          fixtureLoreMcpAtWithCache False fixtureRoot do
            callToolWithArgs discoverDirectoryTool (directoryTreeArgs "hybrid-root")

      discoveryResult `shouldContainText` "└── zzz-noisy/ (0 dirs, 30 files)"
      discoveryResult `shouldNotContainText` "zzz-noisy/\n    └── ... omitted:"

directoryTreeArgs :: FilePath -> J.Value
directoryTreeArgs path =
  J.object
    [ "path" J..= path
    ]

pad :: Int -> String
pad value
  | value < 10 = "00" <> show value
  | value < 100 = "0" <> show value
  | otherwise = show value

shouldContainText :: T.Text -> T.Text -> Expectation
shouldContainText actual expected =
  if T.isInfixOf expected actual
    then pure ()
    else
      expectationFailure
        ( "Missing expected snippet: "
            <> T.unpack expected
            <> "\n\nFull output:\n"
            <> T.unpack actual
        )

shouldNotContainText :: T.Text -> T.Text -> Expectation
shouldNotContainText actual unexpected =
  if T.isInfixOf unexpected actual
    then
      expectationFailure
        ( "Unexpected snippet found: "
            <> T.unpack unexpected
            <> "\n\nFull output:\n"
            <> T.unpack actual
        )
    else pure ()
