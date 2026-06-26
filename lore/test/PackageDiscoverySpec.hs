module PackageDiscoverySpec (spec) where

import Control.Exception (bracket)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Lore.Internal.Package.Discovery
  ( discoverCabalPackageRoots,
    discoverStackPackageRoots,
  )
import Lore.Internal.Package.Materialize
  ( PackageMaterializeRunner (..),
    materializeCabalPackageFileIO,
    runHpackGeneratorWithProcess,
  )
import Lore.Internal.Package.Root (PackageRoot (..))
import Lore.Internal.ProjectProvider (ProjectProvider (..), detectProjectProvider)
import Lore.Internal.Session (preparePackageMaterializationBeforeEnvironmentProbeWithRunner)
import Lore.Logger (noLogHandle)
import System.Directory
  ( createDirectory,
    createDirectoryIfMissing,
    makeAbsolute,
    removeFile,
    removePathForcibly,
  )
import System.FilePath (normalise, takeDirectory, (</>))
import System.IO (hClose, openTempFile)
import Test.Hspec

spec :: Spec
spec = do
  describe "discoverStackPackageRoots" do
    it "defaults to current directory when stack.yaml has no packages stanza" do
      withTempProject \projectRoot -> do
        writeProjectFile projectRoot "stack.yaml" "resolver: lts-23.0\n"

        discoverStackPackageRoots projectRoot
          `shouldReturn` Right [mkRoot projectRoot Nothing]

    it "resolves explicit package.yaml entry to package root" do
      withTempProject \projectRoot -> do
        writeProjectFile projectRoot "stack.yaml" (unlines ["resolver: lts-23.0", "packages:", "- packages/foo/package.yaml"])
        writeProjectFile projectRoot "packages/foo/package.yaml" "name: foo\n"

        discoverStackPackageRoots projectRoot
          `shouldReturn` Right [mkRoot (projectRoot </> "packages/foo") Nothing]

    it "resolves explicit .cabal entry to package root with preferred cabal file" do
      withTempProject \projectRoot -> do
        writeProjectFile projectRoot "stack.yaml" (unlines ["resolver: lts-23.0", "packages:", "- packages/foo/foo.cabal"])
        writeProjectFile projectRoot "packages/foo/foo.cabal" "name: foo\nversion: 0.1.0.0\n"

        discoverStackPackageRoots projectRoot
          `shouldReturn` Right [mkRoot (projectRoot </> "packages/foo") (Just (projectRoot </> "packages/foo/foo.cabal"))]

    it "resolves directory entry to package root" do
      withTempProject \projectRoot -> do
        writeProjectFile projectRoot "stack.yaml" (unlines ["resolver: lts-23.0", "packages:", "- packages/foo"])
        createDirectoryIfMissing True (projectRoot </> "packages/foo")

        discoverStackPackageRoots projectRoot
          `shouldReturn` Right [mkRoot (projectRoot </> "packages/foo") Nothing]

  describe "discoverCabalPackageRoots" do
    it "returns project root when cabal.project is absent" do
      withTempProject \projectRoot -> do
        writeProjectFile projectRoot "foo.cabal" "name: foo\nversion: 0.1.0.0\n"

        discoverCabalPackageRoots projectRoot
          `shouldReturn` Right [mkRoot projectRoot Nothing]

    it "resolves directory entry from cabal.project" do
      withTempProject \projectRoot -> do
        writeProjectFile projectRoot "cabal.project" "packages: packages/foo\n"
        writeProjectFile projectRoot "packages/foo/foo.cabal" "name: foo\nversion: 0.1.0.0\n"

        discoverCabalPackageRoots projectRoot
          `shouldReturn` Right [mkRoot (projectRoot </> "packages/foo") Nothing]

    it "resolves .cabal file entry from cabal.project with preferred cabal file" do
      withTempProject \projectRoot -> do
        writeProjectFile projectRoot "cabal.project" "packages: packages/foo/foo.cabal\n"
        writeProjectFile projectRoot "packages/foo/foo.cabal" "name: foo\nversion: 0.1.0.0\n"

        discoverCabalPackageRoots projectRoot
          `shouldReturn` Right [mkRoot (projectRoot </> "packages/foo") (Just (projectRoot </> "packages/foo/foo.cabal"))]

    it "resolves package.yaml entry from cabal.project" do
      withTempProject \projectRoot -> do
        writeProjectFile projectRoot "cabal.project" "packages: packages/foo/package.yaml\n"
        writeProjectFile projectRoot "packages/foo/package.yaml" "name: foo\n"

        discoverCabalPackageRoots projectRoot
          `shouldReturn` Right [mkRoot (projectRoot </> "packages/foo") Nothing]

    it "expands wildcard .cabal entries from cabal.project and preserves preferred cabal files" do
      withTempProject \projectRoot -> do
        writeProjectFile projectRoot "cabal.project" "packages: packages/*/*.cabal\n"
        writeProjectFile projectRoot "packages/a/a.cabal" "name: a\nversion: 0.1.0.0\n"
        writeProjectFile projectRoot "packages/b/b.cabal" "name: b\nversion: 0.1.0.0\n"

        discoverCabalPackageRoots projectRoot
          `shouldReturn` Right
            [ mkRoot (projectRoot </> "packages/a") (Just (projectRoot </> "packages/a/a.cabal")),
              mkRoot (projectRoot </> "packages/b") (Just (projectRoot </> "packages/b/b.cabal"))
            ]

    it "expands directory wildcard and keeps only package-like directories" do
      withTempProject \projectRoot -> do
        writeProjectFile projectRoot "cabal.project" "packages: packages/*\n"
        writeProjectFile projectRoot "packages/a/a.cabal" "name: a\nversion: 0.1.0.0\n"
        writeProjectFile projectRoot "packages/b/package.yaml" "name: b\n"
        writeProjectFile projectRoot "packages/not-a-package/README.md" "nope\n"

        discoverCabalPackageRoots projectRoot
          `shouldReturn` Right
            [ mkRoot (projectRoot </> "packages/a") Nothing,
              mkRoot (projectRoot </> "packages/b") Nothing
            ]

  describe "materializeCabalPackageFileWithRunner" do
    it "returns top-level .cabal without running hpack when package.yaml is absent" do
      withTempProject \projectRoot -> do
        let packageRootPath = projectRoot </> "pkg"
        createDirectoryIfMissing True packageRootPath
        writeProjectFile projectRoot "pkg/foo.cabal" "name: foo\nversion: 0.1.0.0\n"

        hpackCalls <- newIORef ([] :: [FilePath])
        logs <- newIORef ([] :: [String])
        let runner =
              PackageMaterializeRunner
                { runHpackGenerator = \root -> do
                    modifyIORef' hpackCalls (root :)
                    pure (Right ())
                }

        result <-
          materializeCabalPackageFileIO
            runner
            (\msg -> modifyIORef' logs (msg :))
            id
            (mkRoot packageRootPath Nothing)

        result `shouldBe` Right (packageRootPath </> "foo.cabal")
        readIORef hpackCalls `shouldReturn` []
        readIORef logs `shouldReturn` []

    it "runs hpack once and returns generated .cabal for stack directory hpack package" do
      withTempProject \projectRoot -> do
        let packageRootPath = projectRoot </> "pkg"
            packageRoot = mkRoot packageRootPath Nothing
        createDirectoryIfMissing True packageRootPath
        writeProjectFile projectRoot "pkg/package.yaml" "name: foo\n"

        hpackCalls <- newIORef ([] :: [FilePath])
        let runner =
              PackageMaterializeRunner
                { runHpackGenerator = \root -> do
                    modifyIORef' hpackCalls (root :)
                    writeFile (root </> "foo.cabal") "name: foo\nversion: 0.1.0.0\n"
                    pure (Right ())
                }

        result <- materializeCabalPackageFileIO runner (\_ -> pure ()) id packageRoot

        result `shouldBe` Right (packageRootPath </> "foo.cabal")
        readIORef hpackCalls `shouldReturn` [packageRootPath]

    it "tries system hpack before cabal exec hpack for Cabal projects" do
      withTempProject \projectRoot -> do
        let packageRootPath = projectRoot </> "pkg"
        createDirectoryIfMissing True packageRootPath

        calls <- newIORef ([] :: [(FilePath, [String])])
        result <-
          runHpackGeneratorWithProcess
            ( \root command arguments -> do
                root `shouldBe` packageRootPath
                modifyIORef' calls (<> [(command, arguments)])
                pure case command of
                  "hpack" -> Left "system hpack unavailable"
                  "cabal" -> Right "generated"
                  _ -> Left "unexpected command"
            )
            packageRootPath

        result `shouldBe` Right ()
        readIORef calls `shouldReturn` [("hpack", []), ("cabal", ["exec", "--", "hpack"])]

    it "fails when hpack succeeds but no .cabal file exists" do
      withTempProject \projectRoot -> do
        let packageRootPath = projectRoot </> "pkg"
        createDirectoryIfMissing True packageRootPath
        writeProjectFile projectRoot "pkg/package.yaml" "name: foo\n"

        let runner =
              PackageMaterializeRunner
                { runHpackGenerator = \_ -> pure (Right ())
                }

        result <- materializeCabalPackageFileIO runner (\_ -> pure ()) id (mkRoot packageRootPath Nothing)
        result
          `shouldBe` Left ("No .cabal file found in package directory: " <> packageRootPath)

    it "fails when hpack reports version mismatch" do
      withTempProject \projectRoot -> do
        let packageRootPath = projectRoot </> "pkg"
        createDirectoryIfMissing True packageRootPath
        writeProjectFile projectRoot "pkg/package.yaml" "name: foo\n"
        writeProjectFile projectRoot "pkg/foo.cabal" "name: foo\nversion: 0.1.0.0\n"

        let runner =
              PackageMaterializeRunner
                { runHpackGenerator = \_ ->
                    pure (Left "lore.cabal was generated with a newer version of hpack, please upgrade and try again.")
                }

        result <- materializeCabalPackageFileIO runner (\_ -> pure ()) id (mkRoot packageRootPath Nothing)
        result
          `shouldBe` Left
            ( "Detected package.yaml in "
                <> packageRootPath
                <> ", but failed to generate a .cabal file before reading package metadata. lore.cabal was generated with a newer version of hpack, please upgrade and try again."
            )

    it "succeeds with explicit preferred cabal when multiple cabal files exist" do
      withTempProject \projectRoot -> do
        let packageRootPath = projectRoot </> "pkg"
            preferredCabalFile = packageRootPath </> "foo.cabal"
        createDirectoryIfMissing True packageRootPath
        writeProjectFile projectRoot "pkg/foo.cabal" "name: foo\nversion: 0.1.0.0\n"
        writeProjectFile projectRoot "pkg/bar.cabal" "name: bar\nversion: 0.1.0.0\n"

        result <-
          materializeCabalPackageFileIO
            noOpRunner
            (\_ -> pure ())
            id
            (mkRoot packageRootPath (Just preferredCabalFile))
        result `shouldBe` Right preferredCabalFile

    it "fails without explicit preferred cabal when multiple cabal files exist" do
      withTempProject \projectRoot -> do
        let packageRootPath = projectRoot </> "pkg"
        createDirectoryIfMissing True packageRootPath
        writeProjectFile projectRoot "pkg/a.cabal" "name: a\nversion: 0.1.0.0\n"
        writeProjectFile projectRoot "pkg/b.cabal" "name: b\nversion: 0.1.0.0\n"

        result <-
          materializeCabalPackageFileIO
            noOpRunner
            (\_ -> pure ())
            id
            (mkRoot packageRootPath Nothing)
        result
          `shouldBe` Left ("Multiple .cabal files found in package directory: " <> packageRootPath <> ". Use explicit package entries or remove ambiguity.")

  describe "preparePackageMaterializationBeforeEnvironmentProbeWithRunner" do
    it "materializes a root package.yaml project before environment probing" do
      withTempProject \projectRoot -> do
        writeProjectFile projectRoot "package.yaml" "name: demo\n"

        hpackCalls <- newIORef ([] :: [FilePath])
        let runner =
              PackageMaterializeRunner
                { runHpackGenerator = \root -> do
                    modifyIORef' hpackCalls (root :)
                    writeFile (root </> "demo.cabal") "name: demo\nversion: 0.1.0.0\n"
                    pure (Right ())
                }

        result <-
          preparePackageMaterializationBeforeEnvironmentProbeWithRunner
            runner
            noLogHandle
            CabalProject
            projectRoot

        result
          `shouldBe` Right
            ( [mkRoot projectRoot Nothing],
              [normalise (projectRoot </> "demo.cabal")]
            )
        readIORef hpackCalls `shouldReturn` [normalise projectRoot]

  describe "detectProjectProvider" do
    it "treats a root package.yaml as CabalProject" do
      withTempProject \projectRoot -> do
        writeProjectFile projectRoot "package.yaml" "name: demo\n"

        detectProjectProvider projectRoot
          `shouldReturn` Right CabalProject

noOpRunner :: PackageMaterializeRunner
noOpRunner =
  PackageMaterializeRunner
    { runHpackGenerator = \_ -> pure (Right ())
    }

mkRoot :: FilePath -> Maybe FilePath -> PackageRoot
mkRoot rootPath maybePreferredCabalFile =
  PackageRoot
    { packageRootPath = normalise rootPath,
      packageRootPreferredCabalFile = normalise <$> maybePreferredCabalFile
    }

withTempProject :: (FilePath -> IO a) -> IO a
withTempProject action =
  bracket createTempDirectoryPath removePathForcibly action

createTempDirectoryPath :: IO FilePath
createTempDirectoryPath = do
  (tempFilePath, handle) <- openTempFile "/tmp" "lore-package-discovery-"
  hClose handle
  removeFile tempFilePath
  createDirectory tempFilePath
  makeAbsolute tempFilePath

writeProjectFile :: FilePath -> FilePath -> String -> IO ()
writeProjectFile projectRoot relativePath contents = do
  let fullPath = projectRoot </> relativePath
  createDirectoryIfMissing True (takeDirectory fullPath)
  writeFile fullPath contents
