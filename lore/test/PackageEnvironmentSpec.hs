module PackageEnvironmentSpec (spec) where

import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Lore.Internal.Ghc.PackageEnvironment.Parse
  ( packagePathToPackageDbStack,
    parseGhcEnvironmentFile,
  )
import Lore.Internal.Ghc.PackageEnvironment.Probe
  ( GhcEnvironmentProbeRunner (..),
    captureGhcEnvironmentSnapshotWithRunner,
  )
import Lore.Internal.Ghc.PackageEnvironment.Resolve
  ( packageEnvironmentCacheKey,
    resolveDependencyPackageEnvironment,
  )
import Lore.Internal.Ghc.PackageEnvironment.Types
  ( GhcEnvironmentSnapshot (..),
    PackageDb (..),
    PackageDbStack (..),
    PackageIndex (..),
    PackageIndexEntry (..),
    PackageNameText (..),
    PackageResolutionError (..),
    ParsedGhcEnvironmentFile (..),
    ResolvedPackageEnvironment (..),
    UnitIdText (..),
  )
import Lore.Internal.ProjectProvider (ProjectProvider (..))
import System.FilePath (normalise, (</>))
import Test.Hspec

spec :: Spec
spec = do
  describe "parseGhcEnvironmentFile" do
    it "parses package DB directives and package-id selections" do
      let environmentPath = "/tmp/project/.ghc.environment"
          contents =
            T.unlines
              [ "clear-package-db",
                "global-package-db",
                "package-db /x/y/package.conf.d",
                "package-id text-2.1.1-abcd"
              ]

      parseGhcEnvironmentFile environmentPath contents
        `shouldBe` Right
          ParsedGhcEnvironmentFile
            { parsedEnvPackageDbStack =
                PackageDbStack
                  [ GlobalPackageDb,
                    SpecificPackageDb "/x/y/package.conf.d"
                  ],
              parsedEnvSelectedUnitIds = Set.singleton (UnitIdText "text-2.1.1-abcd")
            }

    it "resolves relative package-db paths relative to the environment file directory" do
      let environmentPath = "/tmp/project/.ghc.environment"
          contents = T.unlines ["clear-package-db", "package-db ../db"]

      parseGhcEnvironmentFile environmentPath contents
        `shouldBe` Right
          ParsedGhcEnvironmentFile
            { parsedEnvPackageDbStack =
                PackageDbStack
                  [SpecificPackageDb (normalise ("/tmp/project" </> "../db"))],
              parsedEnvSelectedUnitIds = Set.empty
            }

    it "fails on unsupported directives" do
      parseGhcEnvironmentFile "/tmp/project/.ghc.environment" (T.unlines ["bad-directive"]) `shouldSatisfy` isLeft

    it "fails on empty package-db directive argument" do
      parseGhcEnvironmentFile "/tmp/project/.ghc.environment" (T.unlines ["package-db"]) `shouldSatisfy` isLeft

    it "fails on malformed package-id directive argument" do
      parseGhcEnvironmentFile "/tmp/project/.ghc.environment" (T.unlines ["package-id"]) `shouldSatisfy` isLeft

  describe "packagePathToPackageDbStack" do
    it "returns default package DBs for empty package path" do
      packagePathToPackageDbStack "" `shouldBe` PackageDbStack [GlobalPackageDb, UserPackageDb]

    it "splits multiple package-path entries" do
      packagePathToPackageDbStack "/a:/b"
        `shouldBe` PackageDbStack [SpecificPackageDb "/a", SpecificPackageDb "/b"]

    it "expands empty entries to default package DBs" do
      packagePathToPackageDbStack "/a::/b"
        `shouldBe` PackageDbStack [SpecificPackageDb "/a", GlobalPackageDb, UserPackageDb, SpecificPackageDb "/b"]

  describe "resolveDependencyPackageEnvironment" do
    it "fails for missing dependency package" do
      resolveDependencyPackageEnvironment snapshotWithoutText (Set.singleton "text")
        `shouldBe` Left (MissingPackage (PackageNameText "text"))

    it "resolves unique package name to exact unit ID" do
      resolveDependencyPackageEnvironment snapshotUniqueText (Set.singleton "text")
        `shouldBe` Right
          ResolvedPackageEnvironment
            { resolvedPackageDbStack = PackageDbStack [GlobalPackageDb],
              resolvedExposedUnitIds = Set.singleton (UnitIdText "text-2.1.1-aaaa")
            }

    it "fails on ambiguous package names when no selected unit-id exists" do
      resolveDependencyPackageEnvironment snapshotAmbiguousText (Set.singleton "text")
        `shouldBe` Left
          ( AmbiguousPackage
              (PackageNameText "text")
              [UnitIdText "text-2.0.2-old", UnitIdText "text-2.1.1-new"]
          )

    it "uses selected unit-id to disambiguate package name" do
      resolveDependencyPackageEnvironment snapshotAmbiguousWithSelection (Set.singleton "text")
        `shouldBe` Right
          ResolvedPackageEnvironment
            { resolvedPackageDbStack = PackageDbStack [GlobalPackageDb],
              resolvedExposedUnitIds = Set.singleton (UnitIdText "text-2.1.1-new")
            }

  describe "captureGhcEnvironmentSnapshotWithRunner" do
    it "fails when selected unit-id is not in the package index" do
      let runner =
            GhcEnvironmentProbeRunner
              { runBuildToolProbe = \_ _ _ ->
                  pure
                    ( Right
                        ( mkProbeOutput
                            (Just "/tmp/project/.ghc.environment")
                            Nothing
                            (Just (T.unlines ["clear-package-db", "global-package-db", "package-id text-2.1-missing"]))
                        )
                    ),
                runBuildPackageIndex = \_ _ _ -> pure (Right (mkPackageIndex []))
              }

      result <- captureGhcEnvironmentSnapshotWithRunner runner StackProject "/tmp/project"
      result `shouldSatisfy` isLeft
      either id (const "") result `shouldContain` "not present in the ghc-pkg package index dump"

    it "captures multiple selected unit IDs for one package when the environment selects them" do
      let runner =
            GhcEnvironmentProbeRunner
              { runBuildToolProbe = \_ _ _ ->
                  pure
                    ( Right
                        ( mkProbeOutput
                            (Just "/tmp/project/.ghc.environment")
                            Nothing
                            ( Just
                                ( T.unlines
                                    [ "clear-package-db",
                                      "global-package-db",
                                      "package-id text-2.0.2-old",
                                      "package-id text-2.1.1-new"
                                    ]
                                )
                            )
                        )
                    ),
                runBuildPackageIndex = \_ _ _ ->
                  pure
                    ( Right
                        ( mkPackageIndex
                            [ mkEntry "text" "text-2.0.2-old",
                              mkEntry "text" "text-2.1.1-new"
                            ]
                        )
                    )
              }

      result <- captureGhcEnvironmentSnapshotWithRunner runner StackProject "/tmp/project"
      result
        `shouldBe` Right
          GhcEnvironmentSnapshot
            { ghcEnvironmentCompilerExe = "/tmp/fake-ghc",
              ghcEnvironmentGhcPkgExe = "/tmp/fake-ghc-pkg",
              ghcEnvironmentLibDir = "/tmp/fake-libdir",
              ghcEnvironmentPackageDbStack = PackageDbStack [GlobalPackageDb],
              ghcEnvironmentPackageIndex =
                mkPackageIndex
                  [ mkEntry "text" "text-2.0.2-old",
                    mkEntry "text" "text-2.1.1-new"
                  ],
              ghcEnvironmentSelectedUnitIdsByPackageName =
                Map.singleton
                  (PackageNameText "text")
                  (Set.fromList [UnitIdText "text-2.0.2-old", UnitIdText "text-2.1.1-new"])
            }

    it "passes normalized package DB stack to the package-index runner" do
      capturedPackageDbStackRef <- newIORef (PackageDbStack [])
      let runner =
            GhcEnvironmentProbeRunner
              { runBuildToolProbe = \_ _ _ ->
                  pure
                    ( Right
                        ( mkProbeOutput
                            (Just "/tmp/work/.ghc.environment")
                            Nothing
                            ( Just
                                ( T.unlines
                                    [ "clear-package-db",
                                      "global-package-db",
                                      "package-db ../custom-db",
                                      "package-id text-2.1.1-selected"
                                    ]
                                )
                            )
                        )
                    ),
                runBuildPackageIndex = \_ _ packageDbStack -> do
                  writeIORef capturedPackageDbStackRef packageDbStack
                  pure (Right (mkPackageIndex [mkEntry "text" "text-2.1.1-selected"]))
              }

      result <- captureGhcEnvironmentSnapshotWithRunner runner CabalProject "/tmp/work"
      result `shouldSatisfy` isRight

      capturedPackageDbStack <- readIORef capturedPackageDbStackRef
      capturedPackageDbStack
        `shouldBe` PackageDbStack [GlobalPackageDb, SpecificPackageDb (normalise "/tmp/work/../custom-db")]

    it "uses package-path semantics when GHC_ENVIRONMENT is '-'" do
      capturedPackageDbStackRef <- newIORef (PackageDbStack [])
      let runner =
            GhcEnvironmentProbeRunner
              { runBuildToolProbe = \_ _ _ ->
                  pure
                    (Right (mkProbeOutput (Just "-") (Just "/a::/b") Nothing)),
                runBuildPackageIndex = \_ _ packageDbStack -> do
                  writeIORef capturedPackageDbStackRef packageDbStack
                  pure (Right (mkPackageIndex []))
              }

      result <- captureGhcEnvironmentSnapshotWithRunner runner StackProject "/tmp/work"
      result `shouldSatisfy` isRight

      capturedPackageDbStack <- readIORef capturedPackageDbStackRef
      capturedPackageDbStack
        `shouldBe` PackageDbStack [SpecificPackageDb "/a", GlobalPackageDb, UserPackageDb, SpecificPackageDb "/b"]

    it "fails when GHC_ENVIRONMENT points to a missing file and no inline contents were captured" do
      let runner =
            GhcEnvironmentProbeRunner
              { runBuildToolProbe = \_ _ _ ->
                  pure (Right (mkProbeOutput (Just "/path/that/does/not/exist/.ghc.environment") Nothing Nothing)),
                runBuildPackageIndex = \_ _ _ -> pure (Left "package index should not be called")
              }

      result <- captureGhcEnvironmentSnapshotWithRunner runner StackProject "/tmp/work"
      result `shouldSatisfy` isLeft
      either id (const "") result `shouldContain` "referenced file does not exist"

  describe "packageEnvironmentCacheKey" do
    it "uses package-db stack and exact unit IDs (no package names)" do
      let cacheKey =
            packageEnvironmentCacheKey
              ResolvedPackageEnvironment
                { resolvedPackageDbStack = PackageDbStack [GlobalPackageDb, SpecificPackageDb "/db"],
                  resolvedExposedUnitIds = Set.singleton (UnitIdText "text-2.1.1-aaaa")
                }

      cacheKey
        `shouldBe` Set.fromList ["package-db:0:global", "package-db:1:path:/db", "package:id:text-2.1.1-aaaa"]

snapshotWithoutText :: GhcEnvironmentSnapshot
snapshotWithoutText =
  mkSnapshot [] Map.empty

snapshotUniqueText :: GhcEnvironmentSnapshot
snapshotUniqueText =
  mkSnapshot [mkEntry "text" "text-2.1.1-aaaa"] Map.empty

snapshotAmbiguousText :: GhcEnvironmentSnapshot
snapshotAmbiguousText =
  mkSnapshot
    [ mkEntry "text" "text-2.0.2-old",
      mkEntry "text" "text-2.1.1-new"
    ]
    Map.empty

snapshotAmbiguousWithSelection :: GhcEnvironmentSnapshot
snapshotAmbiguousWithSelection =
  mkSnapshot
    [ mkEntry "text" "text-2.0.2-old",
      mkEntry "text" "text-2.1.1-new"
    ]
    (Map.singleton (PackageNameText "text") (Set.singleton (UnitIdText "text-2.1.1-new")))

mkSnapshot :: [PackageIndexEntry] -> Map.Map PackageNameText (Set.Set UnitIdText) -> GhcEnvironmentSnapshot
mkSnapshot packageEntries selectedUnitIdsByPackageName =
  GhcEnvironmentSnapshot
    { ghcEnvironmentCompilerExe = "ghc",
      ghcEnvironmentGhcPkgExe = "ghc-pkg",
      ghcEnvironmentLibDir = "/libdir",
      ghcEnvironmentPackageDbStack = PackageDbStack [GlobalPackageDb],
      ghcEnvironmentPackageIndex = mkPackageIndex packageEntries,
      ghcEnvironmentSelectedUnitIdsByPackageName = selectedUnitIdsByPackageName
    }

mkPackageIndex :: [PackageIndexEntry] -> PackageIndex
mkPackageIndex packageEntries =
  PackageIndex
    { packageIndexByUnitId =
        Map.fromList
          [ (entry.packageIndexUnitId, entry)
          | entry <- packageEntries
          ],
      packageIndexByPackageName =
        Map.fromListWith
          (<>)
          [ (entry.packageIndexPackageName, [entry])
          | entry <- packageEntries
          ]
    }

mkEntry :: String -> String -> PackageIndexEntry
mkEntry packageName unitId =
  PackageIndexEntry
    { packageIndexPackageName = PackageNameText packageName,
      packageIndexUnitId = UnitIdText unitId,
      packageIndexVersion = "0.0.0",
      packageIndexExposed = True
    }

mkProbeOutput :: Maybe FilePath -> Maybe String -> Maybe T.Text -> String
mkProbeOutput maybeEnvironmentPath maybePackagePath maybeEnvironmentContents =
  unlines
    ( [ "__LORE_GHC_EXE__:/tmp/fake-ghc",
        "__LORE_GHC_PKG_EXE__:/tmp/fake-ghc-pkg",
        "__LORE_GHC_LIBDIR__:/tmp/fake-libdir",
        "__LORE_GHC_ENVIRONMENT__:" <> maybe "" id maybeEnvironmentPath,
        "__LORE_GHC_PACKAGE_PATH__:" <> maybe "" id maybePackagePath
      ]
        <> case maybeEnvironmentContents of
          Nothing -> []
          Just environmentContents ->
            [ "__LORE_GHC_ENVIRONMENT_CONTENT_BEGIN__",
              T.unpack environmentContents,
              "__LORE_GHC_ENVIRONMENT_CONTENT_END__"
            ]
    )

isLeft :: Either a b -> Bool
isLeft eitherValue =
  case eitherValue of
    Left _ -> True
    Right _ -> False

isRight :: Either a b -> Bool
isRight eitherValue =
  case eitherValue of
    Left _ -> False
    Right _ -> True
