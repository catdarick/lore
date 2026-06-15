module PackageEnvironmentSpec (spec) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.RWS (asks)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Distribution.Version as CabalVersion
import Lore.Internal.Ghc.PackageEnvironment.Index (parsePackageEntries)
import Lore.Internal.Ghc.PackageEnvironment.Parse
  ( packagePathToPackageDbStack,
    parseGhcEnvironmentFile,
  )
import Lore.Internal.Ghc.PackageEnvironment.Probe
  ( GhcEnvironmentProbeRunner (..),
    captureGhcEnvironmentWithRunner,
  )
import Lore.Internal.Ghc.PackageEnvironment.Resolve
  ( packageEnvironmentCacheKey,
    resolveDependencyPackageEnvironment,
  )
import Lore.Internal.Ghc.PackageEnvironment.Types
  ( CapturedGhcEnvironment (..),
    GhcToolchain (..),
    PackageDb (..),
    PackageDbStack (..),
    PackageEnvironmentSnapshot (..),
    PackageIndex (..),
    PackageIndexEntry (..),
    PackageNameText (..),
    PackageResolutionError (..),
    ParsedGhcEnvironmentFile (..),
    ResolvedPackageEnvironment (..),
    UnitIdText (..),
  )
import Lore.Internal.Package.Types (ComponentIdentity (..))
import Lore.Internal.ProjectEnvironment.Refresh (ProjectEnvironmentRefreshRunners (..), refreshProjectEnvironmentWith)
import Lore.Internal.ProjectEnvironment.Types
  ( PreparedProjectDescription (..),
    ProjectConfigurationSnapshot (..),
    ProjectEnvironmentFailure (..),
    ProjectEnvironmentRefresh (..),
    ProjectEnvironmentState (..),
  )
import Lore.Internal.ProjectProvider (ProjectProvider (..))
import Lore.Internal.Session (SessionContext (..))
import Lore.Monad (LoreMonadT)
import System.FilePath (normalise, (</>))
import Test.Hspec
import TestSupport (fixtureLoreAt, withFixtureCopy, withFixtureSpec)

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

  describe "parsePackageEntries" do
    it "parses large package records using required top-level fields only" do
      let record =
            unlines
              [ "name:                 some-package",
                "version:              0.1.0.0",
                "visibility:           public",
                "id:                   some-package-0.1.0.0-JPV2jdQqutPI5GlgdpB2ST",
                "key:                  some-package-0.1.0.0-JPV2jdQqutPI5GlgdpB2ST",
                "description:",
                "    Please see the README...",
                "exposed:              True",
                "exposed-modules:",
                "    Dev.DevTools Dev.TimeTravel Dev.UserSetup",
                "    Dev.UserSetup.SetupActions.CreditBuilder"
              ]

      parsePackageEntries record
        `shouldBe` Right
          [ PackageIndexEntry
              { packageIndexPackageName = PackageNameText "some-package",
                packageIndexUnitId = UnitIdText "some-package-0.1.0.0-JPV2jdQqutPI5GlgdpB2ST",
                packageIndexVersion = "0.1.0.0",
                packageIndexExposed = True
              }
          ]

    it "ignores continuation lines that contain colons" do
      let record =
            unlines
              [ "name: package-a",
                "version: 1.0.0",
                "id: package-a-1.0.0-abc",
                "description:",
                "    Something: with colon",
                "exposed: False"
              ]

      parsePackageEntries record
        `shouldBe` Right
          [ PackageIndexEntry
              { packageIndexPackageName = PackageNameText "package-a",
                packageIndexUnitId = UnitIdText "package-a-1.0.0-abc",
                packageIndexVersion = "1.0.0",
                packageIndexExposed = False
              }
          ]

    it "parses unit IDs wrapped onto continuation lines" do
      let record =
            unlines
              [ "name:                 HUnit",
                "version:              1.6.2.0",
                "visibility:           public",
                "id:",
                "    HUnit-1.6.2.0-45e0ccc498517129b7100db6705b111f5ee78238177979a096b0fd4104bc16e6",
                "exposed:              True"
              ]

      parsePackageEntries record
        `shouldBe` Right
          [ PackageIndexEntry
              { packageIndexPackageName = PackageNameText "HUnit",
                packageIndexUnitId = UnitIdText "HUnit-1.6.2.0-45e0ccc498517129b7100db6705b111f5ee78238177979a096b0fd4104bc16e6",
                packageIndexVersion = "1.6.2.0",
                packageIndexExposed = True
              }
          ]

    it "accepts unit-id when id is absent" do
      let record =
            unlines
              [ "name: text",
                "version: 2.0.2",
                "unit-id: text-2.0.2",
                "exposed: True"
              ]

      parsePackageEntries record
        `shouldBe` Right
          [ PackageIndexEntry
              { packageIndexPackageName = PackageNameText "text",
                packageIndexUnitId = UnitIdText "text-2.0.2",
                packageIndexVersion = "2.0.2",
                packageIndexExposed = True
              }
          ]

    it "derives exposed from visibility when exposed is absent" do
      let record =
            unlines
              [ "name: ghc",
                "version: 9.6.5",
                "id: ghc-9.6.5",
                "visibility: public"
              ]

      parsePackageEntries record
        `shouldBe` Right
          [ PackageIndexEntry
              { packageIndexPackageName = PackageNameText "ghc",
                packageIndexUnitId = UnitIdText "ghc-9.6.5",
                packageIndexVersion = "9.6.5",
                packageIndexExposed = True
              }
          ]

    it "defaults exposed to True when both exposed and visibility are absent" do
      let record =
            unlines
              [ "name: custom",
                "version: 1.0.0",
                "id: custom-1.0.0"
              ]

      parsePackageEntries record
        `shouldBe` Right
          [ PackageIndexEntry
              { packageIndexPackageName = PackageNameText "custom",
                packageIndexUnitId = UnitIdText "custom-1.0.0",
                packageIndexVersion = "1.0.0",
                packageIndexExposed = True
              }
          ]

    it "fails when both id and unit-id are missing" do
      let record =
            unlines
              [ "name: text",
                "version: 2.0.2",
                "exposed: True"
              ]

      case parsePackageEntries record of
        Left parseError ->
          parseError `shouldContain` "Missing required field 'id' or 'unit-id'."
        Right parsedEntries ->
          expectationFailure ("Expected parse failure, got: " <> show parsedEntries)

    it "parses multiple records separated by ---" do
      let records =
            unlines
              [ "name: text",
                "version: 2.0.2",
                "id: text-2.0.2-old",
                "exposed: True",
                "---",
                "name: text",
                "version: 2.1.1",
                "id: text-2.1.1-new",
                "exposed: True"
              ]

      parsePackageEntries records
        `shouldBe` Right
          [ PackageIndexEntry
              { packageIndexPackageName = PackageNameText "text",
                packageIndexUnitId = UnitIdText "text-2.0.2-old",
                packageIndexVersion = "2.0.2",
                packageIndexExposed = True
              },
            PackageIndexEntry
              { packageIndexPackageName = PackageNameText "text",
                packageIndexUnitId = UnitIdText "text-2.1.1-new",
                packageIndexVersion = "2.1.1",
                packageIndexExposed = True
              }
          ]

  withFixtureSpec do
    describe "refreshProjectEnvironmentWith" do
      it "first refresh prepares dependencies, captures the environment, and commits state" \fixture -> do
        withFixtureCopy fixture \fixtureRoot -> do
          (prepCount, captureCount, committedSnapshot) <-
            fixtureLoreAt fixture fixtureRoot do
              provider <- asks projectProvider
              stableToolchain <- asks ghcToolchain
              let prepared = mkPreparedProject provider "initial"
              preparedRef <- liftIO $ newIORef [Right prepared, Right prepared]
              prepCountRef <- liftIO $ newIORef (0 :: Int)
              captureCountRef <- liftIO $ newIORef (0 :: Int)
              let runners =
                    mkRefreshRunners
                      stableToolchain
                      preparedRef
                      prepCountRef
                      (pure (Right ()))
                      captureCountRef
                      (pure Nothing)
              refreshResult <- refreshProjectEnvironmentWith runners
              case refreshResult of
                Left failure -> error ("Expected successful refresh, got: " <> show failure)
                Right refresh -> do
                  prepCount <- liftIO $ readIORef prepCountRef
                  captureCount <- liftIO $ readIORef captureCountRef
                  pure (prepCount, captureCount, refresh.refreshedProjectEnvironment.projectEnvironmentConfigurationSnapshot)

          prepCount `shouldBe` 1
          captureCount `shouldBe` 1
          committedSnapshot `shouldBe` (mkPreparedSnapshot CabalProject "initial") {projectConfigurationProvider = projectConfigurationProvider committedSnapshot}

      it "skips dependency preparation when the project has no components" \fixture -> do
        withFixtureCopy fixture \fixtureRoot -> do
          (prepCount, captureCount) <-
            fixtureLoreAt fixture fixtureRoot do
              provider <- asks projectProvider
              stableToolchain <- asks ghcToolchain
              let prepared =
                    (mkPreparedProject provider "empty")
                      { preparedConfigurationSnapshot =
                          (mkPreparedSnapshot provider "empty")
                            { projectConfigurationDependencies = Map.empty
                            }
                      }
              preparedRef <- liftIO $ newIORef [Right prepared, Right prepared]
              prepCountRef <- liftIO $ newIORef (0 :: Int)
              captureCountRef <- liftIO $ newIORef (0 :: Int)
              let runners =
                    mkRefreshRunners
                      stableToolchain
                      preparedRef
                      prepCountRef
                      (pure (Right ()))
                      captureCountRef
                      (pure Nothing)
              refreshResult <- refreshProjectEnvironmentWith runners
              case refreshResult of
                Left failure -> error ("Expected successful refresh, got: " <> show failure)
                Right _ -> do
                  prepCount <- liftIO $ readIORef prepCountRef
                  captureCount <- liftIO $ readIORef captureCountRef
                  pure (prepCount, captureCount)

          prepCount `shouldBe` 0
          captureCount `shouldBe` 1

      it "unchanged configuration reuses the previous package environment without preparation" \fixture -> do
        withFixtureCopy fixture \fixtureRoot -> do
          (prepCount, captureCount, secondChanged) <-
            fixtureLoreAt fixture fixtureRoot do
              provider <- asks projectProvider
              stableToolchain <- asks ghcToolchain
              let prepared = mkPreparedProject provider "same"
              preparedRef <- liftIO $ newIORef [Right prepared, Right prepared, Right prepared]
              prepCountRef <- liftIO $ newIORef (0 :: Int)
              captureCountRef <- liftIO $ newIORef (0 :: Int)
              let runners =
                    mkRefreshRunners
                      stableToolchain
                      preparedRef
                      prepCountRef
                      (pure (Right ()))
                      captureCountRef
                      (pure Nothing)
              _ <- refreshProjectEnvironmentWith runners
              secondResult <- refreshProjectEnvironmentWith runners
              case secondResult of
                Left failure -> error ("Expected successful refresh, got: " <> show failure)
                Right refresh -> do
                  prepCount <- liftIO $ readIORef prepCountRef
                  captureCount <- liftIO $ readIORef captureCountRef
                  pure (prepCount, captureCount, refresh.projectEnvironmentChanged)

          prepCount `shouldBe` 1
          captureCount `shouldBe` 1
          secondChanged `shouldBe` False

      it "changed configuration prepares dependencies and commits the second post-build project description" \fixture -> do
        withFixtureCopy fixture \fixtureRoot -> do
          (prepCount, captureCount, committedSnapshot) <-
            fixtureLoreAt fixture fixtureRoot do
              provider <- asks projectProvider
              stableToolchain <- asks ghcToolchain
              let initialA = mkPreparedProject provider "initial-a"
                  postA = mkPreparedProject provider "post-a"
                  initialB = mkPreparedProject provider "initial-b"
                  postB = mkPreparedProject provider "post-b"
              preparedRef <- liftIO $ newIORef [Right initialA, Right postA, Right initialB, Right postB]
              prepCountRef <- liftIO $ newIORef (0 :: Int)
              captureCountRef <- liftIO $ newIORef (0 :: Int)
              let runners =
                    mkRefreshRunners
                      stableToolchain
                      preparedRef
                      prepCountRef
                      (pure (Right ()))
                      captureCountRef
                      (pure Nothing)
              _ <- refreshProjectEnvironmentWith runners
              secondResult <- refreshProjectEnvironmentWith runners
              case secondResult of
                Left failure -> error ("Expected successful refresh, got: " <> show failure)
                Right refresh -> do
                  prepCount <- liftIO $ readIORef prepCountRef
                  captureCount <- liftIO $ readIORef captureCountRef
                  pure (prepCount, captureCount, refresh.refreshedProjectEnvironment.projectEnvironmentConfigurationSnapshot)

          prepCount `shouldBe` 2
          captureCount `shouldBe` 2
          projectConfigurationProviderFiles committedSnapshot `shouldBe` projectConfigurationProviderFiles (mkPreparedSnapshot (projectConfigurationProvider committedSnapshot) "post-b")

      it "preparation or capture failure leaves the previous state unchanged" \fixture -> do
        withFixtureCopy fixture \fixtureRoot -> do
          (prepCount, captureCount, stateAfterPrepFailure, stateAfterCaptureFailure) <-
            fixtureLoreAt fixture fixtureRoot do
              provider <- asks projectProvider
              stableToolchain <- asks ghcToolchain
              let baseline = mkPreparedProject provider "baseline"
                  changedForPrepFailure = mkPreparedProject provider "prep-failure"
                  changedForCaptureFailure = mkPreparedProject provider "capture-failure"
              preparedRef <- liftIO $ newIORef [Right baseline, Right baseline, Right changedForPrepFailure, Right baseline, Right changedForCaptureFailure, Right changedForCaptureFailure, Right baseline]
              prepResultsRef <- liftIO $ newIORef [Right (), Left "prep failed", Right ()]
              captureResultsRef <- liftIO $ newIORef [Nothing, Just "capture failed"]
              prepCountRef <- liftIO $ newIORef (0 :: Int)
              captureCountRef <- liftIO $ newIORef (0 :: Int)
              let runners =
                    mkRefreshRunners
                      stableToolchain
                      preparedRef
                      prepCountRef
                      (popIO prepResultsRef)
                      captureCountRef
                      (popIO captureResultsRef)
              _ <- refreshProjectEnvironmentWith runners
              prepFailureResult <- refreshProjectEnvironmentWith runners
              stateAfterPrepFailure <- refreshProjectEnvironmentWith runners
              captureFailureResult <- refreshProjectEnvironmentWith runners
              stateAfterCaptureFailure <- refreshProjectEnvironmentWith runners
              case (prepFailureResult, captureFailureResult, stateAfterPrepFailure, stateAfterCaptureFailure) of
                (Left (ProjectEnvironmentFailed _), Left (ProjectEnvironmentFailed _), Right afterPrep, Right afterCapture) -> do
                  prepCount <- liftIO $ readIORef prepCountRef
                  captureCount <- liftIO $ readIORef captureCountRef
                  pure
                    ( prepCount,
                      captureCount,
                      afterPrep.refreshedProjectEnvironment.projectEnvironmentConfigurationSnapshot,
                      afterCapture.refreshedProjectEnvironment.projectEnvironmentConfigurationSnapshot
                    )
                other -> error ("Unexpected refresh results: " <> showRefreshResults other)

          prepCount `shouldBe` 3
          captureCount `shouldBe` 2
          projectConfigurationProviderFiles stateAfterPrepFailure `shouldBe` projectConfigurationProviderFiles (mkPreparedSnapshot (projectConfigurationProvider stateAfterPrepFailure) "baseline")
          projectConfigurationProviderFiles stateAfterCaptureFailure `shouldBe` projectConfigurationProviderFiles (mkPreparedSnapshot (projectConfigurationProvider stateAfterCaptureFailure) "baseline")

  describe "captureGhcEnvironmentWithRunner" do
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

      result <- captureGhcEnvironmentWithRunner runner StackProject "/tmp/project"
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

      result <- captureGhcEnvironmentWithRunner runner StackProject "/tmp/project"
      result
        `shouldBe` Right
          CapturedGhcEnvironment
            { capturedGhcToolchain =
                GhcToolchain
                  { ghcToolchainCompilerExe = "/tmp/fake-ghc",
                    ghcToolchainCompilerVersion = CabalVersion.mkVersion [9, 6, 5],
                    ghcToolchainGhcPkgExe = "/tmp/fake-ghc-pkg",
                    ghcToolchainLibDir = "/tmp/fake-libdir"
                  },
              capturedPackageEnvironment =
                PackageEnvironmentSnapshot
                  { packageEnvironmentPackageDbStack = PackageDbStack [GlobalPackageDb],
                    packageEnvironmentPackageIndex =
                      mkPackageIndex
                        [ mkEntry "text" "text-2.0.2-old",
                          mkEntry "text" "text-2.1.1-new"
                        ],
                    packageEnvironmentSelectedUnitIdsByPackageName =
                      Map.singleton
                        (PackageNameText "text")
                        (Set.fromList [UnitIdText "text-2.0.2-old", UnitIdText "text-2.1.1-new"])
                  }
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

      result <- captureGhcEnvironmentWithRunner runner CabalProject "/tmp/work"
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

      result <- captureGhcEnvironmentWithRunner runner StackProject "/tmp/work"
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

      result <- captureGhcEnvironmentWithRunner runner StackProject "/tmp/work"
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

snapshotWithoutText :: PackageEnvironmentSnapshot
snapshotWithoutText =
  mkSnapshot [] Map.empty

mkRefreshRunners ::
  GhcToolchain ->
  IORef [Either ProjectEnvironmentFailure PreparedProjectDescription] ->
  IORef Int ->
  IO (Either String ()) ->
  IORef Int ->
  IO (Maybe String) ->
  ProjectEnvironmentRefreshRunners (LoreMonadT IO)
mkRefreshRunners stableToolchain preparedRef prepCountRef prepResult captureCountRef captureFailure =
  ProjectEnvironmentRefreshRunners
    { refreshRunnerPrepareDescription = liftIO (popIO preparedRef),
      refreshRunnerPrepareDependencies = \_ _ -> do
        modifyIORef' prepCountRef (+ 1)
        prepResult,
      refreshRunnerCaptureEnvironment = \_ _ -> do
        modifyIORef' captureCountRef (+ 1)
        maybeFailure <- captureFailure
        pure case maybeFailure of
          Just failure -> Left failure
          Nothing -> Right (mkCapturedEnvironment stableToolchain)
    }

popIO :: IORef [a] -> IO a
popIO ref = do
  values <- readIORef ref
  case values of
    [] -> fail "Unexpected empty fake runner result queue."
    value : remaining -> do
      writeIORef ref remaining
      pure value

mkCapturedEnvironment :: GhcToolchain -> CapturedGhcEnvironment
mkCapturedEnvironment stableToolchain =
  CapturedGhcEnvironment
    { capturedGhcToolchain = stableToolchain,
      capturedPackageEnvironment = mkSnapshot [] Map.empty
    }

mkPreparedProject :: ProjectProvider -> String -> PreparedProjectDescription
mkPreparedProject provider tag =
  PreparedProjectDescription
    { preparedPackageRoots = [],
      preparedCabalFiles = [],
      preparedPackages = [],
      preparedRequiredExternalDependencies = Set.empty,
      preparedConfigurationSnapshot = mkPreparedSnapshot provider tag
    }

mkPreparedSnapshot :: ProjectProvider -> String -> ProjectConfigurationSnapshot
mkPreparedSnapshot provider tag =
  ProjectConfigurationSnapshot
    { projectConfigurationProvider = provider,
      projectConfigurationPackageRoots = [],
      projectConfigurationDependencies = Map.singleton (ComponentIdentity "pkg" "library") (Set.singleton tag),
      projectConfigurationProviderFiles = [("provider", TE.encodeUtf8 (T.pack tag))]
    }

showRefreshResults :: (Either ProjectEnvironmentFailure ProjectEnvironmentRefresh, Either ProjectEnvironmentFailure ProjectEnvironmentRefresh, Either ProjectEnvironmentFailure ProjectEnvironmentRefresh, Either ProjectEnvironmentFailure ProjectEnvironmentRefresh) -> String
showRefreshResults = show

snapshotUniqueText :: PackageEnvironmentSnapshot
snapshotUniqueText =
  mkSnapshot [mkEntry "text" "text-2.1.1-aaaa"] Map.empty

snapshotAmbiguousText :: PackageEnvironmentSnapshot
snapshotAmbiguousText =
  mkSnapshot
    [ mkEntry "text" "text-2.0.2-old",
      mkEntry "text" "text-2.1.1-new"
    ]
    Map.empty

snapshotAmbiguousWithSelection :: PackageEnvironmentSnapshot
snapshotAmbiguousWithSelection =
  mkSnapshot
    [ mkEntry "text" "text-2.0.2-old",
      mkEntry "text" "text-2.1.1-new"
    ]
    (Map.singleton (PackageNameText "text") (Set.singleton (UnitIdText "text-2.1.1-new")))

mkSnapshot :: [PackageIndexEntry] -> Map.Map PackageNameText (Set.Set UnitIdText) -> PackageEnvironmentSnapshot
mkSnapshot packageEntries selectedUnitIdsByPackageName =
  PackageEnvironmentSnapshot
    { packageEnvironmentPackageDbStack = PackageDbStack [GlobalPackageDb],
      packageEnvironmentPackageIndex = mkPackageIndex packageEntries,
      packageEnvironmentSelectedUnitIdsByPackageName = selectedUnitIdsByPackageName
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
        "__LORE_GHC_VERSION__:9.6.5",
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
