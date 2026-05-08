{-# LANGUAGE RecordWildCards #-}

module Lore.Bench.E2EBench
  ( benchmarks,
    smokeBenchmarks,
    runSmallLoadTargetsCold,
    runSmallGetDefinitionRecursive,
    runSmallGetDefinitionSingle,
    runSmallFindReferencesCommon,
    runMediumLoadTargetsCold,
  )
where

import Criterion.Main
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified GHC.Plugins as GHC
import qualified Lore
import Lore.Bench.FixtureProject
import Lore.Monad (LoreMonadT)
import qualified Lore.Targets as Targets

benchmarks :: Benchmark
benchmarks =
  bgroup
    "e2e"
    [ smallBenchmarks,
      mediumBenchmarks
    ]

smokeBenchmarks :: Benchmark
smokeBenchmarks =
  bgroup
    "e2e-smoke"
    [ bench "small/loadTargets/cold" $ nfIO runSmallLoadTargetsCold
    ]

smallBenchmarks :: Benchmark
smallBenchmarks =
  bgroup
    "e2e-small"
    [ bench "loadTargets/cold" $ nfIO runSmallLoadTargetsCold,
      bench "loadTargets/warm-reload" $ nfIO runSmallWarmReload,
      bench "getDefinition/single" $ nfIO runSmallGetDefinitionSingle,
      bench "getDefinition/recursive-depth-1" $ nfIO (runSmallGetDefinitionRecursive 1),
      bench "getDefinition/recursive-depth-3" $ nfIO (runSmallGetDefinitionRecursive 3),
      bench "findReferences/rare-symbol" $ nfIO runSmallFindReferencesRare,
      bench "findReferences/common-symbol" $ nfIO runSmallFindReferencesCommon,
      bench "findReferences/typeclass-method" $ nfIO runSmallFindReferencesTypeclassMethod,
      bench "findReferences/record-field" $ nfIO runSmallFindReferencesRecordField
    ]

mediumBenchmarks :: Benchmark
mediumBenchmarks =
  bgroup
    "e2e-medium"
    [ bench "loadTargets/cold" $ nfIO runMediumLoadTargetsCold,
      bench "findReferences/common-symbol" $ nfIO runMediumFindReferencesCommon,
      bench "getDefinition/recursive-depth-2" $ nfIO (runMediumGetDefinitionRecursive 2)
    ]

runSmallLoadTargetsCold :: IO Int
runSmallLoadTargetsCold =
  withFixtureLore SmallFixture do
    result <- loadTargetsNoAuto
    pure result.loadTargetsModulesLoaded

runSmallWarmReload :: IO Int
runSmallWarmReload =
  withFixtureLore SmallFixture do
    _ <- loadTargetsNoAuto
    result <- loadTargetsNoAuto
    pure result.loadTargetsModulesLoaded

runSmallGetDefinitionSingle :: IO Bool
runSmallGetDefinitionSingle =
  withFixtureLore SmallFixture do
    _ <- loadTargetsNoAuto
    name <- requireSymbolInModule "Fixture.Small.Core" "lookupOrZero"
    maybeSource <- Lore.resolveDefinitionSourceNamed name
    pure (maybe False (const True) maybeSource)

runSmallGetDefinitionRecursive :: Int -> IO Int
runSmallGetDefinitionRecursive depth =
  withFixtureLore SmallFixture do
    _ <- loadTargetsNoAuto
    name <- requireSymbolInModule "Fixture.Small.Core" "crossModuleRecord"
    closure <- Lore.resolveDefinitionClosureSourcesNamed depth name
    pure (length closure)

runSmallFindReferencesRare :: IO Int
runSmallFindReferencesRare =
  withFixtureLore SmallFixture do
    _ <- loadTargetsNoAuto
    name <- requireSymbolInModule "Fixture.Small.Core" "lookupOrZero"
    matches <- Lore.resolveReferenceMatchesForNames [name]
    pure (length matches)

runSmallFindReferencesCommon :: IO Int
runSmallFindReferencesCommon =
  withFixtureLore SmallFixture do
    _ <- loadTargetsNoAuto
    names <- symbolsByOccurrence "run"
    let filtered = take 4 (filter (modulePrefix "Fixture.Small") names)
    matches <- Lore.resolveReferenceMatchesForNames filtered
    pure (length matches)

runSmallFindReferencesTypeclassMethod :: IO Int
runSmallFindReferencesTypeclassMethod =
  withFixtureLore SmallFixture do
    _ <- loadTargetsNoAuto
    name <- requireSymbolInModule "Fixture.Small.Instances" "render"
    matches <- Lore.resolveReferenceMatchesForNames [name]
    pure (length matches)

runSmallFindReferencesRecordField :: IO Int
runSmallFindReferencesRecordField =
  withFixtureLore SmallFixture do
    _ <- loadTargetsNoAuto
    name <- requireSymbolInModule "Fixture.Small.Records" "supportValues"
    matches <- Lore.resolveReferenceMatchesForNames [name]
    pure (length matches)

runMediumLoadTargetsCold :: IO Int
runMediumLoadTargetsCold =
  withFixtureLore MediumFixture do
    result <- loadTargetsNoAuto
    pure result.loadTargetsModulesLoaded

runMediumFindReferencesCommon :: IO Int
runMediumFindReferencesCommon =
  withFixtureLore MediumFixture do
    _ <- loadTargetsNoAuto
    name <- requireSymbolInModule "Fixture.Medium.Module150" "run"
    matches <- Lore.resolveReferenceMatchesForNames [name]
    pure (length matches)

runMediumGetDefinitionRecursive :: Int -> IO Int
runMediumGetDefinitionRecursive depth =
  withFixtureLore MediumFixture do
    _ <- loadTargetsNoAuto
    name <- requireSymbolInModule "Fixture.Medium.Module150" "run"
    closure <- Lore.resolveDefinitionClosureSourcesNamed depth name
    pure (length closure)

loadTargetsNoAuto :: LoreMonadT IO Lore.LoadTargetsResult
loadTargetsNoAuto =
  Lore.loadTargets Targets.defaultLoadTargetsOptions {Targets.enableAutoRefactor = False}

symbolsByOccurrence :: String -> LoreMonadT IO [GHC.Name]
symbolsByOccurrence query = do
  symbols <-
    Set.toList
      <$> Lore.findMatchingSymbols (Lore.parseAndNormalizeName (T.pack query))
  pure (map (.name) symbols)

requireSymbolInModule :: String -> String -> LoreMonadT IO GHC.Name
requireSymbolInModule moduleName occName = do
  names <- symbolsByOccurrence occName
  case List.find (inModule moduleName) names of
    Just name -> pure name
    Nothing -> error ("benchmark symbol not found: " <> moduleName <> "." <> occName)

inModule :: String -> GHC.Name -> Bool
inModule moduleName name =
  case GHC.nameModule_maybe name of
    Nothing -> False
    Just module_ ->
      GHC.moduleNameString (GHC.moduleName module_) == moduleName

modulePrefix :: String -> GHC.Name -> Bool
modulePrefix prefix name =
  case GHC.nameModule_maybe name of
    Nothing -> False
    Just module_ ->
      prefix `List.isPrefixOf` GHC.moduleNameString (GHC.moduleName module_)
