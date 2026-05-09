module Lore.Bench.MemoryCases
  ( runMemoryBenchmarks,
  )
where

import qualified Data.Set as Set
import Lore.Bench.E2EBench
  ( runMediumLoadTargetsCold,
    runSmallFindReferencesCommon,
    runSmallGetDefinitionRecursive,
    runSmallLoadTargetsCold,
  )
import Lore.Bench.Fixtures
  ( DefinitionIndexFixture (..),
    MinifiedImportsFixture (..),
    ReferenceSearchFixture (..),
    commonOccReferenceSearchFixture,
    largeDefinitionIndexFixture,
    largeMinifiedImportsFixture,
  )
import Lore.Bench.Memory
  ( MemoryResult,
    measureIO,
    measureNF,
    printMemoryResult,
  )
import Lore.Definition (matchingReferenceMatches)
import Lore.Internal.Definition.Analysis (buildDefinitionModuleIndex)
import Lore.Internal.Definition.RequiredImports (buildMinifiedImports)

runMemoryBenchmarks :: Maybe String -> IO ()
runMemoryBenchmarks maybeMode = do
  let mode = normalizeMode maybeMode
      selectedCases = memoryCasesForMode mode
  mapM_ runCase selectedCases
  where
    runCase caseAction = do
      result <- caseAction
      printMemoryResult result
      putStrLn ""

memoryCasesForMode :: String -> [IO MemoryResult]
memoryCasesForMode mode =
  case mode of
    "smoke" -> smokeMemoryCases
    "e2e" -> e2eMemoryCases
    _ -> fullMemoryCases

normalizeMode :: Maybe String -> String
normalizeMode = maybe "full" id

smokeMemoryCases :: [IO MemoryResult]
smokeMemoryCases =
  [ definitionIndexLarge,
    referenceCommonOcc,
    e2eLoadSmall
  ]

e2eMemoryCases :: [IO MemoryResult]
e2eMemoryCases =
  [ e2eLoadSmall,
    e2eGetDefinitionSmall,
    e2eFindReferencesSmall,
    e2eLoadMedium
  ]

fullMemoryCases :: [IO MemoryResult]
fullMemoryCases =
  [ definitionIndexLarge,
    minifiedImportsLarge,
    referenceCommonOcc
  ]
    <> e2eMemoryCases

definitionIndexLarge :: IO MemoryResult
definitionIndexLarge =
  measureNF "memory/definition-index/buildDefinitionModuleIndex/large" $
    buildDefinitionModuleIndex
      fixtureModule
      fixtureParsedFacts
      fixtureTypedFacts
      fixtureCoreFacts
  where
    DefinitionIndexFixture {fixtureModule, fixtureParsedFacts, fixtureTypedFacts, fixtureCoreFacts} =
      largeDefinitionIndexFixture

minifiedImportsLarge :: IO MemoryResult
minifiedImportsLarge =
  measureNF "memory/minified-imports/buildMinifiedImports/large" $
    buildMinifiedImports
      minifiedImportCandidates
      minifiedOccurrences
  where
    MinifiedImportsFixture {minifiedImportCandidates, minifiedOccurrences} =
      largeMinifiedImportsFixture

referenceCommonOcc :: IO MemoryResult
referenceCommonOcc =
  measureNF "memory/reference-search/matchingReferenceMatches/common-occ" $
    matchingReferenceMatches
      (Set.fromList referenceFixtureTargetNames)
      referenceFixtureModuleIndex
  where
    ReferenceSearchFixture {referenceFixtureTargetNames, referenceFixtureModuleIndex} =
      commonOccReferenceSearchFixture

e2eLoadSmall :: IO MemoryResult
e2eLoadSmall =
  measureIO "memory/e2e-small/loadTargets/cold" do
    runSmallLoadTargetsCold

e2eGetDefinitionSmall :: IO MemoryResult
e2eGetDefinitionSmall =
  measureIO "memory/e2e-small/getDefinition/recursive-depth-3" do
    runSmallGetDefinitionRecursive 3

e2eFindReferencesSmall :: IO MemoryResult
e2eFindReferencesSmall =
  measureIO "memory/e2e-small/findReferences/common-symbol" do
    runSmallFindReferencesCommon

e2eLoadMedium :: IO MemoryResult
e2eLoadMedium =
  measureIO "memory/e2e-medium/loadTargets/cold" do
    runMediumLoadTargetsCold
