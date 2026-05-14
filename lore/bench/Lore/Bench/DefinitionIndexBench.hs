{-# LANGUAGE RecordWildCards #-}

module Lore.Bench.DefinitionIndexBench
  ( benchmarks,
    smokeBenchmarks,
  )
where

import Criterion.Main
import qualified Data.Map.Strict as Map
import Lore.Bench.Fixtures
import Lore.Internal.Definition.Analysis
  ( buildDefinitionBindings,
    buildDefinitionMemberIndexes,
    buildDefinitionModuleIndex,
    buildDefinitionOccurrences,
    buildDependencies,
    buildReferenceHitsByOccKey,
  )
import Lore.Internal.Definition.RequiredImports (buildRequiredImportsById)
import qualified Lore.Internal.Definition.Types as Def

benchmarks :: Benchmark
benchmarks =
  bgroup
    "definition-index"
    [ bgroup
        "small"
        [ bench "buildDefinitionBindings" $ nf buildBindings smallDefinitionIndexFixture,
          bench "buildDefinitionOccurrences" $ nf buildOccurrences smallDefinitionIndexFixture,
          bench "buildReferenceHitsByOccKey" $ nf (buildReferenceHitsByOccKey . buildOccurrences) smallDefinitionIndexFixture,
          bench "buildDependencies" $ nf buildDeps smallDefinitionIndexFixture,
          bench "buildRequiredImportsById" $ nf buildImports smallDefinitionIndexFixture,
          bench "buildDefinitionModuleIndex" $ nf buildModuleIndex smallDefinitionIndexFixture
        ],
      bgroup
        "medium"
        [ bench "buildDefinitionModuleIndex" $ nf buildModuleIndex mediumDefinitionIndexFixture
        ],
      bgroup
        "large"
        [ bench "buildDefinitionModuleIndex" $ nf buildModuleIndex largeDefinitionIndexFixture
        ]
    ]

smokeBenchmarks :: Benchmark
smokeBenchmarks =
  bgroup
    "definition-index-smoke"
    [ bench "buildDefinitionModuleIndex" $ nf buildModuleIndex smallDefinitionIndexFixture
    ]

buildBindings :: DefinitionIndexFixture -> Def.DefinitionBindings
buildBindings DefinitionIndexFixture {..} =
  buildDefinitionBindings fixtureModule fixtureParsedFacts fixtureTypedFacts

buildOccurrences :: DefinitionIndexFixture -> Map.Map Def.DefinitionId [Def.DefinitionOccurrenceFact]
buildOccurrences fixture@DefinitionIndexFixture {..} =
  buildDefinitionOccurrences fixtureModule fixtureParsedFacts fixtureTypedFacts bindings memberIndexesById importCandidatesById
  where
    bindings = buildBindings fixture
    memberIndexesById = buildDefinitionMemberIndexes fixtureParsedFacts bindings
    importCandidates = map minimalImportToImportCandidate (Def.typedSourceImports fixtureTypedFacts)
    importCandidatesById =
      Map.fromList
        [ (Def.importCandidateId candidate, candidate)
        | candidate <- importCandidates
        ]

buildDeps :: DefinitionIndexFixture -> Map.Map Def.DefinitionId Def.DefinitionDependencies
buildDeps fixture@DefinitionIndexFixture {..} =
  buildDependencies bindings memberIndexesById (buildOccurrences fixture) fixtureCoreFacts
  where
    bindings = buildBindings fixture
    memberIndexesById = buildDefinitionMemberIndexes fixtureParsedFacts bindings

buildImports :: DefinitionIndexFixture -> Map.Map Def.DefinitionId [Def.RequiredImport]
buildImports fixture@DefinitionIndexFixture {..} =
  buildRequiredImportsById importCandidates (buildOccurrences fixture)
  where
    importCandidates = map minimalImportToImportCandidate (Def.typedSourceImports fixtureTypedFacts)

buildModuleIndex :: DefinitionIndexFixture -> Def.DefinitionModuleIndex
buildModuleIndex DefinitionIndexFixture {..} =
  buildDefinitionModuleIndex fixtureModule fixtureParsedFacts fixtureTypedFacts fixtureCoreFacts

minimalImportToImportCandidate :: Def.MinimalTypedImport -> Def.ImportCandidate
minimalImportToImportCandidate typedImport =
  let Def.ImportId importKey = Def.typedImportId typedImport
   in Def.ImportCandidate
        { Def.importCandidateId = Def.typedImportId typedImport,
          Def.importCandidateBaseImport =
            Def.RequiredImport
              { Def.importKey = importKey,
                Def.importModule = Def.typedImportModule typedImport,
                Def.importPackageQualifier = Def.typedImportPackageQualifier typedImport,
                Def.importSource = Def.typedImportSource typedImport,
                Def.importQualifiedStyle = Def.typedImportQualifiedStyle typedImport,
                Def.importAlias = Def.typedImportAlias typedImport,
                Def.importOriginallyExplicit = Def.typedImportOriginallyExplicit typedImport,
                Def.importItems = []
              }
        }
