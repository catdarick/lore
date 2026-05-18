{-# LANGUAGE RecordWildCards #-}

module Lore.Bench.Fixtures
  ( DefinitionIndexFixture (..),
    MinifiedImportsFixture (..),
    ReferenceSearchFixture (..),
    SourceTextFixture (..),
    SourceRegionFixture (..),
    smallDefinitionIndexFixture,
    mediumDefinitionIndexFixture,
    largeDefinitionIndexFixture,
    smallReferenceSearchFixture,
    commonOccReferenceSearchFixture,
    smallMinifiedImportsFixture,
    ambiguousMinifiedImportsFixture,
    largeMinifiedImportsFixture,
    smallSourceTextFixture,
    largeSourceTextFixture,
    smallSourceRegionFixture,
    largeSourceRegionFixture,
    mkBenchModule,
    mkBenchName,
    mkBenchSrcSpan,
    mkBenchDefinitionId,
  )
where

import Data.Char (ord)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified GHC.Data.Strict as Strict
import GHC.Generics (Generic)
import qualified GHC.Plugins as GHC
import qualified GHC.Types.Unique as Unique
import Lore.Diagnostics (Span (..))
import qualified Lore.Internal.AutoRefactor.Edit as Edit
import qualified Lore.Internal.Definition.Types as Def

data DefinitionIndexFixture = DefinitionIndexFixture
  { fixtureModule :: !GHC.Module,
    fixtureParsedFacts :: !Def.ParsedModuleFacts,
    fixtureTypedFacts :: !Def.MinimalTypedModuleFacts,
    fixtureCoreFacts :: !(Maybe Def.MinimalCoreModuleFacts)
  }
  deriving stock (Generic)

data MinifiedImportsFixture = MinifiedImportsFixture
  { minifiedImportCandidates :: ![Def.ImportCandidate],
    minifiedOccurrences :: ![Def.DefinitionOccurrenceFact]
  }
  deriving stock (Generic)

data ReferenceSearchFixture = ReferenceSearchFixture
  { referenceFixtureModuleIndex :: !Def.DefinitionModuleIndex,
    referenceFixtureTargetNames :: ![GHC.Name],
    referenceFixtureOccurrenceIndex :: !(Map.Map Def.OccKey (Set.Set GHC.Module)),
    referenceFixtureDuplicateHits :: ![Def.ReferenceHit]
  }
  deriving stock (Generic)

data SourceTextFixture = SourceTextFixture
  { sourceTextFixtureContents :: !T.Text,
    sourceTextFixtureEdits :: ![Edit.FileEdit]
  }
  deriving stock (Generic)

data SourceRegionFixture = SourceRegionFixture
  { sourceRegionFixtureDeclaration :: !Def.DeclarationSpans,
    sourceRegionFixtureCandidates :: ![Def.SourceRegionCandidate],
    sourceRegionFixtureReferenceSpan :: !GHC.SrcSpan
  }
  deriving stock (Generic)

smallDefinitionIndexFixture :: DefinitionIndexFixture
smallDefinitionIndexFixture = mkDefinitionIndexFixture 10 10 5

mediumDefinitionIndexFixture :: DefinitionIndexFixture
mediumDefinitionIndexFixture = mkDefinitionIndexFixture 100 20 50

largeDefinitionIndexFixture :: DefinitionIndexFixture
largeDefinitionIndexFixture = mkDefinitionIndexFixture 1000 25 200

smallReferenceSearchFixture :: ReferenceSearchFixture
smallReferenceSearchFixture = mkReferenceSearchFixture 24 4 False

commonOccReferenceSearchFixture :: ReferenceSearchFixture
commonOccReferenceSearchFixture = mkReferenceSearchFixture 120 20 True

smallMinifiedImportsFixture :: MinifiedImportsFixture
smallMinifiedImportsFixture = mkMinifiedImportsFixture 5 100 False

ambiguousMinifiedImportsFixture :: MinifiedImportsFixture
ambiguousMinifiedImportsFixture = mkMinifiedImportsFixture 20 400 True

largeMinifiedImportsFixture :: MinifiedImportsFixture
largeMinifiedImportsFixture = mkMinifiedImportsFixture 200 25000 True

smallSourceTextFixture :: SourceTextFixture
smallSourceTextFixture = mkSourceTextFixture 100 20

largeSourceTextFixture :: SourceTextFixture
largeSourceTextFixture = mkSourceTextFixture 50000 1000

smallSourceRegionFixture :: SourceRegionFixture
smallSourceRegionFixture = mkSourceRegionFixture 10 3

largeSourceRegionFixture :: SourceRegionFixture
largeSourceRegionFixture = mkSourceRegionFixture 2000 8

mkBenchModule :: String -> GHC.Module
mkBenchModule moduleName =
  GHC.mkModule (GHC.stringToUnit "main") (GHC.mkModuleName moduleName)

mkBenchName :: GHC.Module -> String -> GHC.Name
mkBenchName module_ occurrence =
  mkBenchNameAt module_ GHC.noSrcSpan occurrence (stableUnique occurrence)

mkBenchSrcSpan :: FilePath -> Int -> Int -> Int -> Int -> GHC.SrcSpan
mkBenchSrcSpan filePath startLine startCol endLine endCol =
  GHC.RealSrcSpan realSpan Strict.Nothing
  where
    startLoc = GHC.mkRealSrcLoc (GHC.mkFastString filePath) startLine startCol
    endLoc = GHC.mkRealSrcLoc (GHC.mkFastString filePath) endLine endCol
    realSpan = GHC.mkRealSrcSpan startLoc endLoc

mkBenchDefinitionId :: GHC.Module -> GHC.SrcSpan -> Def.DefinitionId
mkBenchDefinitionId module_ span' =
  Def.DefinitionId
    { Def.definitionIdModule = module_,
      Def.definitionIdSpanKey = Def.srcSpanKey span'
    }

mkDefinitionIndexFixture :: Int -> Int -> Int -> DefinitionIndexFixture
mkDefinitionIndexFixture definitionCount occurrencesPerDefinition importCount =
  DefinitionIndexFixture
    { fixtureModule = module_,
      fixtureParsedFacts = parsedFacts,
      fixtureTypedFacts = typedFacts,
      fixtureCoreFacts = Just coreFacts
    }
  where
    module_ = mkBenchModule "Fixture.Bench.DefinitionIndex"
    sourceFile = "Fixture/Bench/DefinitionIndex.hs"

    declarationSpansByIndex =
      Map.fromList
        [ (i, declarationSpanFor i)
        | i <- [1 .. definitionCount]
        ]

    declarationSpanFor i =
      Def.DeclarationSpans
        { Def.declarationSpan = mkBenchSrcSpan sourceFile (i * 3) 1 (i * 3 + 2) 80,
          Def.signatureSpan = Just (mkBenchSrcSpan sourceFile (i * 3 - 1) 1 (i * 3 - 1) 60)
        }

    definitionIdFor i =
      mkBenchDefinitionId module_ (Def.declarationSpan (declarationSpansByIndex Map.! i))

    definitionNameFor i =
      mkBenchNameAt
        module_
        (Def.declarationSpan (declarationSpansByIndex Map.! i))
        ("def" <> show i)
        (10_000 + i)

    typedDefinitionNames = [definitionNameFor i | i <- [1 .. definitionCount]]

    typedImports = [mkTypedImport i | i <- [1 .. max 1 importCount]]

    typedOccurrences =
      concat
        [ occurrencesForDefinition definitionIndex
        | definitionIndex <- [1 .. definitionCount]
        ]

    occurrencesForDefinition definitionIndex =
      [ Def.MinimalTypedOccurrence
          { Def.typedOccurrenceName = targetName,
            Def.typedOccurrenceSpan = mkBenchSrcSpan sourceFile (definitionIndex * 3 + 1) (2 + occurrenceIndex) (definitionIndex * 3 + 1) (4 + occurrenceIndex),
            Def.typedOccurrenceParent =
              if occurrenceIndex `mod` 6 == 0
                then Just (mkBenchTcName module_ ("Parent" <> show (definitionIndex `mod` 9)) (30_000 + definitionIndex))
                else Nothing,
            Def.typedOccurrenceCandidates = occurrenceCandidateImports occurrenceIndex
          }
      | occurrenceIndex <- [1 .. occurrencesPerDefinition],
        let targetIndex = ((definitionIndex + occurrenceIndex) `mod` definitionCount) + 1,
        let targetName = definitionNameFor targetIndex
      ]

    occurrenceCandidateImports occurrenceIndex
      | importCount <= 0 = []
      | occurrenceIndex `mod` 3 == 0 =
          [ Def.ImportId ((occurrenceIndex `mod` importCount) + 1),
            Def.ImportId (((occurrenceIndex + 7) `mod` importCount) + 1)
          ]
      | otherwise =
          [Def.ImportId ((occurrenceIndex `mod` importCount) + 1)]

    parsedFacts =
      Def.ParsedModuleFacts
        { Def.parsedOccKeys =
            Set.fromList (map (Def.nameOccKey . Def.typedOccurrenceName) typedOccurrences),
          Def.parsedDeclarationsById =
            Map.fromList
              [ (definitionIdFor i, declarationSpansByIndex Map.! i)
              | i <- [1 .. definitionCount]
              ],
          Def.parsedDefinitionMembersById = Map.empty,
          Def.parsedOccurrenceSyntaxBySpan = Map.empty,
          Def.parsedRegionCandidates =
            [ Def.SourceRegionCandidate
                { Def.candidateRegionKind = Def.BindingRegion,
                  Def.candidateRegionSpan = Def.declarationSpan (declarationSpansByIndex Map.! i)
                }
            | i <- [1 .. definitionCount]
            ]
        }

    typedFacts =
      Def.MinimalTypedModuleFacts
        { Def.typedDefinitionNames,
          Def.typedInstanceNames = [],
          Def.typedDefinitionOccAliases = Map.empty,
          Def.typedExportedNames = typedDefinitionNames,
          Def.typedExportedOccAliases = Map.empty,
          Def.typedSourceImports = typedImports,
          Def.typedOccurrences
        }

    coreFacts =
      Def.MinimalCoreModuleFacts
        { Def.coreUsedInstancesByBinder =
            Map.fromList
              [ (definitionNameFor i, [mkBenchName module_ ("inst" <> show i)])
              | i <- [1 .. definitionCount]
              ]
        }

mkTypedImport :: Int -> Def.MinimalTypedImport
mkTypedImport importId =
  Def.MinimalTypedImport
    { Def.typedImportId = Def.ImportId importId,
      Def.typedImportModule = GHC.mkModuleName ("Bench.Import" <> show importId),
      Def.typedImportPackageQualifier = Nothing,
      Def.typedImportSource = False,
      Def.typedImportQualifiedStyle = Def.NotQualified,
      Def.typedImportAlias = Nothing,
      Def.typedImportOriginallyExplicit = True
    }

mkMinifiedImportsFixture :: Int -> Int -> Bool -> MinifiedImportsFixture
mkMinifiedImportsFixture importCount occurrenceCount ambiguous =
  MinifiedImportsFixture
    { minifiedImportCandidates = importCandidates,
      minifiedOccurrences = occurrences
    }
  where
    module_ = mkBenchModule "Fixture.Bench.Minified"

    importCandidates = map mkImportCandidate [1 .. max 1 importCount]

    mkImportCandidate importId =
      Def.ImportCandidate
        { Def.importCandidateId = Def.ImportId importId,
          Def.importCandidateBaseImport =
            Def.RequiredImport
              { Def.importKey = importId,
                Def.importModule = GHC.mkModuleName ("Bench.Import" <> show importId),
                Def.importPackageQualifier = Nothing,
                Def.importSource = False,
                Def.importQualifiedStyle = Def.NotQualified,
                Def.importAlias = Nothing,
                Def.importOriginallyExplicit = True,
                Def.importItems = []
              }
        }

    occurrences =
      [ Def.DefinitionOccurrenceFact
          { Def.occurrenceFactName =
              mkBenchName module_ ("value" <> show ((occurrenceIndex `mod` 200) + 1)),
            Def.occurrenceFactSpan = mkBenchSrcSpan "Fixture/Bench/Minified.hs" lineNo 5 lineNo 20,
            Def.occurrenceFactOwners = Set.empty,
            Def.occurrenceFactParent =
              if occurrenceIndex `mod` 11 == 0
                then Just (mkBenchTcName module_ ("Parent" <> show (occurrenceIndex `mod` 17)) (70_000 + occurrenceIndex))
                else Nothing,
            Def.occurrenceFactImportCandidates = candidatesFor occurrenceIndex
          }
      | occurrenceIndex <- [1 .. occurrenceCount],
        let lineNo = (occurrenceIndex `mod` 400) + 1
      ]

    candidatesFor occurrenceIndex
      | importCount <= 0 = []
      | ambiguous =
          [ Def.ImportId ((occurrenceIndex `mod` importCount) + 1),
            Def.ImportId (((occurrenceIndex + 3) `mod` importCount) + 1)
          ]
      | otherwise =
          [Def.ImportId ((occurrenceIndex `mod` importCount) + 1)]

mkReferenceSearchFixture :: Int -> Int -> Bool -> ReferenceSearchFixture
mkReferenceSearchFixture definitionCount hitsPerDefinition commonOcc =
  ReferenceSearchFixture
    { referenceFixtureModuleIndex =
        Def.DefinitionModuleIndex
          { Def.definitionsById,
            Def.definitionIdByName,
            Def.referenceHitsByOccKey,
            Def.dependenciesById,
            Def.requiredImportsById
          },
      referenceFixtureTargetNames = targetNames,
      referenceFixtureOccurrenceIndex = occurrenceIndex,
      referenceFixtureDuplicateHits = duplicateHits
    }
  where
    module_ = mkBenchModule "Fixture.Bench.References"
    sourceFile = "Fixture/Bench/References.hs"

    definitionTriples =
      [ (index, definitionId, definitionName, definitionSource)
      | index <- [1 .. definitionCount],
        let span' = mkBenchSrcSpan sourceFile (index * 2) 1 (index * 2 + 1) 70,
        let definitionId = mkBenchDefinitionId module_ span',
        let definitionName = mkBenchNameAt module_ span' ("def" <> show index) (80_000 + index),
        let definitionSource =
              Def.DefinitionSource
                { Def.definitionSourceId = definitionId,
                  Def.definitionSourceModule = module_,
                  Def.definitionSourceNames = Set.singleton definitionName,
                  Def.definitionSourceSpans = Def.DeclarationSpans span' Nothing
                }
      ]

    definitionsById =
      Map.fromList
        [ (definitionId, definitionSource)
        | (_, definitionId, _, definitionSource) <- definitionTriples
        ]

    definitionIdByName =
      Map.fromList
        [ (definitionName, definitionId)
        | (_, definitionId, definitionName, _) <- definitionTriples
        ]

    dependenciesById =
      Map.fromList
        [ (definitionId, Def.DefinitionDependencies Set.empty Set.empty Map.empty Map.empty)
        | (_, definitionId, _, _) <- definitionTriples
        ]

    requiredImportsById =
      Map.fromList
        [ (definitionId, [])
        | (_, definitionId, _, _) <- definitionTriples
        ]

    targetModule = mkBenchModule "Fixture.Bench.Targets"
    targetName = mkBenchName targetModule (if commonOcc then "run" else "rareRun")

    decoyNames =
      [ mkBenchNameAt targetModule GHC.noSrcSpan (if commonOcc then "run" else "rareRun") (90_000 + i)
      | i <- [1 .. definitionCount]
      ]

    targetNames = [targetName]

    hitsForDefinition (index, definitionId, _, _) =
      [ mkHit definitionId index hitIndex
      | hitIndex <- [1 .. hitsPerDefinition]
      ]

    mkHit definitionId index hitIndex =
      Def.ReferenceHit
        { Def.referenceHitDefinitionId = definitionId,
          Def.referenceHitTargetName =
            if commonOcc
              then
                if hitIndex `mod` 5 == 0
                  then targetName
                  else decoyNames !! ((hitIndex + index) `mod` length decoyNames)
              else targetName,
          Def.referenceHitExactSpan = mkBenchSrcSpan sourceFile (hitIndex + index) 8 (hitIndex + index) 18
        }

    allHits = concatMap hitsForDefinition definitionTriples

    referenceHitsByOccKey =
      Map.fromListWith
        (<>)
        [ (Def.nameOccKey (Def.referenceHitTargetName hit), [hit])
        | hit <- allHits
        ]

    duplicateHits =
      case allHits of
        firstHit : _ -> firstHit : firstHit : take 6 allHits
        [] -> []

    occurrenceIndex =
      Map.fromListWith
        (<>)
        [ (Def.nameOccKey (Def.referenceHitTargetName hit), Set.singleton module_)
        | hit <- allHits
        ]

mkSourceTextFixture :: Int -> Int -> SourceTextFixture
mkSourceTextFixture lineCount editCount =
  SourceTextFixture
    { sourceTextFixtureContents =
        T.unlines
          [ T.pack ("line_" <> show lineNo <> " = value_" <> show lineNo)
          | lineNo <- [1 .. lineCount]
          ],
      sourceTextFixtureEdits =
        [ Edit.ReplaceSpanEdit
            "Fixture/Bench/Edits.hs"
            (Span lineFilePath lineNo 1 lineNo 10)
            (T.pack ("edited_" <> show editIndex))
        | editIndex <- [1 .. editCount],
          let lineNo = ((editIndex * 37) `mod` lineCount) + 1
        ]
    }
  where
    lineFilePath = "Fixture/Bench/Edits.hs"

mkSourceRegionFixture :: Int -> Int -> SourceRegionFixture
mkSourceRegionFixture candidateCount depth =
  SourceRegionFixture
    { sourceRegionFixtureDeclaration = declarationSpans,
      sourceRegionFixtureCandidates = candidates,
      sourceRegionFixtureReferenceSpan = mkBenchSrcSpan "Fixture/Bench/Rendering.hs" 12 12 12 18
    }
  where
    declarationSpans =
      Def.DeclarationSpans
        { Def.declarationSpan = mkBenchSrcSpan "Fixture/Bench/Rendering.hs" 1 1 200 120,
          Def.signatureSpan = Just (mkBenchSrcSpan "Fixture/Bench/Rendering.hs" 1 1 1 30)
        }

    candidates =
      [ Def.SourceRegionCandidate
          { Def.candidateRegionKind = kindFor index,
            Def.candidateRegionSpan = spanFor index
          }
      | index <- [1 .. candidateCount]
      ]

    kindFor index =
      case index `mod` 5 of
        0 -> Def.MatchRegion
        1 -> Def.GuardRegion
        2 -> Def.StatementRegion
        3 -> Def.ApplicationRegion
        _ -> Def.BindingRegion

    spanFor index =
      let level = (index `mod` depth) + 1
          startLine = 2 + level + (index `mod` 30)
          endLine = min 199 (startLine + level + 1)
          startCol = 2 + (index `mod` 20)
          endCol = min 118 (startCol + 10 + level)
       in mkBenchSrcSpan "Fixture/Bench/Rendering.hs" startLine startCol endLine endCol

mkBenchNameAt :: GHC.Module -> GHC.SrcSpan -> String -> Int -> GHC.Name
mkBenchNameAt module_ span' occurrence uniqueSeed =
  GHC.mkExternalName
    (Unique.mkUnique 'b' uniqueSeed)
    module_
    (GHC.mkVarOcc occurrence)
    span'

mkBenchTcName :: GHC.Module -> String -> Int -> GHC.Name
mkBenchTcName module_ occurrence uniqueSeed =
  GHC.mkExternalName
    (Unique.mkUnique 'c' uniqueSeed)
    module_
    (GHC.mkTcOcc occurrence)
    GHC.noSrcSpan

stableUnique :: String -> Int
stableUnique =
  foldl
    (\acc ch -> acc * 167 + ord ch)
    19
