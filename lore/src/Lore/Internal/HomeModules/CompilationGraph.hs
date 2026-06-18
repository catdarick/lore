{-# LANGUAGE CPP #-}

module Lore.Internal.HomeModules.CompilationGraph
  ( AnalyzeHomeModuleCompilationOptions (..),
    HomeModuleCompilationAnalysisResult (..),
    HomeModuleCompilationAnalysis (..),
    HomeModuleCompilationComponentAnalysis (..),
    HomeModuleCompilationSummary (..),
    HomeModuleCompilationNode (..),
    ModuleCompileMetrics (..),
    analyzeHomeModuleCompilation,
  )
where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (execState, get, modify')
import Data.Char (isAlphaNum, isDigit, isSpace)
import Data.Function (on)
import Data.Graph (SCC (..), stronglyConnComp)
import Data.List (foldl', isSuffixOf, maximumBy, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Time.Clock as Time
import qualified GHC
import qualified GHC.Unit.Module.Graph as ModuleGraph
import qualified GHC.Unit.Types as Unit
import Lore.Diagnostics (Diagnostic)
import Lore.Internal.HomeModules (prepareConfiguredHomeModulesLoadPlan)
import Lore.Internal.HomeModules.ModuleGraph (PreparedHomeModuleGraph (..), preparePatchedHomeModuleGraph)
import Lore.Internal.HomeModules.Plan (HomeModuleComponent (..), HomeModuleKey (..), HomeModulesLoadPlan (..))
import Lore.Internal.ProjectEnvironment.Types (ProjectEnvironmentFailure)
import Lore.Internal.SourcePath (normalizeSourceFilePathM)
import qualified Lore.Logger as Log
import Lore.Monad (MonadLore)
import System.Directory (doesDirectoryExist, doesFileExist, getModificationTime, listDirectory)
import System.FilePath (takeBaseName, takeFileName, (</>))

data AnalyzeHomeModuleCompilationOptions = AnalyzeHomeModuleCompilationOptions
  { analyzeCompilationJobs :: Int,
    analyzeCompilationTimingPaths :: [FilePath]
  }
  deriving stock (Eq, Show)

data HomeModuleCompilationAnalysisResult
  = HomeModuleCompilationAnalysisCompleted HomeModuleCompilationAnalysis
  | HomeModuleCompilationAnalysisPreparationFailed ProjectEnvironmentFailure
  deriving stock (Eq, Show)

data HomeModuleCompilationAnalysis = HomeModuleCompilationAnalysis
  { homeModuleCompilationSummary :: HomeModuleCompilationSummary,
    homeModuleCompilationNodes :: [HomeModuleCompilationNode],
    homeModuleCompilationCriticalPath :: [Text],
    homeModuleCompilationComponents :: [HomeModuleCompilationComponentAnalysis],
    homeModuleCompilationDiagnostics :: [Diagnostic]
  }
  deriving stock (Eq, Show)

data HomeModuleCompilationComponentAnalysis = HomeModuleCompilationComponentAnalysis
  { componentCompilationName :: Text,
    componentCompilationSummary :: HomeModuleCompilationSummary,
    componentCompilationNodes :: [HomeModuleCompilationNode],
    componentCompilationCriticalPath :: [Text]
  }
  deriving stock (Eq, Show)

data HomeModuleCompilationSummary = HomeModuleCompilationSummary
  { compilationTotalModules :: Int,
    compilationComponentCount :: Int,
    compilationTotalImports :: Int,
    compilationCycleCount :: Int,
    compilationLargestCycleSize :: Int,
    compilationJobs :: Int,
    compilationTotalWorkSeconds :: Double,
    compilationCriticalPathSeconds :: Double,
    compilationIdealParallelSeconds :: Double,
    compilationParallelismEfficiency :: Double,
    compilationModulesWithTiming :: Int,
    compilationTimingFileCount :: Int
  }
  deriving stock (Eq, Show)

data HomeModuleCompilationNode = HomeModuleCompilationNode
  { compilationModuleName :: Text,
    compilationComponentName :: Text,
    compilationSourcePath :: Maybe FilePath,
    compilationImports :: [Text],
    compilationImportedBy :: [Text],
    compilationDirectDependentCount :: Int,
    compilationTransitiveDependentCount :: Int,
    compilationBuildLayer :: Int,
    compilationDownstreamCriticalPathLength :: Int,
    compilationCompileMetrics :: Maybe ModuleCompileMetrics,
    compilationBottleneckScore :: Double,
    compilationCycleMembers :: [Text]
  }
  deriving stock (Eq, Show)

data ModuleCompileMetrics = ModuleCompileMetrics
  { moduleCompileTimeSeconds :: Double,
    moduleCompileAllocBytes :: Integer,
    moduleCompileTimingFiles :: [FilePath]
  }
  deriving stock (Eq, Show)

analyzeHomeModuleCompilation :: (MonadLore m) => AnalyzeHomeModuleCompilationOptions -> m HomeModuleCompilationAnalysisResult
analyzeHomeModuleCompilation options = do
  configuredPlan <- prepareConfiguredHomeModulesLoadPlan
  case configuredPlan of
    Left failure ->
      pure (HomeModuleCompilationAnalysisPreparationFailed failure)
    Right plan -> do
      preparedGraph <- preparePatchedHomeModuleGraph plan
      Log.debug "Loading per-module timing samples..."
      metricsByModule <- liftIO (loadCompileMetrics options.analyzeCompilationTimingPaths)
      Log.debug $
        "Loaded timing samples for "
          <> show (Map.size metricsByModule)
          <> " modules."
      Log.debug "Building per-component compilation graphs..."
      analysis <-
        buildCompilationAnalysis
          (max 1 options.analyzeCompilationJobs)
          metricsByModule
          plan
          preparedGraph
      Log.debug $
        "Built "
          <> show analysis.homeModuleCompilationSummary.compilationComponentCount
          <> " component compilation graphs."
      pure (HomeModuleCompilationAnalysisCompleted analysis)

buildCompilationAnalysis :: (MonadLore m) => Int -> Map.Map Text ModuleCompileMetrics -> HomeModulesLoadPlan -> PreparedHomeModuleGraph -> m HomeModuleCompilationAnalysis
buildCompilationAnalysis jobs metricsByModule plan preparedGraph = do
  rawNodesByComponent <- rawNodesByComponentName plan.homeModulesComponents preparedGraph.preparedHomeModuleGraphModuleGraph
  let componentAnalyses =
        [ analyzeComponentGraph jobs metricsByModule componentName rawNodes
        | (componentName, rawNodes) <- Map.toAscList rawNodesByComponent
        ]
      nodes = sortOn (negate . compilationBottleneckScore) (concatMap componentCompilationNodes componentAnalyses)
      criticalPath = longestCriticalPath componentAnalyses
      summary = summarizeComponentAnalyses jobs componentAnalyses
  pure
    HomeModuleCompilationAnalysis
      { homeModuleCompilationSummary = summary,
        homeModuleCompilationNodes = nodes,
        homeModuleCompilationCriticalPath = criticalPath,
        homeModuleCompilationComponents = componentAnalyses,
        homeModuleCompilationDiagnostics = preparedGraph.preparedHomeModuleGraphDiagnostics
      }

summarizeComponentAnalyses :: Int -> [HomeModuleCompilationComponentAnalysis] -> HomeModuleCompilationSummary
summarizeComponentAnalyses jobs componentAnalyses =
  HomeModuleCompilationSummary
    { compilationTotalModules = sumIntComponent compilationTotalModules,
      compilationComponentCount = length componentAnalyses,
      compilationTotalImports = sumIntComponent compilationTotalImports,
      compilationCycleCount = sumIntComponent compilationCycleCount,
      compilationLargestCycleSize = maximum (0 : mapIntComponent compilationLargestCycleSize),
      compilationJobs = jobs,
      compilationTotalWorkSeconds = totalWorkSeconds,
      compilationCriticalPathSeconds = criticalPathSeconds,
      compilationIdealParallelSeconds = idealParallelSeconds,
      compilationParallelismEfficiency = efficiency,
      compilationModulesWithTiming = sumIntComponent compilationModulesWithTiming,
      compilationTimingFileCount = sumIntComponent compilationTimingFileCount
    }
  where
    mapIntComponent field = map (field . componentCompilationSummary) componentAnalyses
    mapDoubleComponent field = map (field . componentCompilationSummary) componentAnalyses
    sumIntComponent field = sum (mapIntComponent field)
    sumDoubleComponent field = sum (mapDoubleComponent field)
    totalWorkSeconds = sumDoubleComponent compilationTotalWorkSeconds
    criticalPathSeconds = maximum (0 : mapDoubleComponent compilationCriticalPathSeconds)
    idealParallelSeconds = totalWorkSeconds / fromIntegral jobs
    estimatedWallSeconds = max criticalPathSeconds idealParallelSeconds
    efficiency
      | estimatedWallSeconds <= 0 = 1
      | otherwise = totalWorkSeconds / (fromIntegral jobs * estimatedWallSeconds)

analyzeComponentGraph :: Int -> Map.Map Text ModuleCompileMetrics -> Text -> [RawNode] -> HomeModuleCompilationComponentAnalysis
analyzeComponentGraph jobs metricsByModule componentName rawNodes =
  HomeModuleCompilationComponentAnalysis
    { componentCompilationName = componentName,
      componentCompilationSummary = summary,
      componentCompilationNodes = sortOn (negate . compilationBottleneckScore) nodes,
      componentCompilationCriticalPath = criticalPath
    }
  where
    graph = Map.fromList [(nodeName node, nodeImports node) | node <- rawNodes]
    homeModules = Map.keysSet graph
    reverseGraph = buildReverseGraph homeModules graph
    components = graphComponents graph
    componentOf = componentIndex components
    componentMembersById = Map.fromList [(componentId component, componentMembers component) | component <- components]
    componentDeps = componentDependencyGraph componentOf graph
    reverseComponentDeps = reverseComponentDependencyGraph componentMembersById componentDeps
    componentLayers = componentDepths componentMembersById componentDeps
    componentDownstreamDepths = componentDepths componentMembersById reverseComponentDeps
    nodes =
      [ enrichNode
          componentName
          metricsByModule
          reverseGraph
          componentOf
          componentMembersById
          componentLayers
          componentDownstreamDepths
          rawNode
      | rawNode <- rawNodes
      ]
    criticalPath = computeCriticalPath graph metricsByModule
    totalWorkSeconds = sum [nodeDuration metricsByModule name | name <- Set.toList homeModules]
    criticalPathSeconds = sum [nodeDuration metricsByModule name | name <- criticalPath]
    idealParallelSeconds = totalWorkSeconds / fromIntegral jobs
    estimatedWallSeconds = max criticalPathSeconds idealParallelSeconds
    efficiency
      | estimatedWallSeconds <= 0 = 1
      | otherwise = totalWorkSeconds / (fromIntegral jobs * estimatedWallSeconds)
    cycleSizes = [length (componentMembers component) | component <- components, length (componentMembers component) > 1]
    summary =
      HomeModuleCompilationSummary
        { compilationTotalModules = Set.size homeModules,
          compilationComponentCount = 1,
          compilationTotalImports = sum (map (length . nodeImports) rawNodes),
          compilationCycleCount = length cycleSizes,
          compilationLargestCycleSize = maximum (0 : cycleSizes),
          compilationJobs = jobs,
          compilationTotalWorkSeconds = totalWorkSeconds,
          compilationCriticalPathSeconds = criticalPathSeconds,
          compilationIdealParallelSeconds = idealParallelSeconds,
          compilationParallelismEfficiency = efficiency,
          compilationModulesWithTiming = Map.size (Map.filterWithKey (\name _ -> name `Set.member` homeModules) metricsByModule),
          compilationTimingFileCount = timingFileCountForModules metricsByModule homeModules
        }

timingFileCountForModules :: Map.Map Text ModuleCompileMetrics -> Set.Set Text -> Int
timingFileCountForModules metricsByModule modules =
  Set.size $
    Set.fromList
      [ timingFile
      | (moduleName, metrics) <- Map.toList metricsByModule,
        moduleName `Set.member` modules,
        timingFile <- metrics.moduleCompileTimingFiles
      ]

longestCriticalPath :: [HomeModuleCompilationComponentAnalysis] -> [Text]
longestCriticalPath [] = []
longestCriticalPath analyses =
  componentCompilationCriticalPath (maximumBy (compare `on` (compilationCriticalPathSeconds . componentCompilationSummary)) analyses)

data RawModuleInfo = RawModuleInfo
  { rawModuleInfoName :: Text,
    rawModuleInfoSourcePath :: Maybe FilePath,
    rawModuleInfoComponents :: Set.Set Text,
    rawModuleInfoDependencies :: [Text]
  }
  deriving stock (Eq, Show)

data RawNode = RawNode
  { nodeName :: Text,
    nodeSourcePath :: Maybe FilePath,
    nodeImports :: [Text]
  }
  deriving stock (Eq, Show)

rawNodesByComponentName :: (MonadLore m) => Map.Map HomeModuleKey (Set.Set HomeModuleComponent) -> GHC.ModuleGraph -> m (Map.Map Text [RawNode])
rawNodesByComponentName homeModuleComponents moduleGraph = do
  rawModuleInfos <- catMaybes <$> mapM (graphNodeToRawModuleInfo homeModuleComponents) (ModuleGraph.mgModSummaries' moduleGraph)
  let componentsByModuleName =
        Map.fromListWith
          Set.union
          [ (rawModuleInfo.rawModuleInfoName, rawModuleInfo.rawModuleInfoComponents)
          | rawModuleInfo <- rawModuleInfos
          ]
      componentNames = Set.toAscList (Set.unions (map rawModuleInfoComponents rawModuleInfos))
  pure $
    Map.fromList
      [ ( componentName,
          [ rawNodeForComponent componentsByModuleName componentName rawModuleInfo
          | rawModuleInfo <- rawModuleInfos,
            componentName `Set.member` rawModuleInfo.rawModuleInfoComponents
          ]
        )
      | componentName <- componentNames
      ]

graphNodeToRawModuleInfo :: (MonadLore m) => Map.Map HomeModuleKey (Set.Set HomeModuleComponent) -> ModuleGraph.ModuleGraphNode -> m (Maybe RawModuleInfo)
graphNodeToRawModuleInfo homeModuleComponents graphNode =
  case moduleGraphNodeSummaryAndDeps graphNode of
    Nothing ->
      pure Nothing
    Just (deps, summary) -> do
      sourcePath <- traverse normalizeSourceFilePathM (GHC.ml_hs_file (GHC.ms_location summary))
      let name = moduleNameText (GHC.ms_mod_name summary)
          components = componentsForSummary homeModuleComponents (GHC.ms_mod_name summary) sourcePath
          dependencyNames = Set.toAscList (Set.delete name (Set.fromList (mapMaybe nodeKeyModuleName deps)))
      pure $
        Just
          RawModuleInfo
            { rawModuleInfoName = name,
              rawModuleInfoSourcePath = sourcePath,
              rawModuleInfoComponents = components,
              rawModuleInfoDependencies = dependencyNames
            }

{- ORMOLU_DISABLE -}
moduleGraphNodeSummaryAndDeps :: ModuleGraph.ModuleGraphNode -> Maybe ([ModuleGraph.NodeKey], GHC.ModSummary)
moduleGraphNodeSummaryAndDeps graphNode =
  case graphNode of
#if MIN_VERSION_ghc(9,14,0)
    ModuleGraph.ModuleNode deps (ModuleGraph.ModuleNodeCompile summary) -> Just (deps, summary)
#else
    ModuleGraph.ModuleNode deps summary -> Just (deps, summary)
#endif
    _ -> Nothing
{- ORMOLU_ENABLE -}

nodeKeyModuleName :: ModuleGraph.NodeKey -> Maybe Text
nodeKeyModuleName nodeKey =
  case nodeKey of
    ModuleGraph.NodeKey_Module moduleKey -> Just (moduleNameText (Unit.gwib_mod (ModuleGraph.mnkModuleName moduleKey)))
    _ -> Nothing

rawNodeForComponent :: Map.Map Text (Set.Set Text) -> Text -> RawModuleInfo -> RawNode
rawNodeForComponent componentsByModuleName componentName rawModuleInfo =
  RawNode
    { nodeName = rawModuleInfo.rawModuleInfoName,
      nodeSourcePath = rawModuleInfo.rawModuleInfoSourcePath,
      nodeImports = imports
    }
  where
    imports =
      Set.toAscList $
        Set.fromList
          [ imported
          | imported <- rawModuleInfo.rawModuleInfoDependencies,
            componentName `Set.member` Map.findWithDefault Set.empty imported componentsByModuleName
          ]

componentsForSummary :: Map.Map HomeModuleKey (Set.Set HomeModuleComponent) -> GHC.ModuleName -> Maybe FilePath -> Set.Set Text
componentsForSummary homeModuleComponents moduleName sourcePath =
  case Set.unions componentSets of
    components | Set.null components -> Set.singleton unassignedComponentName
    components -> Set.map renderHomeModuleComponent components
  where
    componentSets =
      [ Map.findWithDefault Set.empty (HomeModuleName moduleName) homeModuleComponents,
        maybe Set.empty (\path -> Map.findWithDefault Set.empty (HomeModuleSourceFile path) homeModuleComponents) sourcePath
      ]

renderHomeModuleComponent :: HomeModuleComponent -> Text
renderHomeModuleComponent component =
  T.pack component.homeModuleComponentPackageName
    <> ":"
    <> T.pack component.homeModuleComponentName

unassignedComponentName :: Text
unassignedComponentName = "unassigned"

enrichNode ::
  Text ->
  Map.Map Text ModuleCompileMetrics ->
  Map.Map Text [Text] ->
  Map.Map Text Int ->
  Map.Map Int [Text] ->
  Map.Map Int Int ->
  Map.Map Int Int ->
  RawNode ->
  HomeModuleCompilationNode
enrichNode componentName metricsByModule reverseGraph componentOf componentMembersById componentLayers componentDownstreamDepths rawNode =
  HomeModuleCompilationNode
    { compilationModuleName = name,
      compilationComponentName = componentName,
      compilationSourcePath = nodeSourcePath rawNode,
      compilationImports = imports,
      compilationImportedBy = importedBy,
      compilationDirectDependentCount = length importedBy,
      compilationTransitiveDependentCount = Set.size transitiveDependents,
      compilationBuildLayer = maybe 0 (\componentId -> Map.findWithDefault 0 componentId componentLayers) maybeComponentId,
      compilationDownstreamCriticalPathLength = maybe 0 (\componentId -> Map.findWithDefault 0 componentId componentDownstreamDepths) maybeComponentId,
      compilationCompileMetrics = metrics,
      compilationBottleneckScore = bottleneckScore metrics transitiveDependents importedBy,
      compilationCycleMembers = cycleMembers
    }
  where
    name = nodeName rawNode
    imports = nodeImports rawNode
    importedBy = Map.findWithDefault [] name reverseGraph
    transitiveDependents = reachable reverseGraph name
    metrics = Map.lookup name metricsByModule
    maybeComponentId = Map.lookup name componentOf
    cycleMembers =
      case maybeComponentId >>= (`Map.lookup` componentMembersById) of
        Just members | length members > 1 -> members
        _ -> []

bottleneckScore :: Maybe ModuleCompileMetrics -> Set.Set Text -> [Text] -> Double
bottleneckScore metrics transitiveDependents importedBy =
  duration * (1 + fromIntegral (Set.size transitiveDependents)) + fromIntegral (length importedBy)
  where
    duration = maybe 1 moduleCompileTimeSeconds metrics

buildReverseGraph :: Set.Set Text -> Map.Map Text [Text] -> Map.Map Text [Text]
buildReverseGraph homeModules graph =
  foldl' addModule emptyReverse (Map.toList graph)
  where
    emptyReverse = Map.fromSet (const []) homeModules
    addModule reverseMap (name, imports) =
      foldl' (\acc imported -> Map.adjust (name :) imported acc) reverseMap imports

reachable :: Map.Map Text [Text] -> Text -> Set.Set Text
reachable graph start =
  Set.delete start (go Set.empty [start])
  where
    go seen [] = seen
    go seen (name : rest)
      | name `Set.member` seen = go seen rest
      | otherwise = go (Set.insert name seen) (Map.findWithDefault [] name graph <> rest)

data Component = Component
  { componentId :: Int,
    componentMembers :: [Text]
  }
  deriving stock (Eq, Show)

graphComponents :: Map.Map Text [Text] -> [Component]
graphComponents graph =
  zipWith toComponent [0 ..] (stronglyConnComp [(name, name, imports) | (name, imports) <- Map.toList graph])
  where
    toComponent componentId scc =
      Component
        { componentId,
          componentMembers =
            case scc of
              AcyclicSCC name -> [name]
              CyclicSCC names -> names
        }

componentIndex :: [Component] -> Map.Map Text Int
componentIndex components =
  Map.fromList
    [ (member, component.componentId)
    | component <- components,
      member <- component.componentMembers
    ]

componentDependencyGraph :: Map.Map Text Int -> Map.Map Text [Text] -> Map.Map Int (Set.Set Int)
componentDependencyGraph componentOf graph =
  Map.fromListWith
    Set.union
    [ (componentId, Set.fromList dependencyComponentIds)
    | (name, imports) <- Map.toList graph,
      Just componentId <- [Map.lookup name componentOf],
      let dependencyComponentIds =
            [ dependencyComponentId
            | imported <- imports,
              Just dependencyComponentId <- [Map.lookup imported componentOf],
              dependencyComponentId /= componentId
            ]
    ]

reverseComponentDependencyGraph :: Map.Map Int [Text] -> Map.Map Int (Set.Set Int) -> Map.Map Int (Set.Set Int)
reverseComponentDependencyGraph componentMembersById componentDeps =
  foldl' addDeps emptyReverse (Map.toList componentDeps)
  where
    emptyReverse = Map.fromSet (const Set.empty) (Map.keysSet componentMembersById)
    addDeps reverseMap (componentId, deps) =
      foldl' (\acc dep -> Map.adjust (Set.insert componentId) dep acc) reverseMap (Set.toList deps)

componentDepths :: Map.Map Int [Text] -> Map.Map Int (Set.Set Int) -> Map.Map Int Int
componentDepths componentMembersById graph =
  execState (mapM_ componentDepth (Map.keys componentMembersById)) Map.empty
  where
    componentDepth = componentDepthFrom Set.empty

    componentDepthFrom seen componentId
      | componentId `Set.member` seen =
          pure 0
      | otherwise = do
          memo <- get
          case Map.lookup componentId memo of
            Just cached ->
              pure cached
            Nothing -> do
              let deps = Set.toList (Map.findWithDefault Set.empty componentId graph)
              depDepths <- mapM (componentDepthFrom (Set.insert componentId seen)) deps
              let depth =
                    case depDepths of
                      [] -> 0
                      _ -> 1 + maximum depDepths
              modify' (Map.insert componentId depth)
              pure depth

computeCriticalPath :: Map.Map Text [Text] -> Map.Map Text ModuleCompileMetrics -> [Text]
computeCriticalPath graph metricsByModule
  | Map.null graph = []
  | otherwise = concatMap membersForComponent (reverse (criticalComponentPath Set.empty endComponent))
  where
    components = graphComponents graph
    componentOf = componentIndex components
    componentMembersById = Map.fromList [(component.componentId, component.componentMembers) | component <- components]
    componentDeps = componentDependencyGraph componentOf graph
    componentIds = Map.keys componentMembersById
    componentCriticalCosts = computeComponentCriticalCosts componentIds componentDeps componentDuration
    endComponent = maximumBy (compare `on` componentCriticalCost) componentIds

    componentCriticalCost componentId =
      Map.findWithDefault (componentDuration componentId) componentId componentCriticalCosts

    criticalComponentPath seen componentId
      | componentId `Set.member` seen = []
    criticalComponentPath seen componentId =
      componentId
        : case filter (`Set.notMember` seen) (Set.toList (Map.findWithDefault Set.empty componentId componentDeps)) of
          [] -> []
          deps -> criticalComponentPath (Set.insert componentId seen) (maximumBy (compare `on` componentCriticalCost) deps)

    componentDuration componentId =
      sum [nodeDuration metricsByModule name | name <- membersForComponent componentId]

    membersForComponent componentId =
      Map.findWithDefault [] componentId componentMembersById

computeComponentCriticalCosts :: [Int] -> Map.Map Int (Set.Set Int) -> (Int -> Double) -> Map.Map Int Double
computeComponentCriticalCosts componentIds componentDeps componentDuration =
  execState (mapM_ componentCriticalCost componentIds) Map.empty
  where
    componentCriticalCost = componentCriticalCostFrom Set.empty

    componentCriticalCostFrom seen componentId
      | componentId `Set.member` seen =
          pure 0
      | otherwise = do
          memo <- get
          case Map.lookup componentId memo of
            Just cached ->
              pure cached
            Nothing -> do
              let deps = Set.toList (Map.findWithDefault Set.empty componentId componentDeps)
              depCosts <- mapM (componentCriticalCostFrom (Set.insert componentId seen)) deps
              let cost = componentDuration componentId + maximum (0 : depCosts)
              modify' (Map.insert componentId cost)
              pure cost

nodeDuration :: Map.Map Text ModuleCompileMetrics -> Text -> Double
nodeDuration metricsByModule name =
  maybe 1 moduleCompileTimeSeconds (Map.lookup name metricsByModule)

data TimingSample = TimingSample
  { timingSampleModuleName :: Text,
    timingSampleVariant :: Text,
    timingSampleModified :: Time.UTCTime,
    timingSampleMetrics :: ModuleCompileMetrics
  }
  deriving stock (Eq, Show)

loadCompileMetrics :: [FilePath] -> IO (Map.Map Text ModuleCompileMetrics)
loadCompileMetrics paths = do
  timingFiles <- Set.toAscList . Set.fromList . concat <$> mapM collectTimingFiles paths
  samples <- catMaybes <$> mapM parseTimingFile timingFiles
  let latestSamplesByModuleVariant =
        Map.fromListWith
          chooseLatestTimingSample
          [ ((sample.timingSampleModuleName, sample.timingSampleVariant), sample)
          | sample <- samples
          ]
      metricsByModule =
        Map.fromListWith
          mergeMetrics
          [ (sample.timingSampleModuleName, sample.timingSampleMetrics)
          | sample <- Map.elems latestSamplesByModuleVariant
          ]
  pure metricsByModule

chooseLatestTimingSample :: TimingSample -> TimingSample -> TimingSample
chooseLatestTimingSample left right =
  case compare left.timingSampleModified right.timingSampleModified of
    GT -> left
    LT -> right
    EQ
      | leftTime >= rightTime -> left
      | otherwise -> right
  where
    leftTime = left.timingSampleMetrics.moduleCompileTimeSeconds
    rightTime = right.timingSampleMetrics.moduleCompileTimeSeconds

mergeMetrics :: ModuleCompileMetrics -> ModuleCompileMetrics -> ModuleCompileMetrics
mergeMetrics left right =
  ModuleCompileMetrics
    { moduleCompileTimeSeconds = left.moduleCompileTimeSeconds + right.moduleCompileTimeSeconds,
      moduleCompileAllocBytes = left.moduleCompileAllocBytes + right.moduleCompileAllocBytes,
      moduleCompileTimingFiles = left.moduleCompileTimingFiles <> right.moduleCompileTimingFiles
    }

collectTimingFiles :: FilePath -> IO [FilePath]
collectTimingFiles path = do
  isFile <- doesFileExist path
  isDirectory <- doesDirectoryExist path
  if
    | isFile -> pure [path | isTimingDumpPath path]
    | isDirectory -> do
        children <- listDirectory path
        concat <$> mapM (collectTimingFiles . (path </>)) children
    | otherwise -> pure []

isTimingDumpPath :: FilePath -> Bool
isTimingDumpPath path =
  any (`isSuffixOf` fileName) [".dump-timings", ".timings"]
  where
    fileName = takeFileName path

parseTimingFile :: FilePath -> IO (Maybe TimingSample)
parseTimingFile path = do
  modified <- getModificationTime path
  contents <- readFile path
  let rawLines = lines contents
      moduleName = fromMaybe (moduleNameFromTimingFilePath path) (firstJust (map moduleNameFromTimingLine rawLines))
      lineMetrics = map parseTimingLine rawLines
      totalTime = sum (map fst lineMetrics)
      totalAlloc = sum (map snd lineMetrics)
  pure $
    if totalTime <= 0 && totalAlloc <= 0
      then Nothing
      else
        Just
          TimingSample
            { timingSampleModuleName = moduleName,
              timingSampleVariant = timingVariantFromPath path,
              timingSampleModified = modified,
              timingSampleMetrics =
                ModuleCompileMetrics
                  { moduleCompileTimeSeconds = totalTime,
                    moduleCompileAllocBytes = totalAlloc,
                    moduleCompileTimingFiles = [path]
                  }
            }

parseTimingLine :: String -> (Double, Integer)
parseTimingLine rawLine =
  (fromMaybe 0 (parseLineSeconds text), fromMaybe 0 (parseLineAllocBytes text))
  where
    text = T.pack rawLine

parseLineSeconds :: Text -> Maybe Double
parseLineSeconds text =
  firstJust
    [ parsePrefixedSeconds text "time=",
      parsePrefixedSeconds text "elapsed=",
      parseNumberBeforeUnit text [("milliseconds", 0.001), ("millisecond", 0.001), ("ms", 0.001), ("seconds", 1), ("second", 1), ("s", 1)]
    ]

parseLineAllocBytes :: Text -> Maybe Integer
parseLineAllocBytes text =
  firstJust
    [ parsePrefixedBytes text "alloc=",
      parsePrefixedBytes text "allocated=",
      fmap floor (parseNumberBeforeUnit text byteUnits)
    ]

parsePrefixedSeconds :: Text -> Text -> Maybe Double
parsePrefixedSeconds text prefix = do
  token <- firstTokenWithPrefix prefix text
  parseNumberWithSuffixDefault 0.001 [("ms", 0.001), ("s", 1)] token

parsePrefixedBytes :: Text -> Text -> Maybe Integer
parsePrefixedBytes text prefix = do
  token <- firstTokenWithPrefix prefix text
  floor <$> parseNumberWithSuffix byteUnits token

parseNumberBeforeUnit :: Text -> [(Text, Double)] -> Maybe Double
parseNumberBeforeUnit text units =
  firstJust
    [ (* multiplier) <$> parseDouble (stripToken previous)
    | (previous, current) <- zip tokens (drop 1 tokens),
      (unit, multiplier) <- units,
      stripToken current == unit
    ]
  where
    tokens = T.words (T.toLower text)

parseNumberWithSuffix :: [(Text, Double)] -> Text -> Maybe Double
parseNumberWithSuffix units token =
  parseNumberWithSuffixDefault 1 units token

parseNumberWithSuffixDefault :: Double -> [(Text, Double)] -> Text -> Maybe Double
parseNumberWithSuffixDefault defaultMultiplier units token =
  firstJust
    [ (* multiplier) <$> parseDouble (T.dropEnd (T.length suffix) stripped)
    | (suffix, multiplier) <- units,
      suffix `T.isSuffixOf` stripped
    ]
    <|> ((* defaultMultiplier) <$> parseDouble stripped)
  where
    stripped = stripToken (T.toLower token)

firstTokenWithPrefix :: Text -> Text -> Maybe Text
firstTokenWithPrefix prefix text =
  firstJust
    [ T.stripPrefix prefix token
    | token <- map stripToken (T.words (T.toLower text))
    ]

byteUnits :: [(Text, Double)]
byteUnits =
  [ ("bytes", 1),
    ("byte", 1),
    ("b", 1),
    ("kilobytes", 1024),
    ("kb", 1024),
    ("megabytes", 1024 * 1024),
    ("mb", 1024 * 1024),
    ("gigabytes", 1024 * 1024 * 1024),
    ("gb", 1024 * 1024 * 1024)
  ]

parseDouble :: Text -> Maybe Double
parseDouble text =
  case reads (T.unpack (T.filter (/= ',') text)) of
    [(value, "")] -> Just value
    _ -> Nothing

moduleNameFromTimingLine :: String -> Maybe Text
moduleNameFromTimingLine rawLine =
  case T.breakOn "[" (T.pack rawLine) of
    (_, rest)
      | T.null rest -> Nothing
      | otherwise ->
          let candidate = T.takeWhile (/= ']') (T.drop 1 rest)
           in if isPlausibleModuleName candidate then Just candidate else Nothing

timingVariantFromPath :: FilePath -> Text
timingVariantFromPath path
  | ".dyn" `isSuffixOf` stem = "dyn"
  | ".p" `isSuffixOf` stem = "profiling"
  | otherwise = "normal"
  where
    stem = timingFileStem path

moduleNameFromTimingFilePath :: FilePath -> Text
moduleNameFromTimingFilePath path =
  T.pack (dropKnownObjectVariant (timingFileStem path))

timingFileStem :: FilePath -> String
timingFileStem path =
  dropTimingSuffix (takeFileName path)
  where
    dropTimingSuffix name
      | ".dump-timings" `isSuffixOf` name = take (length name - length (".dump-timings" :: String)) name
      | ".timings" `isSuffixOf` name = take (length name - length (".timings" :: String)) name
      | otherwise = takeBaseName name

dropKnownObjectVariant :: String -> String
dropKnownObjectVariant name
  | ".dyn" `isSuffixOf` name = take (length name - length (".dyn" :: String)) name
  | ".p" `isSuffixOf` name = take (length name - length (".p" :: String)) name
  | otherwise = name

isPlausibleModuleName :: Text -> Bool
isPlausibleModuleName text =
  not (T.null text)
    && T.all (\c -> isAlphaNum c || c == '_' || c == '.' || c == '\'') text
    && T.any (not . isDigit) text

stripToken :: Text -> Text
stripToken =
  T.dropAround (\c -> isSpace c || c == ',' || c == '(' || c == ')' || c == ';')

firstJust :: [Maybe a] -> Maybe a
firstJust =
  foldr (<|>) Nothing

moduleNameText :: GHC.ModuleName -> Text
moduleNameText =
  T.pack . GHC.moduleNameString
