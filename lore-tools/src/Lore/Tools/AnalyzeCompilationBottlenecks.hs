module Lore.Tools.AnalyzeCompilationBottlenecks
  ( AnalyzeCompilationBottlenecksOptions (..),
    analyzeCompilationBottlenecks,
    renderAnalyzeCompilationBottlenecksResult,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Lore.HomeModules as Core
import Lore.Monad (MonadLore)
import Lore.Tools.Render.Doc (LoreDoc, bulletList, heading2, numberedListFrom, paragraph)
import Numeric (showFFloat)

data AnalyzeCompilationBottlenecksOptions = AnalyzeCompilationBottlenecksOptions
  { analyzeCompilationBottlenecksJobs :: Int,
    analyzeCompilationBottlenecksTimingPaths :: [FilePath]
  }
  deriving stock (Eq, Show)

analyzeCompilationBottlenecks :: (MonadLore m) => AnalyzeCompilationBottlenecksOptions -> m Core.HomeModuleCompilationAnalysisResult
analyzeCompilationBottlenecks options =
  Core.analyzeHomeModuleCompilation
    Core.AnalyzeHomeModuleCompilationOptions
      { Core.analyzeCompilationJobs = max 1 options.analyzeCompilationBottlenecksJobs,
        Core.analyzeCompilationTimingPaths = options.analyzeCompilationBottlenecksTimingPaths
      }

renderAnalyzeCompilationBottlenecksResult :: Int -> Core.HomeModuleCompilationAnalysisResult -> LoreDoc
renderAnalyzeCompilationBottlenecksResult _ (Core.HomeModuleCompilationAnalysisPreparationFailed failure) =
  heading2 "Component Compilation Graphs"
    <> paragraph ("Project environment preparation failed: " <> T.pack (Core.projectEnvironmentFailureMessage failure))
renderAnalyzeCompilationBottlenecksResult limit (Core.HomeModuleCompilationAnalysisCompleted analysis) =
  heading2 "Component Compilation Graphs"
    <> numberedListFrom 1 (map (renderComponentGraph (max 0 limit)) analysis.homeModuleCompilationComponents)

renderTimingCoverageNotice :: Core.HomeModuleCompilationSummary -> LoreDoc
renderTimingCoverageNotice summary
  | summary.compilationModulesWithTiming == summary.compilationTotalModules = mempty
  | summary.compilationModulesWithTiming == 0 =
      paragraph $
        "No per-module timing samples were loaded. Generate them with a build using "
          <> timingFlagsInstruction
          <> ", then rerun this tool with --timings .lore-timings."
  | otherwise =
      paragraph $
        "Partial timing coverage: "
          <> showText summary.compilationModulesWithTiming
          <> " of "
          <> showText summary.compilationTotalModules
          <> " modules have timing samples. Pass additional --timings paths for more accurate bottleneck ranking; generate missing samples with "
          <> timingFlagsInstruction
          <> "."

timingFlagsInstruction :: Text
timingFlagsInstruction =
  "--ghc-options=\"-fforce-recomp -ddump-to-file -ddump-timings -dumpdir .lore-timings\""

renderSummary :: Core.HomeModuleCompilationSummary -> LoreDoc
renderSummary summary =
  bulletList
    ( [ paragraph ("Home modules: " <> showText summary.compilationTotalModules),
        paragraph ("Home-module imports: " <> showText summary.compilationTotalImports),
        paragraph ("Timing coverage: " <> timingCoverageText summary),
        paragraph ("Jobs model: -j" <> showText summary.compilationJobs),
        paragraph ("Total work: " <> workText summary summary.compilationTotalWorkSeconds),
        paragraph ("Critical path lower bound: " <> workText summary summary.compilationCriticalPathSeconds),
        paragraph ("Worker-capacity lower bound at -j" <> showText summary.compilationJobs <> ": " <> workText summary summary.compilationIdealParallelSeconds),
        paragraph ("Estimated lower bound at -j" <> showText summary.compilationJobs <> ": " <> workText summary (max summary.compilationCriticalPathSeconds summary.compilationIdealParallelSeconds)),
        paragraph ("Best possible average worker utilization at -j" <> showText summary.compilationJobs <> ": " <> percentText summary.compilationParallelismEfficiency)
      ]
        <> [ paragraph "This is a serial -j1 model, so the worker-capacity lower bound equals total work and utilization is trivially 100%. Use --jobs N to inspect parallel bottlenecks."
             | summary.compilationJobs == 1
           ]
    )

workText :: Core.HomeModuleCompilationSummary -> Double -> Text
workText summary value
  | summary.compilationModulesWithTiming == summary.compilationTotalModules = secondsText value
  | summary.compilationModulesWithTiming == 0 = unitText value
  | otherwise = unitText value <> " (seconds for timed modules; 1 unit per untimed module)"

unitText :: Double -> Text
unitText value =
  T.pack (showFFloat (Just 2) value " module-units")

timingCoverageText :: Core.HomeModuleCompilationSummary -> Text
timingCoverageText summary =
  showText summary.compilationModulesWithTiming
    <> " modules from "
    <> showText summary.compilationTimingFileCount
    <> " timing files"

renderBottleneck :: Core.HomeModuleCompilationNode -> LoreDoc
renderBottleneck node =
  paragraph
    ( node.compilationModuleName
        <> " ["
        <> node.compilationComponentName
        <> "]"
        <> maybe "" (\path -> " (" <> T.pack path <> ")") node.compilationSourcePath
        <> ": blocks "
        <> showText node.compilationTransitiveDependentCount
        <> " transitive dependents, "
        <> showText node.compilationDirectDependentCount
        <> " direct dependents, layer "
        <> showText node.compilationBuildLayer
        <> metricsText node.compilationCompileMetrics
    )

renderComponentGraph :: Int -> Core.HomeModuleCompilationComponentAnalysis -> LoreDoc
renderComponentGraph limit component =
  paragraph
    ( component.componentCompilationName
    )
    <> renderSummary summary
    <> renderTimingCoverageNotice summary
    <> paragraph ("Critical path: " <> renderPath component.componentCompilationCriticalPath)
    <> numberedListFrom 1 (map renderBottleneck (take limit component.componentCompilationNodes))
  where
    summary = component.componentCompilationSummary

renderPath :: [Text] -> Text
renderPath path
  | null path = "No modules found."
  | otherwise = T.intercalate " -> " (take 12 path) <> suffix
  where
    suffix
      | length path > 12 = " -> ..."
      | otherwise = ""

metricsText :: Maybe Core.ModuleCompileMetrics -> Text
metricsText Nothing =
  "; no timing sample"
metricsText (Just metrics) =
  "; compile sample "
    <> secondsText metrics.moduleCompileTimeSeconds
    <> ", alloc "
    <> bytesText metrics.moduleCompileAllocBytes

secondsText :: Double -> Text
secondsText value =
  T.pack (showFFloat (Just 2) value "s")

percentText :: Double -> Text
percentText value =
  T.pack (showFFloat (Just 1) (value * 100) "%")

bytesText :: Integer -> Text
bytesText bytes
  | bytes >= 1024 * 1024 * 1024 = scaledText (1024 * 1024 * 1024) "GiB"
  | bytes >= 1024 * 1024 = scaledText (1024 * 1024) "MiB"
  | bytes >= 1024 = scaledText 1024 "KiB"
  | otherwise = showText bytes <> "B"
  where
    scaledText divisor suffix =
      T.pack (showFFloat (Just 2) (fromIntegral bytes / fromIntegral (divisor :: Integer) :: Double) (T.unpack suffix))

showText :: (Show a) => a -> Text
showText =
  T.pack . show
