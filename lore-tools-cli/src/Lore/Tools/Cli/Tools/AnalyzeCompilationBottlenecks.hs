module Lore.Tools.Cli.Tools.AnalyzeCompilationBottlenecks
  ( analyzeCompilationBottlenecksCliTool,
  )
where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import GHC.Conc (getNumCapabilities)
import Lore.Tools.Cli.Internal.Annotated
  ( CliArgs,
    CompletionProvider (FileCompletion),
    manyOptionText,
    optionalOptionWithReader,
  )
import Lore.Tools.Cli.Internal.Tool
  ( CliTool (..),
    LoreCliM,
    successfulCliToolRun,
  )
import Lore.Tools.Cli.Tools.Common
  ( limitArg,
    noCompletion,
    resultLimitToInt,
  )
import qualified Lore.Tools.AnalyzeCompilationBottlenecks as AnalyzeCompilationBottlenecks
import Lore.Tools.Render.Doc (LoreDoc)
import Lore.Tools.Result (ResultLimit)
import Options.Applicative (ReadM, eitherReader)

data AnalyzeCompilationBottlenecksArgs = AnalyzeCompilationBottlenecksArgs
  { analyzeCompilationJobsArg :: Maybe Int,
    analyzeCompilationTimingsArg :: [FilePath],
    analyzeCompilationLimitArg :: ResultLimit
  }

analyzeCompilationBottlenecksCliTool :: CliTool LoreCliM AnalyzeCompilationBottlenecksArgs
analyzeCompilationBottlenecksCliTool =
  CliTool
    { cliToolName = "analyze-compilation-bottlenecks",
      cliToolAliases = ["module-bottlenecks", "compile-bottlenecks"],
      cliToolSummary = "Analyze home-module compilation bottlenecks",
      cliToolDescription = "Build the home-module dependency graph, estimate parallel compilation bottlenecks, and optionally combine it with GHC .dump-timings files.",
      cliToolExamples =
        [ "lore-cli analyze-compilation-bottlenecks --jobs 8 --limit 20",
          "lore-cli module-bottlenecks --timings .stack-work --jobs 12"
        ],
      cliToolArgs = analyzeCompilationBottlenecksArgs,
      cliToolRun = successfulCliToolRun runAnalyzeCompilationBottlenecks
    }

analyzeCompilationBottlenecksArgs :: CliArgs LoreCliM AnalyzeCompilationBottlenecksArgs
analyzeCompilationBottlenecksArgs =
  AnalyzeCompilationBottlenecksArgs
    <$> optionalOptionWithReader positiveIntReader "jobs" (Just 'j') "N" "Parallel jobs to model; defaults to the RTS capability count" noCompletion
    <*> (concatMap splitTimingPaths <$> manyOptionText "timings" Nothing "PATH[,PATH...]" "GHC .dump-timings file or directory; repeat the option or use comma-separated paths" FileCompletion)
    <*> limitArg

runAnalyzeCompilationBottlenecks :: AnalyzeCompilationBottlenecksArgs -> LoreCliM LoreDoc
runAnalyzeCompilationBottlenecks args = do
  defaultJobs <- liftIO getNumCapabilities
  let jobs = maybe defaultJobs id args.analyzeCompilationJobsArg
  result <-
    AnalyzeCompilationBottlenecks.analyzeCompilationBottlenecks
      AnalyzeCompilationBottlenecks.AnalyzeCompilationBottlenecksOptions
        { AnalyzeCompilationBottlenecks.analyzeCompilationBottlenecksJobs = jobs,
          AnalyzeCompilationBottlenecks.analyzeCompilationBottlenecksTimingPaths = args.analyzeCompilationTimingsArg
        }
  pure (AnalyzeCompilationBottlenecks.renderAnalyzeCompilationBottlenecksResult (resultLimitToInt args.analyzeCompilationLimitArg) result)

positiveIntReader :: ReadM Int
positiveIntReader =
  eitherReader \raw ->
    case reads raw of
      [(value, "")] | value > 0 -> Right value
      _ -> Left "expected positive integer"

splitTimingPaths :: T.Text -> [FilePath]
splitTimingPaths raw =
  [ T.unpack path
  | path <- map T.strip (T.splitOn "," raw),
    not (T.null path)
  ]
