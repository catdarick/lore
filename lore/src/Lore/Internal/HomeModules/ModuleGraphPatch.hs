module Lore.Internal.HomeModules.ModuleGraphPatch
  ( applyModuleScopedArgs,
    applySourcePragmas,
  )
where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust)
import qualified GHC
import qualified GHC.Data.StringBuffer as GHC
import qualified GHC.Driver.Config.Parser as GHC
import qualified GHC.Driver.Session as GHC
import qualified GHC.Parser.Header as GHC
import qualified GHC.Unit.Module.Graph as GHC
import Lore.Internal.HomeModules.Plan (ComponentSpecificOptions (..), HomeModuleKey (..))
import Lore.Internal.SourcePath (normalizeSourceFilePathM)
import Lore.Monad (MonadLore)

applyModuleScopedArgs ::
  (MonadLore m) =>
  Map.Map HomeModuleKey ComponentSpecificOptions ->
  GHC.ModuleGraph ->
  m GHC.ModuleGraph
applyModuleScopedArgs homeModulesWithComponentOptions modGraph = do
  patchedNodes <- mapM patchNode (GHC.mgModSummaries' modGraph)
  pure (GHC.mkModuleGraph patchedNodes)
  where
    patchNode node =
      case node of
        GHC.ModuleNode deps summary ->
          GHC.ModuleNode deps <$> patchSummary summary
        _ ->
          pure node

    patchSummary summary = do
      let summaryFile =
            fromMaybe
              (GHC.ms_hspp_file summary)
              (GHC.ml_hs_file (GHC.ms_location summary))
          moduleName = GHC.moduleName (GHC.ms_mod summary)
      normalizedSummaryFile <- normalizeSourceFilePathM summaryFile
      let maybeComponentOptions =
            Map.lookup (HomeModuleSourceFile normalizedSummaryFile) homeModulesWithComponentOptions
              <|> Map.lookup (HomeModuleName moduleName) homeModulesWithComponentOptions
      case maybeComponentOptions of
        Just componentOptions
          | isJust componentOptions.language
              || length componentOptions.ghcOptions + length componentOptions.extensions > 0 -> do
              dynFlags <- applySourcePragmas summary componentOptions summaryFile
              pure summary {GHC.ms_hspp_opts = dynFlags}
        _ ->
          pure summary

applySourcePragmas ::
  (MonadLore m) =>
  GHC.ModSummary ->
  ComponentSpecificOptions ->
  FilePath ->
  m GHC.DynFlags
applySourcePragmas summary compOptions summaryFile = do
  contents <-
    case GHC.ms_hspp_buf summary of
      Just buffer ->
        pure buffer
      Nothing ->
        liftIO (GHC.hGetStringBuffer summaryFile)
  let (_warnings, options) =
        GHC.getOptions
          (GHC.initParserOpts compOptions.baseDynFlags)
          contents
          summaryFile
  (dynFlags, _, _) <- liftIO (GHC.parseDynamicFilePragma compOptions.baseDynFlags options)
  pure dynFlags
