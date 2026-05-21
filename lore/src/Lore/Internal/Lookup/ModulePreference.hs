module Lore.Internal.Lookup.ModulePreference
  ( ModulePreferenceContext (..),
    PreferredModuleChoice (..),
    choosePreferredModuleForRoot,
  )
where

import Data.List (sortOn)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC
import qualified GHC.Plugins as Plugins
import Lore.Internal.Lookup.Types (Symbol (..), SymbolVisibility (..))

data ModulePreferenceContext = ModulePreferenceContext
  { modulePreferenceHomeModules :: !(Set.Set GHC.ModuleName),
    modulePreferenceCustomPrelude :: !(Maybe GHC.ModuleName)
  }

data PreferredModuleChoice
  = PreferredModule !GHC.ModuleName
  | NoUsableModule

choosePreferredModuleForRoot ::
  ModulePreferenceContext ->
  Plugins.Name ->
  NE.NonEmpty Symbol ->
  PreferredModuleChoice
choosePreferredModuleForRoot context rootName rootSymbols =
  case sortedCandidates of
    moduleName : _ ->
      PreferredModule moduleName
    [] ->
      NoUsableModule
  where
    sortedCandidates =
      sortOn (modulePreferenceKey context rootModule exportedModulesSet) candidateModules

    candidateModules =
      dedupeBy renderModuleName (rootModuleList <> exportedModules)

    rootModuleList =
      maybe [] pure rootModule

    rootModule =
      GHC.moduleName <$> Plugins.nameModule_maybe rootName

    exportedModules =
      dedupeBy
        renderModuleName
        ( concatMap
            ( \symbol ->
                case symbol.visibility of
                  Symbol'ExportedFrom modules ->
                    map GHC.moduleName (Set.toList modules)
                  Symbol'Unexported ->
                    maybe [] pure (GHC.moduleName <$> Plugins.nameModule_maybe symbol.name)
            )
            (NE.toList rootSymbols)
        )

    exportedModulesSet =
      Set.fromList exportedModules

modulePreferenceKey ::
  ModulePreferenceContext ->
  Maybe GHC.ModuleName ->
  Set.Set GHC.ModuleName ->
  GHC.ModuleName ->
  (Int, Int, Text)
modulePreferenceKey context maybeRootModule exportedModulesSet moduleName =
  ( penalty,
    moduleDepth,
    renderedName
  )
  where
    renderedName =
      T.pack (GHC.moduleNameString moduleName)

    moduleDepth =
      length (T.splitOn "." renderedName)

    isHomeModule =
      moduleName `Set.member` context.modulePreferenceHomeModules

    preludeModule =
      maybe (GHC.mkModuleName "Prelude") id context.modulePreferenceCustomPrelude

    isPreludeModule =
      moduleName == preludeModule && moduleName `Set.member` exportedModulesSet

    isDefiningModule =
      case maybeRootModule of
        Just rootModule ->
          moduleName == rootModule && moduleName `Set.member` exportedModulesSet
        Nothing ->
          False

    internalPenalty =
      if T.isInfixOf ".Internal" renderedName then 100 else 0

    ghcPenalty =
      if T.isPrefixOf "GHC." renderedName then 50 else 0

    homeBonus =
      if isHomeModule then -400 else 0

    preludeBonus =
      if isPreludeModule then -300 else 0

    definingBonus =
      if isDefiningModule then -200 else 0

    exportedBonus =
      if moduleName `Set.member` exportedModulesSet then -100 else 0

    penalty =
      internalPenalty + ghcPenalty + homeBonus + preludeBonus + definingBonus + exportedBonus

dedupeBy :: (Ord key) => (value -> key) -> [value] -> [value]
dedupeBy renderKey =
  reverse . snd . foldl dedupeOne (Set.empty, [])
  where
    dedupeOne (seenKeys, dedupedValues) value =
      let key = renderKey value
       in if key `Set.member` seenKeys
            then (seenKeys, dedupedValues)
            else (Set.insert key seenKeys, value : dedupedValues)

renderModuleName :: GHC.ModuleName -> String
renderModuleName =
  GHC.moduleNameString
