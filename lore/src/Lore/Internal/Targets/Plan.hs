module Lore.Internal.Targets.Plan
  ( TargetsPlan (..),
    ComponentSpecificOptions (..),
    prepareTargetsPlan,
    commonComponentLanguage,
    commonSetIntersection,
  )
where

import Control.Monad (foldM, forM)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified GHC
import Lore.Internal.Ghc.DynFlags (Extension (..), GhcOption (..), Language (..), setGhcOptionsAndExtensions)
import Lore.Internal.Package (ComponentData (..), commonSetIntersection, defaultExtensions)
import qualified Lore.Logger as Log
import Lore.Monad (MonadLore)

data TargetsPlan = TargetsPlan
  { commonLanguage :: Maybe Language,
    commonExtensions :: Set.Set Extension,
    commonGhcOptions :: Set.Set GhcOption,
    modulesWithComponentOptions :: Map.Map GHC.ModuleName ComponentSpecificOptions
  }

data ComponentSpecificOptions = ComponentSpecificOptions
  { language :: Maybe Language,
    extensions :: Set.Set Extension,
    ghcOptions :: Set.Set GhcOption,
    baseDynFlags :: GHC.DynFlags
  }

prepareTargetsPlan :: (MonadLore m) => [ComponentData] -> m TargetsPlan
prepareTargetsPlan components = do
  sessionDynFlags <- GHC.getSessionDynFlags
  let commonLanguage = commonComponentLanguage components
      commonExtensions = commonSetIntersection (map defaultExtensions components)
      commonGhcOptions = commonSetIntersection (map (.ghcOptions) components)

  modulesWithComponentOptionsByComponent <- forM components \component -> do
    componentFlags <-
      setGhcOptionsAndExtensions
        component.language
        (Set.toList component.ghcOptions)
        (Set.toList component.defaultExtensions)
        sessionDynFlags
    let componentSpecificExtensions = component.defaultExtensions Set.\\ commonExtensions
        componentSpecificGhcOptions = component.ghcOptions Set.\\ commonGhcOptions
        componentSpecificLanguage =
          if component.language == commonLanguage
            then Nothing
            else component.language
        componentSpecificOptions =
          ComponentSpecificOptions
            { language = componentSpecificLanguage,
              extensions = componentSpecificExtensions,
              ghcOptions = componentSpecificGhcOptions,
              baseDynFlags = componentFlags
            }
    pure $ Map.fromSet (const componentSpecificOptions) component.modules
  modulesWithComponentOptions <- mergeModuleComponentOptions modulesWithComponentOptionsByComponent
  pure
    TargetsPlan
      { commonLanguage = commonLanguage,
        commonExtensions = commonExtensions,
        commonGhcOptions = commonGhcOptions,
        modulesWithComponentOptions = modulesWithComponentOptions
      }

commonComponentLanguage :: [ComponentData] -> Maybe Language
commonComponentLanguage [] = Nothing
commonComponentLanguage (component : restComponents)
  | all ((== component.language) . (.language)) restComponents = component.language
  | otherwise = Nothing

mergeModuleComponentOptions ::
  (MonadLore m) =>
  [Map.Map GHC.ModuleName ComponentSpecificOptions] ->
  m (Map.Map GHC.ModuleName ComponentSpecificOptions)
mergeModuleComponentOptions =
  foldM mergeComponentMap Map.empty
  where
    mergeComponentMap merged componentMap =
      foldM mergeModuleOptions merged (Map.toList componentMap)

    mergeModuleOptions merged (moduleName, newOptions) =
      case Map.lookup moduleName merged of
        Nothing ->
          pure (Map.insert moduleName newOptions merged)
        Just existingOptions
          | componentOptionsEquivalent existingOptions newOptions ->
              pure merged
          | otherwise -> do
              Log.warn $
                "Target planning conflict for module "
                  <> GHC.moduleNameString moduleName
                  <> ": component-specific options differ across components; keeping the first mapping."
              pure merged

componentOptionsEquivalent :: ComponentSpecificOptions -> ComponentSpecificOptions -> Bool
componentOptionsEquivalent left right =
  left.language == right.language
    && left.extensions == right.extensions
    && left.ghcOptions == right.ghcOptions
