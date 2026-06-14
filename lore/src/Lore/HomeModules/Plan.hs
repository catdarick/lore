module Lore.HomeModules.Plan
  ( HomeModulesLoadInputs (..),
    HomeModulesLoadConfig (..),
    HomeModulesSelection (..),
    HomeModulesLoadPlan (..),
    HomeModulesComponentPlan (..),
    HomeModuleKey (..),
    ComponentSpecificOptions (..),
    prepareHomeModulesLoadPlan,
    computeHomeModuleSourceDirs,
    buildHomeModulesSelection,
    homeModulesSelectionTotal,
    prepareHomeModulesComponentPlan,
    mkGhcModuleTarget,
    mkGhcFileTarget,
    commonComponentLanguage,
    commonSetIntersection,
  )
where

import Lore.Internal.HomeModules.Plan
