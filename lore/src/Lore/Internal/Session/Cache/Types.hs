module Lore.Internal.Session.Cache.Types
  ( InterpreterContextCache (..),
    LastLoadTargetsResultCache (..),
  )
where

import qualified GHC
import Lore.Internal.Targets.Result (LoadTargetsResult)

newtype InterpreterContextCache = InterpreterContextCache
  { cachedInterpreterModuleNames :: Maybe [GHC.ModuleName]
  }

newtype LastLoadTargetsResultCache = LastLoadTargetsResultCache
  { cachedLastLoadTargetsResult :: Maybe LoadTargetsResult
  }
