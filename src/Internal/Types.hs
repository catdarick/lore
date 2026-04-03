module Internal.Types where

newtype ModuleName = ModuleName {unModuleName :: String}
  deriving (Eq, Ord, Show)
