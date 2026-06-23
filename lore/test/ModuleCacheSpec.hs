module ModuleCacheSpec
  ( spec,
  )
where

import Data.IORef (IORef, mkWeakIORef, newIORef)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified GHC.MVar as MVar
import qualified GHC.Plugins as GHC
import Lore.Internal.Definition.Cache.ModuleCache (retainModuleCache, storeModuleCache)
import Lore.Internal.Definition.Cache.Types (ModuleCache (..))
import System.Mem (performMajorGC)
import System.Mem.Weak (Weak, deRefWeak)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

spec :: Spec
spec =
  describe "module cache" do
    it "forces retained cache spines so dropped entries can be collected immediately" do
      (cacheVar, droppedWeak) <- cacheWithDroppedWeakReference

      performMajorGC

      droppedValue <- deRefWeak droppedWeak
      ModuleCache retainedCache <- MVar.readMVar cacheVar

      Map.member keptModule retainedCache `shouldBe` True
      case droppedValue of
        Nothing -> pure ()
        Just _ -> expectationFailure "dropped cache entry was still retained after a major GC"

    it "forces stored cache spines so replaced entries can be collected immediately" do
      (cacheVar, replacedWeak) <- cacheWithReplacedWeakReference

      performMajorGC

      replacedValue <- deRefWeak replacedWeak
      ModuleCache retainedCache <- MVar.readMVar cacheVar

      Map.member keptModule retainedCache `shouldBe` True
      case replacedValue of
        Nothing -> pure ()
        Just _ -> expectationFailure "replaced cache entry was still retained after a major GC"

cacheWithDroppedWeakReference :: IO (MVar.MVar (ModuleCache (IORef Int)), Weak (IORef Int))
cacheWithDroppedWeakReference = do
  keptValue <- newIORef 1
  droppedValue <- newIORef 2
  cacheVar <-
    MVar.newMVar $
      ModuleCache $
        Map.fromList
          [ (keptModule, keptValue),
            (droppedModule, droppedValue)
          ]
  retainModuleCache (Set.singleton keptModule) cacheVar
  droppedWeak <- mkWeakIORef droppedValue (pure ())
  pure (cacheVar, droppedWeak)

cacheWithReplacedWeakReference :: IO (MVar.MVar (ModuleCache (IORef Int)), Weak (IORef Int))
cacheWithReplacedWeakReference = do
  replacedValue <- newIORef 1
  currentValue <- newIORef 2
  cacheVar <-
    MVar.newMVar $
      ModuleCache $
        Map.singleton keptModule replacedValue
  storeModuleCache keptModule currentValue cacheVar
  replacedWeak <- mkWeakIORef replacedValue (pure ())
  pure (cacheVar, replacedWeak)

keptModule :: GHC.Module
keptModule =
  testModule "Cache.Kept"

droppedModule :: GHC.Module
droppedModule =
  testModule "Cache.Dropped"

testModule :: String -> GHC.Module
testModule moduleName =
  GHC.mkModule GHC.mainUnit (GHC.mkModuleName moduleName)
