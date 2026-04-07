module Lore.Interpreter
  ( loadInterpreterContext,
    interpretExpression,
    getTypeOfExpression,
  )
where

import Data.Text (Text)
import qualified GHC
import qualified Lore.Internal.Interpreter as Internal
import Lore.Internal.Targets (defaultLoadTargetsOptions, loadTargets)
import Lore.Monad (MonadLore)

loadInterpreterContext :: (MonadLore m) => m ()
loadInterpreterContext = do
  contextReady <- Internal.interpreterContextIsReady
  if contextReady
    then pure ()
    else loadTargets defaultLoadTargetsOptions

interpretExpression :: (MonadLore m) => Text -> m String
interpretExpression source = do
  loadInterpreterContext
  Internal.interpretExpressionRaw source

getTypeOfExpression :: (MonadLore m) => Text -> m GHC.Type
getTypeOfExpression source = do
  loadInterpreterContext
  Internal.getTypeOfExpressionRaw source
