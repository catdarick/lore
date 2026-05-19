module Lore.Interpreter
  ( interpreterContextIsReady,
    loadInterpreterContext,
    executeStatement,
    getTypeOfExpression,
  )
where

import Control.Monad (void)
import Data.Text (Text)
import qualified GHC
import Lore.Diagnostics (Diagnostic)
import Lore.Internal.HomeModules (defaultLoadHomeModulesOptions, loadHomeModules)
import qualified Lore.Internal.Interpreter as Internal
import Lore.Monad (MonadLore)

interpreterContextIsReady :: (MonadLore m) => m Bool
interpreterContextIsReady =
  Internal.interpreterContextIsReady

loadInterpreterContext :: (MonadLore m) => m ()
loadInterpreterContext = do
  contextReady <- interpreterContextIsReady
  if contextReady
    then pure ()
    else void (loadHomeModules defaultLoadHomeModulesOptions)

executeStatement :: (MonadLore m) => Text -> m (Either [Diagnostic] String)
executeStatement source = do
  loadInterpreterContext
  Internal.executeStatementRaw source

getTypeOfExpression :: (MonadLore m) => Text -> m GHC.Type
getTypeOfExpression source = do
  loadInterpreterContext
  Internal.getTypeOfExpressionRaw source
