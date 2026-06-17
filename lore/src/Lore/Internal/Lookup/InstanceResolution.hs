{-# LANGUAGE CPP #-}

module Lore.Internal.Lookup.InstanceResolution
  ( ChosenInstanceError (..),
    ChosenInstanceResolution (..),
    ChosenInstanceContextStatus (..),
    resolveChosenClassInstanceFromTypeText,
    resolveChosenClassInstanceFromResolvedTypeQuery,
    buildInstanceEnvs,
  )
where

-- Typechecks a class application, asks GHC's instance environment which class
-- instance is selected, and checks the selected instance context with GHC's
-- interactive constraint solver.

import Control.Monad (forM)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC
import qualified GHC.Core.InstEnv as InstEnv
import qualified GHC.Core.Predicate as Predicate
import qualified GHC.Core.TyCo.Subst as TyCoSubst
import qualified GHC.Driver.Env as DriverEnv
import qualified GHC.Plugins as Plugins
import qualified GHC.Tc.Errors.Types as TcErrors
import qualified GHC.Tc.Module as TcModule
import qualified GHC.Tc.Solver as TcSolver
import qualified GHC.Tc.Solver.InertSet as InertSet
#if MIN_VERSION_ghc(9,14,0)
import qualified GHC.Tc.Utils.TcType as TcType
#endif
#if MIN_VERSION_ghc(9,8,0)
import qualified GHC.Tc.Zonk.Env as Zonk
#else
import qualified GHC.Tc.Utils.Zonk as Zonk
#endif
import qualified GHC.Types.Error as TypeError
import qualified GHC.Unit.External as External
import Lore.Internal.Lookup.Orphans (collectIndexModules)
import Lore.Internal.Lookup.TypeQuery
  ( ParsedTypeQuery (..),
    ResolvedTypeQuery (..),
    TypeQueryNameResolutionError (..),
    TypeQueryParseError (..),
    TypeQueryUnresolvedSymbols,
    parseTypeQuery,
    resolveParsedTypeQueryNames,
    withAdditionalInteractiveImports,
  )
import Lore.Monad (MonadLore)
import UnliftIO (tryAny)

data ChosenInstanceError
  = ChosenInstanceTypeParseFailed !Text
  | ChosenInstanceNameResolutionFailed !TypeQueryUnresolvedSymbols
  | ChosenInstanceUnsupportedParsedType !Text
  | ChosenInstanceGhcTypeCheckFailed !Text
  | ChosenInstanceNotAClassApplication !Text
  | ChosenInstanceLookupFailed !Text

data ChosenInstanceContextStatus
  = ChosenInstanceContextResolved
  | ChosenInstanceContextUnresolved !Text

data ChosenInstanceResolution = ChosenInstanceResolution
  { chosenInstance :: !InstEnv.ClsInst,
    chosenInstanceDfunInstTypes :: ![GHC.Type],
    chosenInstanceContextPredicates :: ![GHC.Type],
    chosenInstanceContextStatus :: !ChosenInstanceContextStatus
  }

resolveChosenClassInstanceFromTypeText ::
  (MonadLore m) =>
  Text ->
  m (Either ChosenInstanceError ChosenInstanceResolution)
resolveChosenClassInstanceFromTypeText classApplicationText =
  runExceptT do
    parsed <- ExceptT (parseTypeQuery classApplicationText >>= pure . mapParseError)
    resolved <- ExceptT (resolveParsedTypeQueryNames parsed >>= pure . mapNameResolutionError)
    ExceptT (resolveChosenClassInstanceFromResolvedTypeQuery resolved)

resolveChosenClassInstanceFromResolvedTypeQuery ::
  (MonadLore m) =>
  ResolvedTypeQuery ->
  m (Either ChosenInstanceError ChosenInstanceResolution)
resolveChosenClassInstanceFromResolvedTypeQuery resolvedTypeQuery = do
  eiResult <-
    tryAny $
      withAdditionalInteractiveImports resolvedTypeQuery.resolvedTypeQueryImports $
        runExceptT do
          (cls, argTypes) <-
            ExceptT
              ( typecheckClassApplicationType
                  resolvedTypeQuery.resolvedTypeQueryParsed.parsedTypeQueryText
                  resolvedTypeQuery.resolvedTypeQueryParsed.parsedTypeQueryAst
              )
          ExceptT (lookupChosenClassInstance cls argTypes)
  pure $
    case eiResult of
      Left err ->
        Left (ChosenInstanceGhcTypeCheckFailed (T.pack (show err)))
      Right result ->
        result

typecheckClassApplicationType ::
  (MonadLore m) =>
  Text ->
  GHC.LHsType GHC.GhcPs ->
  m (Either ChosenInstanceError (GHC.Class, [GHC.Type]))
typecheckClassApplicationType queryText parsedType = do
  hscEnv <- GHC.getSession
  (messages, maybeTypedResult) <-
    liftIO $
      TcModule.tcRnType hscEnv Zonk.NoFlexi False parsedType
  case maybeTypedResult of
    Nothing ->
      pure (Left (ChosenInstanceGhcTypeCheckFailed (renderTcMessages messages)))
    Just (resolvedType, _) ->
      pure $
        case Predicate.getClassPredTys_maybe resolvedType of
          Just (cls, argTypes) ->
            Right (cls, argTypes)
          Nothing ->
            Left (ChosenInstanceNotAClassApplication queryText)

mapParseError :: Either TypeQueryParseError a -> Either ChosenInstanceError a
mapParseError eiParsed =
  case eiParsed of
    Left parseError ->
      Left $
        case parseError of
          TypeQueryParseFailed details ->
            ChosenInstanceTypeParseFailed details
          TypeQueryUnsupportedParsedType details ->
            ChosenInstanceUnsupportedParsedType details
    Right value ->
      Right value

mapNameResolutionError :: Either TypeQueryNameResolutionError a -> Either ChosenInstanceError a
mapNameResolutionError eiResolved =
  case eiResolved of
    Left resolutionError ->
      Left $
        case resolutionError of
          TypeQueryUnresolvedSymbols unresolved ->
            ChosenInstanceNameResolutionFailed unresolved
          TypeQueryUnsupportedOccurrence details ->
            ChosenInstanceUnsupportedParsedType details
    Right value ->
      Right value

buildInstanceEnvs :: (MonadLore m) => m InstEnv.InstEnvs
buildInstanceEnvs = do
  hscEnv <- GHC.getSession
  eps <- liftIO (DriverEnv.hscEPS hscEnv)
  loadedInfo <- collectLoadedHomeInstanceInfo
  visibleOrphans <- collectIndexModules loadedInfo.loadedHomeModules
  let localInstEnv =
        InstEnv.extendInstEnvList InstEnv.emptyInstEnv loadedInfo.loadedHomeClassInstances
  pure
    InstEnv.InstEnvs
      { InstEnv.ie_global = External.eps_inst_env eps,
        InstEnv.ie_local = localInstEnv,
        InstEnv.ie_visible = Plugins.mkModuleSet visibleOrphans
      }

data LoadedHomeInstanceInfo = LoadedHomeInstanceInfo
  { loadedHomeModules :: ![GHC.Module],
    loadedHomeClassInstances :: ![InstEnv.ClsInst]
  }

collectLoadedHomeInstanceInfo :: (MonadLore m) => m LoadedHomeInstanceInfo
collectLoadedHomeInstanceInfo = do
  moduleGraph <- GHC.getModuleGraph
  let modules = [GHC.ms_mod ms | ms <- GHC.mgModSummaries moduleGraph]
  loaded <- fmap catMaybes $
    forM modules \module_ ->
      tryAny (GHC.getModuleInfo module_) >>= \case
        Right (Just info) ->
          pure (Just (module_, GHC.modInfoInstances info))
        Right Nothing ->
          pure Nothing
        Left _ ->
          pure Nothing
  pure
    LoadedHomeInstanceInfo
      { loadedHomeModules = map fst loaded,
        loadedHomeClassInstances = concatMap snd loaded
      }

resolveInstanceContextStatus ::
  (MonadLore m) =>
  InstEnv.ClsInst ->
  [GHC.Type] ->
  m (ChosenInstanceContextStatus, [GHC.Type])
resolveInstanceContextStatus clsInst dfunInstTypes =
  case instantiateInstanceContext clsInst dfunInstTypes of
    Left mismatch ->
      pure (ChosenInstanceContextUnresolved mismatch, [])
    Right contextPredicates -> do
      -- This intentionally uses GHC's interactive constraint solver rather than
      -- Lore's rough instance index. It checks whether the selected instance's
      -- instantiated context can be solved in the currently loaded session.
      eiContextCheck <- solveSelectedInstanceContext contextPredicates
      pure $
        case eiContextCheck of
          Right () ->
            (ChosenInstanceContextResolved, contextPredicates)
          Left details ->
            (ChosenInstanceContextUnresolved details, contextPredicates)

instantiateInstanceContext ::
  InstEnv.ClsInst ->
  [GHC.Type] ->
  Either Text [GHC.Type]
instantiateInstanceContext clsInst dfunInstTypes =
  if length instanceTypeVars /= length dfunInstTypes
    then
      Left
        "Unable to validate selected instance context due to instantiation mismatch."
    else
      Right (TyCoSubst.substTheta subst instanceContextPredicates)
  where
    (instanceTypeVars, instanceContextPredicates, _, _) =
      InstEnv.instanceSig clsInst

    subst =
      TyCoSubst.zipTvSubst instanceTypeVars dfunInstTypes

lookupChosenClassInstance ::
  (MonadLore m) =>
  GHC.Class ->
  [GHC.Type] ->
  m (Either ChosenInstanceError ChosenInstanceResolution)
lookupChosenClassInstance cls argTypes = do
  instEnvs <- buildInstanceEnvs
  case InstEnv.lookupUniqueInstEnv instEnvs cls argTypes of
    Right (clsInst, dfunInstTypes) -> do
      (contextStatus, contextPredicates) <- resolveInstanceContextStatus clsInst dfunInstTypes
      pure $
        Right
          ChosenInstanceResolution
            { chosenInstance = clsInst,
              chosenInstanceDfunInstTypes = dfunInstTypes,
              chosenInstanceContextPredicates = contextPredicates,
              chosenInstanceContextStatus = contextStatus
            }
    Left err ->
      pure (Left (ChosenInstanceLookupFailed (renderLookupInstanceError err)))

{- ORMOLU_DISABLE -}
solveSelectedInstanceContext ::
  (MonadLore m) =>
  [GHC.Type] ->
  m (Either Text ())
solveSelectedInstanceContext contextPredicates = do
  hscEnv <- GHC.getSession
  (messages, maybeCheckResult) <-
    liftIO $
      TcModule.runTcInteractive hscEnv $
        TcSolver.tcCheckWanteds
#if MIN_VERSION_ghc(9,14,0)
          (InertSet.emptyInertSet TcType.topTcLevel)
#else
          InertSet.emptyInert
#endif
          contextPredicates
  pure $
    case maybeCheckResult of
      Just True ->
        Right ()
      _ ->
        Left (renderUnsatisfiedContextFailure contextPredicates messages)
{- ORMOLU_ENABLE -}

renderUnsatisfiedContextFailure ::
  [GHC.Type] ->
  TypeError.Messages TcErrors.TcRnMessage ->
  Text
renderUnsatisfiedContextFailure contextPredicates messages =
  if TypeError.isEmptyMessages messages
    then
      "Missing required instance constraints for context: "
        <> T.intercalate ", " (map renderType contextPredicates)
        <> "."
    else
      T.pack (Plugins.showSDocUnsafe (Plugins.ppr messages))

renderType :: GHC.Type -> Text
renderType =
  T.pack . Plugins.showSDocUnsafe . Plugins.ppr

renderTcMessages :: (TypeError.Diagnostic e) => TypeError.Messages e -> Text
renderTcMessages messages
  | TypeError.isEmptyMessages messages =
      "Typechecking failed."
  | otherwise =
      T.pack (Plugins.showSDocUnsafe (Plugins.ppr messages))

#if MIN_VERSION_ghc(9,8,0)
renderLookupInstanceError :: InstEnv.LookupInstanceErrReason -> Text
renderLookupInstanceError = \case
  InstEnv.LookupInstErrNotExact ->
    "Matching instance is not exact."
  InstEnv.LookupInstErrFlexiVar ->
    "Instance lookup contains flexible type variables."
  InstEnv.LookupInstErrNotFound ->
    "No matching instance found."
#else
renderLookupInstanceError :: Plugins.SDoc -> Text
renderLookupInstanceError =
  T.pack . Plugins.showSDocUnsafe
#endif
