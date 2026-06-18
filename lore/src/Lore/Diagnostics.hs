{-# LANGUAGE CPP #-}

module Lore.Diagnostics
  ( Diagnostic (..),
    DiagnosticClass (..),
    DiagnosticSpan (..),
    Span (..),
    DiagnosticCodeInfo (..),
    driverMessagesToDiagnostics,
    ghcMessagesToDiagnostics,
    withDiagnosticsCapturing,
  )
where

import qualified Control.Concurrent.MVar as MVar
import Control.Monad.Catch (finally)
import Control.Monad.IO.Class (MonadIO (..))
#if MIN_VERSION_ghc(9,8,0)
import qualified Data.List.NonEmpty as NonEmpty
#endif
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC
import qualified GHC.Data.Bag as GHC
import qualified GHC.Driver.Errors.Types as GHC.Driver
import qualified GHC.Driver.Flags as DriverFlags
import GHC.Types.Error (MessageClass (..))
import qualified GHC.Types.Error as GHC
import qualified GHC.Utils.Error as GHC
import GHC.Utils.Logger (LogAction)
import qualified GHC.Utils.Outputable as GHC
import Lore.Internal.SourceSpan (srcSpanToSpan)
import Lore.Internal.SourceSpan.Types (Span (..))

data Diagnostic = Diagnostic
  { diagnosticClass :: DiagnosticClass,
    diagnosticSeverity :: Maybe GHC.Severity,
    diagnosticReason :: Maybe Text,
    diagnosticWarningFlag :: Maybe DriverFlags.WarningFlag,
    diagnosticCode :: Maybe DiagnosticCodeInfo,
    diagnosticSpan :: DiagnosticSpan,
    diagnosticMessage :: Text,
    diagnosticHints :: [Text]
  }
  deriving (Eq, Show)

data DiagnosticClass
  = DiagOutput
  | DiagFatal
  | DiagInteractive
  | DiagDump
  | DiagInfo
  | DiagCompiler
  deriving (Eq, Show)

data DiagnosticSpan
  = RealDiagnosticSpan Span
  | UnhelpfulDiagnosticSpan Text
  deriving (Eq, Show)

data DiagnosticCodeInfo = DiagnosticCodeInfo
  { diagnosticCodeNamespace :: Text,
    diagnosticCodeNumber :: Integer
  }
  deriving (Eq, Show)

ghcMessageToDiagnostic :: MessageClass -> GHC.SrcSpan -> GHC.SDoc -> Diagnostic
ghcMessageToDiagnostic msgClass srcSpan sdoc =
  Diagnostic
    { diagnosticClass = toDiagnosticClass msgClass,
      diagnosticSeverity = toSeverity msgClass,
      diagnosticReason = toReasonText msgClass,
      diagnosticWarningFlag = toWarningFlag msgClass,
      diagnosticCode = toCodeInfo msgClass,
      diagnosticSpan = toDiagnosticSpan srcSpan,
      diagnosticMessage = T.pack (GHC.showSDocUnsafe sdoc),
      diagnosticHints = []
    }

driverMessagesToDiagnostics :: GHC.Driver.DriverMessages -> [Diagnostic]
driverMessagesToDiagnostics =
  map driverMessageEnvelopeToDiagnostic
    . GHC.bagToList
    . GHC.getMessages

ghcMessagesToDiagnostics :: GHC.Driver.ErrorMessages -> [Diagnostic]
ghcMessagesToDiagnostics =
  map ghcMessageEnvelopeToDiagnostic
    . GHC.bagToList
    . GHC.getMessages

driverMessageEnvelopeToDiagnostic :: GHC.MsgEnvelope GHC.Driver.DriverMessage -> Diagnostic
driverMessageEnvelopeToDiagnostic env =
  let msg = GHC.errMsgDiagnostic env
   in Diagnostic
        { diagnosticClass = DiagCompiler,
          diagnosticSeverity = Just (GHC.errMsgSeverity env),
          diagnosticReason = Just (reasonToText (GHC.diagnosticReason msg)),
          diagnosticWarningFlag = reasonToWarningFlag (GHC.diagnosticReason msg),
          diagnosticCode = fmap fromDiagnosticCode (GHC.diagnosticCode msg),
          diagnosticSpan = toDiagnosticSpan (GHC.errMsgSpan env),
          diagnosticMessage = renderSDoc (GHC.pprLocMsgEnvelope (GHC.defaultDiagnosticOpts @GHC.Driver.DriverMessage) env),
          diagnosticHints = map renderHint (GHC.diagnosticHints msg)
        }

ghcMessageEnvelopeToDiagnostic :: GHC.MsgEnvelope GHC.Driver.GhcMessage -> Diagnostic
ghcMessageEnvelopeToDiagnostic env =
  let msg = GHC.errMsgDiagnostic env
   in Diagnostic
        { diagnosticClass = DiagCompiler,
          diagnosticSeverity = Just (GHC.errMsgSeverity env),
          diagnosticReason = Just (reasonToText (GHC.diagnosticReason msg)),
          diagnosticWarningFlag = reasonToWarningFlag (GHC.diagnosticReason msg),
          diagnosticCode = fmap fromDiagnosticCode (GHC.diagnosticCode msg),
          diagnosticSpan = toDiagnosticSpan (GHC.errMsgSpan env),
          diagnosticMessage = renderSDoc (GHC.pprLocMsgEnvelope (GHC.defaultDiagnosticOpts @GHC.Driver.GhcMessage) env),
          diagnosticHints = map renderHint (GHC.diagnosticHints msg)
        }

toDiagnosticClass :: MessageClass -> DiagnosticClass
toDiagnosticClass = \case
  MCOutput -> DiagOutput
  MCFatal -> DiagFatal
  MCInteractive -> DiagInteractive
  MCDump -> DiagDump
  MCInfo -> DiagInfo
  MCDiagnostic {} -> DiagCompiler

toSeverity :: MessageClass -> Maybe GHC.Severity
toSeverity = \case
  MCDiagnostic sev _ _ -> Just sev
  _ -> Nothing

toReasonText :: MessageClass -> Maybe Text
toReasonText = \case
  MCDiagnostic _ reason _ -> Just (messageClassReasonToText reason)
  _ -> Nothing

toWarningFlag :: MessageClass -> Maybe DriverFlags.WarningFlag
toWarningFlag = \case
  MCDiagnostic _ reason _ -> messageClassReasonToWarningFlag reason
  _ -> Nothing

toCodeInfo :: MessageClass -> Maybe DiagnosticCodeInfo
toCodeInfo = \case
  MCDiagnostic _ _ mCode -> fmap fromDiagnosticCode mCode
  _ -> Nothing

#if MIN_VERSION_ghc(9,8,0)
messageClassReasonToText :: GHC.ResolvedDiagnosticReason -> Text
messageClassReasonToText =
  reasonToText . GHC.resolvedDiagnosticReason

messageClassReasonToWarningFlag :: GHC.ResolvedDiagnosticReason -> Maybe DriverFlags.WarningFlag
messageClassReasonToWarningFlag =
  reasonToWarningFlag . GHC.resolvedDiagnosticReason
#else
messageClassReasonToText :: GHC.DiagnosticReason -> Text
messageClassReasonToText = reasonToText

messageClassReasonToWarningFlag :: GHC.DiagnosticReason -> Maybe DriverFlags.WarningFlag
messageClassReasonToWarningFlag = reasonToWarningFlag
#endif

reasonToText :: GHC.DiagnosticReason -> Text
reasonToText = T.pack . show

{- ORMOLU_DISABLE -}
reasonToWarningFlag :: GHC.DiagnosticReason -> Maybe DriverFlags.WarningFlag
reasonToWarningFlag = \case
#if MIN_VERSION_ghc(9,8,0)
  GHC.WarningWithFlags flags -> Just (NonEmpty.head flags)
#else
  GHC.WarningWithFlag flag -> Just flag
#endif
  _ -> Nothing
{- ORMOLU_ENABLE -}

fromDiagnosticCode :: GHC.DiagnosticCode -> DiagnosticCodeInfo
fromDiagnosticCode
  GHC.DiagnosticCode
    { diagnosticCodeNameSpace,
      diagnosticCodeNumber
    } =
    DiagnosticCodeInfo
      { diagnosticCodeNamespace = T.pack diagnosticCodeNameSpace,
        diagnosticCodeNumber = toInteger diagnosticCodeNumber
      }

toDiagnosticSpan :: GHC.SrcSpan -> DiagnosticSpan
toDiagnosticSpan srcSpan =
  case srcSpanToSpan srcSpan of
    Just span' ->
      RealDiagnosticSpan span'
    Nothing ->
      UnhelpfulDiagnosticSpan $
        case srcSpan of
          GHC.UnhelpfulSpan unhelpful ->
            T.pack (show unhelpful)
          _ ->
            T.pack (show srcSpan)

renderSDoc :: GHC.SDoc -> Text
renderSDoc =
  T.pack . GHC.showSDocUnsafe

renderHint :: GHC.GhcHint -> Text
renderHint =
  T.pack
    . GHC.showSDocUnsafe
    . GHC.ppr

withDiagnosticsCapturing :: (GHC.GhcMonad m) => m b -> m ([Diagnostic], b)
withDiagnosticsCapturing action = do
  diagsVar <- liftIO $ MVar.newMVar []

  let customLogAction :: LogAction -> LogAction
      customLogAction _oldLogAction _logFlags msgClass srcSpan sdoc = do
        let diagnostic = ghcMessageToDiagnostic msgClass srcSpan sdoc
        liftIO $
          MVar.modifyMVar_ diagsVar \diags ->
            pure (diagnostic : diags)

  GHC.pushLogHookM customLogAction
  r <- action `finally` GHC.popLogHookM

  capturedDiags <- reverse <$> liftIO (MVar.readMVar diagsVar)

  pure (capturedDiags, r)
