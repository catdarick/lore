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

import Control.Monad.Catch (finally)
import Control.Monad.IO.Class (MonadIO (..))
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import qualified GHC
import qualified GHC.Data.Bag as GHC
import qualified GHC.Data.FastString as GHC
import qualified GHC.Driver.Errors.Types as GHC.Driver
import qualified GHC.Natural as GHC
import GHC.Types.Error (MessageClass (..))
import qualified GHC.Types.Error as GHC
import GHC.Utils.Logger (LogAction)
import qualified GHC.Utils.Outputable as GHC

data Diagnostic = Diagnostic
  { diagnosticClass :: DiagnosticClass,
    diagnosticSeverity :: Maybe GHC.Severity,
    diagnosticReason :: Maybe Text,
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

data Span = Span
  { spanFile :: FilePath,
    spanStartLine :: Int,
    spanStartCol :: Int,
    spanEndLine :: Int,
    spanEndCol :: Int
  }
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
          diagnosticCode = fmap fromDiagnosticCode (GHC.diagnosticCode msg),
          diagnosticSpan = toDiagnosticSpan (GHC.errMsgSpan env),
          diagnosticMessage = renderDecorated (GHC.diagnosticMessage (GHC.defaultDiagnosticOpts @GHC.Driver.DriverMessage) msg),
          diagnosticHints = map renderHint (GHC.diagnosticHints msg)
        }

ghcMessageEnvelopeToDiagnostic :: GHC.MsgEnvelope GHC.Driver.GhcMessage -> Diagnostic
ghcMessageEnvelopeToDiagnostic env =
  let msg = GHC.errMsgDiagnostic env
   in Diagnostic
        { diagnosticClass = DiagCompiler,
          diagnosticSeverity = Just (GHC.errMsgSeverity env),
          diagnosticReason = Just (reasonToText (GHC.diagnosticReason msg)),
          diagnosticCode = fmap fromDiagnosticCode (GHC.diagnosticCode msg),
          diagnosticSpan = toDiagnosticSpan (GHC.errMsgSpan env),
          diagnosticMessage = renderDecorated (GHC.diagnosticMessage (GHC.defaultDiagnosticOpts @GHC.Driver.GhcMessage) msg),
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
  MCDiagnostic _ reason _ -> Just (reasonToText reason)
  _ -> Nothing

toCodeInfo :: MessageClass -> Maybe DiagnosticCodeInfo
toCodeInfo = \case
  MCDiagnostic _ _ mCode -> fmap fromDiagnosticCode mCode
  _ -> Nothing

reasonToText :: GHC.DiagnosticReason -> Text
reasonToText = T.pack . show

fromDiagnosticCode :: GHC.DiagnosticCode -> DiagnosticCodeInfo
fromDiagnosticCode
  GHC.DiagnosticCode
    { diagnosticCodeNameSpace,
      diagnosticCodeNumber
    } =
    DiagnosticCodeInfo
      { diagnosticCodeNamespace = T.pack diagnosticCodeNameSpace,
        diagnosticCodeNumber = naturalToInteger diagnosticCodeNumber
      }

naturalToInteger :: GHC.Natural -> Integer
naturalToInteger = toInteger

toDiagnosticSpan :: GHC.SrcSpan -> DiagnosticSpan
toDiagnosticSpan = \case
  GHC.RealSrcSpan rss _ ->
    RealDiagnosticSpan
      Span
        { spanFile = GHC.unpackFS (GHC.srcSpanFile rss),
          spanStartLine = GHC.srcSpanStartLine rss,
          spanStartCol = GHC.srcSpanStartCol rss,
          spanEndLine = GHC.srcSpanEndLine rss,
          spanEndCol = GHC.srcSpanEndCol rss
        }
  GHC.UnhelpfulSpan u ->
    UnhelpfulDiagnosticSpan (T.pack (show u))

renderDecorated :: GHC.DecoratedSDoc -> Text
renderDecorated =
  T.pack
    . GHC.showSDocUnsafe
    . GHC.vcat
    . GHC.unDecorated

renderHint :: GHC.GhcHint -> Text
renderHint =
  T.pack
    . GHC.showSDocUnsafe
    . GHC.ppr

withDiagnosticsCapturing :: (GHC.GhcMonad m) => m b -> m ([Diagnostic], b)
withDiagnosticsCapturing action = do
  diagsRef <- liftIO $ newIORef []

  let customLogAction :: LogAction -> LogAction
      customLogAction _oldLogAction _logFlags msgClass srcSpan sdoc = do
        let diagnostic = ghcMessageToDiagnostic msgClass srcSpan sdoc
        modifyIORef' diagsRef (diagnostic :)

  GHC.pushLogHookM customLogAction
  r <- action `finally` GHC.popLogHookM

  capturedDiags <- liftIO $ readIORef diagsRef

  pure (capturedDiags, r)
