module Lore.Tools.Cli.Render
  ( CliRenderResult (..),
    FileWriteMode (..),
    renderOutputPayload,
    renderOutput,
    writeOutputToFile,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Lore.Tools.Cli.Internal.Parser (OutputFormat (..))
import Lore.Tools.Render.Doc (LoreDoc)
import Lore.Tools.Render.Markdown (renderLoreDocMarkdown)

data CliRenderResult = CliRenderResult
  { cliRenderCommand :: Text,
    cliRenderMarkdown :: Text
  }

data FileWriteMode
  = WriteTruncate
  | WriteAppend

renderOutput :: (MonadIO m) => OutputFormat -> Text -> LoreDoc -> m ()
renderOutput format command loreDoc = do
  let rendered =
        CliRenderResult
          { cliRenderCommand = command,
            cliRenderMarkdown = renderLoreDocMarkdown loreDoc
          }
      payload = renderPayload format rendered
  liftIO (TIO.putStrLn payload)

renderOutputPayload :: OutputFormat -> Text -> LoreDoc -> Text
renderOutputPayload format command loreDoc =
  renderPayload
    format
    CliRenderResult
      { cliRenderCommand = command,
        cliRenderMarkdown = renderLoreDocMarkdown loreDoc
      }

writeOutputToFile :: (MonadIO m) => FileWriteMode -> FilePath -> OutputFormat -> Text -> LoreDoc -> m ()
writeOutputToFile fileWriteMode path format command loreDoc = do
  let payload = ensureTrailingNewline (renderOutputPayload format command loreDoc)
  liftIO $
    case fileWriteMode of
      WriteTruncate ->
        TIO.writeFile path payload
      WriteAppend ->
        TIO.appendFile path payload

renderPayload :: OutputFormat -> CliRenderResult -> Text
renderPayload format rendered =
  case format of
    FormatMarkdown ->
      rendered.cliRenderMarkdown
    FormatJson ->
      TE.decodeUtf8
        ( LBS.toStrict
            ( J.encode
                ( J.object
                    [ "command" J..= rendered.cliRenderCommand,
                      "markdown" J..= rendered.cliRenderMarkdown
                    ]
                )
            )
        )

ensureTrailingNewline :: Text -> Text
ensureTrailingNewline outputText
  | "\n" `T.isSuffixOf` outputText = outputText
  | otherwise = outputText <> "\n"
