module Lore.Internal.Config.Document
  ( LoadedConfigDocument (..),
    ConfigError (..),
    loadConfigDocumentAt,
    decodeConfigSection,
    renderConfigError,
  )
where

import qualified Data.Aeson as J
import qualified Data.Aeson.Key as JK
import qualified Data.Aeson.KeyMap as JKM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Yaml as Y
import System.Directory (doesFileExist)

data LoadedConfigDocument = LoadedConfigDocument
  { configFilePath :: FilePath,
    configFileValue :: J.Value
  }
  deriving stock (Eq, Show)

data ConfigError
  = ConfigFileParseError FilePath Text
  | InvalidSessionEnvironmentVariable String String Text
  | InvalidSessionConfig FilePath Text
  deriving stock (Eq, Show)

loadConfigDocumentAt :: FilePath -> IO (Either ConfigError LoadedConfigDocument)
loadConfigDocumentAt configPath = do
  configExists <- doesFileExist configPath
  if not configExists
    then
      pure
        ( Right
            LoadedConfigDocument
              { configFilePath = configPath,
                configFileValue = J.Object mempty
              }
        )
    else do
      eiValue <- Y.decodeFileEither configPath
      pure $
        case eiValue of
          Left parseError ->
            Left (ConfigFileParseError configPath (T.pack (Y.prettyPrintParseException parseError)))
          Right value ->
            Right
              LoadedConfigDocument
                { configFilePath = configPath,
                  configFileValue = value
                }

decodeConfigSection :: (J.FromJSON a) => Text -> LoadedConfigDocument -> Either ConfigError a
decodeConfigSection sectionName document =
  case J.fromJSON (sectionDocumentValue sectionName document.configFileValue) of
    J.Success value ->
      Right value
    J.Error err ->
      Left (InvalidSessionConfig document.configFilePath (T.pack err))

renderConfigError :: ConfigError -> Text
renderConfigError = \case
  ConfigFileParseError path parseError ->
    "Failed to parse "
      <> quote (T.pack path)
      <> ": "
      <> parseError
  InvalidSessionEnvironmentVariable variableName variableValue expectedValue ->
    "Invalid value for "
      <> T.pack variableName
      <> ": "
      <> T.pack (show variableValue)
      <> ". Expected "
      <> expectedValue
      <> "."
  InvalidSessionConfig path message ->
    "Invalid "
      <> T.pack path
      <> " session configuration: "
      <> message

sectionDocumentValue :: Text -> J.Value -> J.Value
sectionDocumentValue sectionName = \case
  J.Object obj ->
    case JKM.lookup (JK.fromText sectionName) obj of
      Nothing -> J.Object mempty
      Just value -> value
  _ ->
    J.Object mempty

quote :: Text -> Text
quote value =
  "\"" <> value <> "\""
