module Lore.Config
  ( LoreConfig (..),
    DeadCodeConfig (..),
    SymbolSearchConfig (..),
    LoadedConfigDocument (..),
    ConfigError (..),
    LoreConfigError (..),
    SynonymGroupError (..),
    defaultLoreConfig,
    defaultDeadCodeConfig,
    defaultSymbolSearchConfig,
    loadLoreConfig,
    loadConfigDocumentAt,
    loreConfigFileName,
    projectSynonymLexicon,
    renderLoreConfigError,
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import qualified Data.Aeson as J
import qualified Data.Aeson.Key as JK
import qualified Data.Aeson.KeyMap as JKM
import Data.Aeson.Types (Parser)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Lore.Internal.Config.Document
  ( ConfigError (..),
    LoadedConfigDocument (..),
    loadConfigDocumentAt,
  )
import Lore.Internal.Lookup.SymbolSearch.Synonyms
  ( SynonymGroupError (..),
    SynonymLexicon,
    compileSynonymGroups,
    renderSynonymGroupError,
  )
import Lore.Internal.Monad (MonadLore)
import Lore.Internal.Session (SessionContext (configFilePath))

data LoreConfig = LoreConfig
  { loreConfigDeadCode :: DeadCodeConfig,
    loreConfigSymbolSearch :: SymbolSearchConfig
  }
  deriving stock (Eq, Show)

data DeadCodeConfig = DeadCodeConfig
  { deadCodeConfigAliveModules :: [Text],
    deadCodeConfigAliveSymbols :: [Text]
  }
  deriving stock (Eq, Show)

data SymbolSearchConfig = SymbolSearchConfig
  { symbolSearchSynonymGroups :: [[Text]]
  }
  deriving stock (Eq, Show)

data LoreConfigError
  = LoreConfigParseError FilePath Text
  | LoreConfigInvalidSynonymGroups FilePath (NE.NonEmpty SynonymGroupError)
  deriving stock (Eq, Show)

defaultLoreConfig :: LoreConfig
defaultLoreConfig =
  LoreConfig
    { loreConfigDeadCode = defaultDeadCodeConfig,
      loreConfigSymbolSearch = defaultSymbolSearchConfig
    }

defaultDeadCodeConfig :: DeadCodeConfig
defaultDeadCodeConfig =
  DeadCodeConfig
    { deadCodeConfigAliveModules = [],
      deadCodeConfigAliveSymbols = []
    }

defaultSymbolSearchConfig :: SymbolSearchConfig
defaultSymbolSearchConfig =
  SymbolSearchConfig
    { symbolSearchSynonymGroups = []
    }

loadLoreConfig :: (MonadLore m) => m (Either LoreConfigError LoreConfig)
loadLoreConfig = do
  path <- asks (.configFilePath)
  eiDocument <- liftIO (loadConfigDocumentAt path)
  pure $
    case eiDocument of
      Left (ConfigFileParseError configPath parseError) ->
        Left (LoreConfigParseError configPath parseError)
      Left (InvalidSessionConfig configPath message) ->
        Left (LoreConfigParseError configPath message)
      Left InvalidSessionEnvironmentVariable {} ->
        Left (LoreConfigParseError path "unexpected session environment error while loading lore.yaml")
      Right document ->
        case J.fromJSON document.configFileValue of
          J.Success config ->
            Right config
          J.Error err ->
            Left (LoreConfigParseError document.configFilePath (T.pack err))

loreConfigFileName :: FilePath
loreConfigFileName = "lore.yaml"

projectSynonymLexicon :: LoreConfig -> Either LoreConfigError SynonymLexicon
projectSynonymLexicon config =
  case compileSynonymGroups config.loreConfigSymbolSearch.symbolSearchSynonymGroups of
    Right lexicon ->
      Right lexicon
    Left errors ->
      Left
        (LoreConfigInvalidSynonymGroups loreConfigFileName errors)

renderLoreConfigError :: LoreConfigError -> Text
renderLoreConfigError = \case
  LoreConfigParseError loreConfigErrorFile loreConfigParseError ->
    "Failed to parse "
      <> quote (T.pack loreConfigErrorFile)
      <> ": "
      <> loreConfigParseError
  LoreConfigInvalidSynonymGroups loreConfigErrorFile loreConfigSynonymGroupErrors ->
    "Invalid "
      <> T.pack loreConfigErrorFile
      <> " symbol-search configuration:\n"
      <> T.intercalate "\n" (map renderSynonymGroupError (NE.toList loreConfigSynonymGroupErrors))

instance J.FromJSON LoreConfig where
  parseJSON = J.withObject "LoreConfig" \obj ->
    rejectLegacyDeadCodeKeys obj *> do
      LoreConfig
        <$> obj J..:? "dead-code" J..!= defaultDeadCodeConfig
        <*> obj J..:? "symbol-search" J..!= defaultSymbolSearchConfig

instance J.FromJSON DeadCodeConfig where
  parseJSON = J.withObject "DeadCodeConfig" \obj ->
    DeadCodeConfig
      <$> obj J..:? "alive-modules" J..!= []
      <*> obj J..:? "alive-symbols" J..!= []

instance J.FromJSON SymbolSearchConfig where
  parseJSON = J.withObject "SymbolSearchConfig" \obj ->
    SymbolSearchConfig
      <$> obj J..:? "synonym-groups" J..!= []

quote :: Text -> Text
quote value =
  "\"" <> value <> "\""

rejectLegacyDeadCodeKeys :: J.Object -> Parser ()
rejectLegacyDeadCodeKeys obj
  | hasKey "alive-modules" =
      fail "alive-modules must be nested under dead-code.alive-modules"
  | hasKey "alive-symbols" =
      fail "alive-symbols must be nested under dead-code.alive-symbols"
  | otherwise =
      pure ()
  where
    hasKey =
      (`JKM.member` obj) . JK.fromString
