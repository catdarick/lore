module Lore.Tools.Config
  ( LoreConfig (..),
    defaultLoreConfig,
    loadLoreConfig,
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.RWS (asks)
import qualified Data.Aeson as J
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Yaml as Y
import Lore.Monad (MonadLore)
import Lore.Session (SessionContext (..))
import System.Directory (doesFileExist)
import System.FilePath ((</>))

data LoreConfig = LoreConfig
  { loreConfigAliveModules :: [Text],
    loreConfigAliveSymbols :: [Text]
  }

defaultLoreConfig :: LoreConfig
defaultLoreConfig =
  LoreConfig
    { loreConfigAliveModules = [],
      loreConfigAliveSymbols = []
    }

loadLoreConfig :: (MonadLore m) => m (Either Text LoreConfig)
loadLoreConfig = do
  rootPath <- asks projectRoot
  let configPath =
        rootPath </> loreConfigFileName
  configExists <- liftIO (doesFileExist configPath)
  if not configExists
    then pure (Right defaultLoreConfig)
    else do
      eiConfig <- liftIO (Y.decodeFileEither configPath)
      pure $
        case eiConfig of
          Left parseError ->
            Left $
              "Failed to parse \""
                <> T.pack loreConfigFileName
                <> "\": "
                <> T.pack (Y.prettyPrintParseException parseError)
          Right config ->
            Right config

loreConfigFileName :: FilePath
loreConfigFileName = "lore.yaml"

instance J.FromJSON LoreConfig where
  parseJSON = J.withObject "LoreConfig" \obj ->
    LoreConfig
      <$> obj J..:? "alive-modules" J..!= []
      <*> obj J..:? "alive-symbols" J..!= []
