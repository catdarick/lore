{-# LANGUAGE CPP #-}

module Lore.Mcp.Version
  ( loreVersionText,
    ghcVersionText,
    targetText,
    versionJson,
    printVersionJson,
  )
where

import Data.Aeson (Value, encode, object, (.=))
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Version (cProjectVersion)

-- Keep this in sync with lore-mcp/package.yaml. The release workflow verifies
-- the executable-reported version against both the package version and tag.
loreVersionText :: Text
loreVersionText = "0.1.0.0"

ghcVersionText :: Text
ghcVersionText = T.pack cProjectVersion

targetText :: Text
targetText =
#if defined(linux_HOST_OS) && defined(x86_64_HOST_ARCH)
  "linux-x64-gnu"
#else
  "unknown"
#endif

versionJson :: Value
versionJson =
  object
    [ "loreVersion" .= loreVersionText,
      "ghcVersion" .= ghcVersionText,
      "target" .= targetText
    ]

printVersionJson :: IO ()
printVersionJson = BL8.putStrLn (encode versionJson)
