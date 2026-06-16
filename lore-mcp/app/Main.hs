module Main
  ( main,
  )
where

import Lore.Mcp.Server (runLoreMcpServer)
import Lore.Mcp.Version (printVersionJson)
import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--version-json"] -> printVersionJson
    [] -> runLoreMcpServer
    _ -> die "usage: lore-mcp [--version-json]"
