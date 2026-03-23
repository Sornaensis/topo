module Main (main) where

import System.IO (hPutStrLn, stderr)

import Topo.MCP.IPC (newIpcConnectionRef)
import Topo.MCP.Server (runMcpServer)

main :: IO ()
main = do
  hPutStrLn stderr "[topo-mcp] starting..."
  connRef <- newIpcConnectionRef
  runMcpServer connRef
