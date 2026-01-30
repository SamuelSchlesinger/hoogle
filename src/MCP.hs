-- | Main entry point for the Hoogle MCP server
module Main where

import Action.MCP (actionMCP)

main :: IO ()
main = actionMCP
