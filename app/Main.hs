{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString.Builder as BSB
import qualified Data.Text.IO as T
import Lamb.Graphviz
import Lamb.Parser
import System.IO

main :: IO ()
main = do
  source <- T.getContents
  case parseLamb "<stdin>" source of
    Left err ->
      putStrLn err
    Right toplevel ->
      BSB.hPutBuilder stdout $
        programToGraphviz
          [("fontname", "Noto Sans")]
          [("shape", "plaintext")]
          [("arrowhead", "none")]
          toplevel
