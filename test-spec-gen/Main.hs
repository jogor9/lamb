module Main (main) where

import qualified Data.Text.IO as T
import Lamb.Parser (parseLamb)
import System.Environment
import Text.Printf

main :: IO ()
main = do
  [fileName] <- getArgs
  file <- T.readFile fileName
  case parseLamb fileName file of
    Left err -> fail $ printf "file '%s' doesn't parse:\n%s\n" fileName err
    Right prog -> writeFile (fileName ++ ".spec") (show prog)
