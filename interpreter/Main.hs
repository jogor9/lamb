{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Arrow ((>>>))
import Data.Foldable
import Data.Function
import qualified Data.Text as T
import Lamb

main :: IO ()
main =
  putStrLn "Lamb v0.1.0.0"
    >> getContents
    >>= ( lines
            >>> traverse_
              ( T.pack >>> parseLamb "<stdin>" >>> \case
                  Left e -> putStrLn e
                  Right ast ->
                    runEval $
                      runEvalStep (traverse evalTopLevel ast) >>= \case
                        Left e -> liftIO $ putStrLn $ "error: " ++ T.unpack e
                        Right v ->
                          sequence v & \case
                            Left e -> liftIO $ putStrLn $ "error: " ++ T.unpack e
                            Right v' -> liftIO $ traverse_ (putStrLn . valueToString) v'
              )
        )
