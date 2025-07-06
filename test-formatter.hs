#!/usr/bin/env cabal
{- cabal:
build-depends: base, text, pact-tng
-}

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Pact.Core.Formatter
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      content <- T.readFile filename
      case formatPactCode content of
        Left err -> do
          putStrLn $ "Error: " ++ show err
          exitFailure
        Right formatted -> T.putStr formatted
    _ -> do
      putStrLn "Usage: test-formatter <filename.pact>"
      exitFailure