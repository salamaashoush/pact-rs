#!/usr/bin/env cabal
{- cabal:
build-depends: 
  base,
  text,
  pact-tng
-}
{-# LANGUAGE OverloadedStrings #-}

import Pact.Core.Formatter
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      content <- T.readFile filename
      case formatPactCode content of
        Left err -> putStrLn $ "Error: " ++ show err
        Right formatted -> T.putStr formatted
    _ -> do
      -- Test with inline code
      let testCode = T.unlines
            [ "(module test-module \"test-keyset\""
            , "(defschema test-schema name:string age:integer)"
            , "(deftable test-table:{test-schema})"
            , "(defun add (x:integer y:integer) (+ x y))"
            , ")"
            ]
      
      putStrLn "Original code:"
      T.putStr testCode
      putStrLn "\nFormatted code:"
      
      case formatPactCode testCode of
        Left err -> putStrLn $ "Error: " ++ show err
        Right formatted -> T.putStr formatted