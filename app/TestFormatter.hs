{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

import Pact.Core.Formatter

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> testInlineCode
    [filename] -> formatFile filename
    _ -> do
      putStrLn "Usage: test-formatter [filename]"
      exitFailure

formatFile :: FilePath -> IO ()
formatFile filename = do
  content <- T.readFile filename
  putStrLn $ "Formatting file: " ++ filename
  putStrLn "Original code:"
  T.putStr content
  putStrLn "\n\nFormatted code:"
  case formatPactCode content of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      exitFailure
    Right formatted -> do
      T.putStr formatted
      exitSuccess

testInlineCode :: IO ()
testInlineCode = do
  let testCases = 
        [ ( "Simple module"
          , T.unlines
            [ "(module simple \"admin-keyset\""
            , "(defun hello (name:string) "
            , "(format \"Hello, {}!\" [name])))"
            ]
          )
        , ( "Module with schema"
          , T.unlines
            [ "(module test-module \"test-keyset\""
            , "@doc \"Test module\""
            , "(defschema user name:string age:integer email:string)"
            , "(deftable users:{user})"
            , "(defun create-user (user:object{user})"
            , "(insert users (at 'name user) user))"
            , ")"
            ]
          )
        , ( "Complex expressions"
          , T.unlines
            [ "(module complex \"keyset\""
            , "(defun test-let (x:integer y:integer)"
            , "(let ((sum (+ x y)) (prod (* x y)))"
            , "(if (> sum prod) sum prod)))"
            , "(defun test-lambda (items:[integer])"
            , "(map (lambda (x) (* x 2)) items))"
            , ")"
            ]
          )
        ]
  
  mapM_ testCase testCases

testCase :: (String, T.Text) -> IO ()
testCase (name, code) = do
  putStrLn $ "\n=== Test: " ++ name ++ " ==="
  putStrLn "Original:"
  T.putStr code
  putStrLn "\nFormatted:"
  case formatPactCode code of
    Left err -> putStrLn $ "Error: " ++ show err
    Right formatted -> T.putStr formatted
  putStrLn ""