import Pact.Core.Formatter
import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = do
  let testCode = T.unlines
        [ "(module test-module \"test-keyset\""
        , "@doc \"Test module for formatter\""
        , "(defschema test-schema name:string age:integer)"
        , "(deftable test-table:{test-schema})"
        , "(defun add (x:integer y:integer) @doc \"Add two numbers\" (+ x y))"
        , "(defcap TRANSFER (from:string to:string amount:decimal)"
        , "@managed amount TRANSFER-mgr"
        , "(enforce (!= from to) \"from and to must be different\"))"
        , ")"
        ]
  
  putStrLn "Original code:"
  T.putStr testCode
  putStrLn "\n\nFormatted code:"
  
  case formatPactCode testCode of
    Left err -> putStrLn $ "Error: " ++ show err
    Right formatted -> T.putStr formatted