#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Aeson as Aeson
import Data.Aeson (object, (.=))
import qualified Data.Text.IO as T

-- Mock PactFormattingConfig for testing
data PactFormattingConfig = PactFormattingConfig
  { pactIndentSize :: Int
  , pactMaxLineWidth :: Int
  , pactSpaceBetweenDefinitions :: Bool
  , pactClosingParensOnNewLine :: Bool
  , pactSpaceInsideModules :: Bool
  } deriving (Show, Eq)

instance Aeson.FromJSON PactFormattingConfig where
  parseJSON = Aeson.withObject "PactFormattingConfig" $ \o -> PactFormattingConfig
    <$> o Aeson..:? "indentSize" Aeson..!= 2
    <*> o Aeson..:? "maxLineWidth" Aeson..!= 80
    <*> o Aeson..:? "spaceBetweenDefinitions" Aeson..!= True
    <*> o Aeson..:? "closingParensOnNewLine" Aeson..!= True
    <*> o Aeson..:? "spaceInsideModules" Aeson..!= True

-- Test workspace configuration parsing
testWorkspaceConfig :: IO ()
testWorkspaceConfig = do
  putStrLn "Testing workspace configuration parsing..."
  
  -- Simulate workspace settings JSON
  let workspaceSettings = object
        [ "formatting" .= object
            [ "indentSize" .= (4 :: Int)
            , "maxLineWidth" .= (120 :: Int)
            , "spaceBetweenDefinitions" .= False
            , "closingParensOnNewLine" .= False
            , "spaceInsideModules" .= True
            ]
        ]
  
  putStrLn $ "Input JSON: " ++ show workspaceSettings
  
  -- Test parsing the formatting section
  case Aeson.fromJSON workspaceSettings of
    Aeson.Success (config :: PactFormattingConfig) -> do
      putStrLn $ "✓ Successfully parsed config: " ++ show config
    Aeson.Error err -> do
      putStrLn $ "✗ Failed to parse config: " ++ err
  
  -- Test the formatting section extraction (like in handlePactConfiguration)
  case workspaceSettings of
    Aeson.Object obj -> case lookup "formatting" (Aeson.toList obj) of
      Just formattingValue -> case Aeson.fromJSON formattingValue of
        Aeson.Success (config :: PactFormattingConfig) -> do
          putStrLn $ "✓ Successfully extracted formatting config: " ++ show config
        Aeson.Error err -> do
          putStrLn $ "✗ Failed to parse formatting config: " ++ err
      Nothing -> putStrLn "✗ No formatting section found"
    _ -> putStrLn "✗ Not a JSON object"

main :: IO ()
main = testWorkspaceConfig