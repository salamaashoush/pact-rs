{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Pact.Core.LanguageServer.Formatting
  ( documentFormattingHandler
  , documentRangeFormattingHandler
  , PactFormattingConfig(..)
  , defaultPactFormattingConfig
  , formatPactDocumentWithConfig
  , extractFormattingConfig
  ) where

import Control.Lens hiding (Iso, parts, List)
import Control.Monad.Trans (lift)
import Control.Monad.IO.Class
import Control.Monad.Reader (ask)
import Control.Concurrent.MVar
import Data.Text (Text)
import qualified Data.Text as T
import Language.LSP.Server
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types hiding (maybeToNull)
import qualified Language.LSP.Protocol.Lens as LSP
import Language.LSP.VFS hiding (line)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.:?), (.!=))

import Pact.Core.LanguageServer.Types
import qualified Pact.Core.Formatter as Formatter

-- | Pact formatting configuration
data PactFormattingConfig = PactFormattingConfig
  { pactIndentSize :: Int
  , pactMaxLineWidth :: Int
  , pactSpaceBetweenDefinitions :: Bool
  , pactClosingParensOnNewLine :: Bool
  , pactSpaceInsideModules :: Bool
  , pactAlignBindings :: Bool
  , pactAlignLetBindings :: Bool
  , pactBreakLongFunctionCalls :: Bool
  } deriving (Show, Eq)

-- | Default formatting configuration
defaultPactFormattingConfig :: PactFormattingConfig
defaultPactFormattingConfig = PactFormattingConfig
  { pactIndentSize = 2
  , pactMaxLineWidth = 80
  , pactSpaceBetweenDefinitions = True
  , pactClosingParensOnNewLine = False
  , pactSpaceInsideModules = True
  , pactAlignBindings = True
  , pactAlignLetBindings = True
  , pactBreakLongFunctionCalls = True
  }

-- JSON instances for configuration
instance Aeson.ToJSON PactFormattingConfig where
  toJSON config = Aeson.object
    [ "indentSize" Aeson..= pactIndentSize config
    , "maxLineWidth" Aeson..= pactMaxLineWidth config
    , "spaceBetweenDefinitions" Aeson..= pactSpaceBetweenDefinitions config
    , "closingParensOnNewLine" Aeson..= pactClosingParensOnNewLine config
    , "spaceInsideModules" Aeson..= pactSpaceInsideModules config
    , "alignBindings" Aeson..= pactAlignBindings config
    , "alignLetBindings" Aeson..= pactAlignLetBindings config
    , "breakLongFunctionCalls" Aeson..= pactBreakLongFunctionCalls config
    ]

instance Aeson.FromJSON PactFormattingConfig where
  parseJSON = Aeson.withObject "PactFormattingConfig" $ \o -> PactFormattingConfig
    <$> o .:? "indentSize" .!= pactIndentSize defaultPactFormattingConfig
    <*> o .:? "maxLineWidth" .!= pactMaxLineWidth defaultPactFormattingConfig
    <*> o .:? "spaceBetweenDefinitions" .!= pactSpaceBetweenDefinitions defaultPactFormattingConfig
    <*> o .:? "closingParensOnNewLine" .!= pactClosingParensOnNewLine defaultPactFormattingConfig
    <*> o .:? "spaceInsideModules" .!= pactSpaceInsideModules defaultPactFormattingConfig
    <*> o .:? "alignBindings" .!= pactAlignBindings defaultPactFormattingConfig
    <*> o .:? "alignLetBindings" .!= pactAlignLetBindings defaultPactFormattingConfig
    <*> o .:? "breakLongFunctionCalls" .!= pactBreakLongFunctionCalls defaultPactFormattingConfig

-- | Extract formatting configuration from LSP FormattingOptions and custom settings
extractFormattingConfig :: FormattingOptions -> Maybe Aeson.Value -> PactFormattingConfig
extractFormattingConfig lspOptions customSettings =
  let baseConfig = defaultPactFormattingConfig
        { pactIndentSize = fromIntegral (lspOptions ^. LSP.tabSize) }
      
      customConfig = case customSettings of
        Just settings -> case Aeson.fromJSON settings of
          Aeson.Success config -> config
          Aeson.Error _ -> baseConfig
        Nothing -> baseConfig
  
  in customConfig { pactIndentSize = fromIntegral (lspOptions ^. LSP.tabSize) }

-- | Convert LSP config to Formatter config
toFormatterConfig :: PactFormattingConfig -> Formatter.FormatterConfig
toFormatterConfig config = Formatter.FormatterConfig
  { Formatter.cfgIndentSize = pactIndentSize config
  , Formatter.cfgMaxLineWidth = pactMaxLineWidth config
  , Formatter.cfgSpaceBetweenDefinitions = pactSpaceBetweenDefinitions config
  , Formatter.cfgSpaceInsideModules = pactSpaceInsideModules config
  , Formatter.cfgAlignBindings = pactAlignBindings config
  , Formatter.cfgAlignLetBindings = pactAlignLetBindings config
  , Formatter.cfgBreakLongFunctionCalls = pactBreakLongFunctionCalls config
  , Formatter.cfgBreakBeforeClosingParen = pactClosingParensOnNewLine config
  }

-- | Main document formatting handler
documentFormattingHandler :: Handlers LSM
documentFormattingHandler = requestHandler SMethod_TextDocumentFormatting $ \req resp ->
  getState >>= \st -> do
    let params' = req ^. LSP.params
        uri' = params' ^. LSP.textDocument . LSP.uri
        nuri = toNormalizedUri uri'
        fmtOptions = params' ^. LSP.options
    
    -- Get the document content
    mvf <- getVirtualFile nuri
    case mvf of
      Nothing -> resp $ Right $ InR Null
      Just vf -> do
        let content = virtualFileText vf
        
        -- Extract formatting configuration from LSP options and stored workspace settings
        let storedConfig = st ^. lsFormattingConfig
            config = extractFormattingConfig fmtOptions storedConfig
        
        -- Apply configurable formatting
        case formatPactDocumentWithConfig config content of
          Nothing -> resp $ Right $ InR Null  -- Return null if formatting fails
          Just formattedText -> do
            -- Create a text edit that replaces the entire document
            let range = Range (Position 0 0) (Position (fromIntegral $ length $ T.lines content) 0)
                textEdit = TextEdit range formattedText
            resp $ Right $ InL [textEdit]

-- | Document range formatting handler
documentRangeFormattingHandler :: Handlers LSM
documentRangeFormattingHandler = requestHandler SMethod_TextDocumentRangeFormatting $ \req resp ->
  getState >>= \st -> do
    let params' = req ^. LSP.params
        uri' = params' ^. LSP.textDocument . LSP.uri
        nuri = toNormalizedUri uri'
        range = params' ^. LSP.range
        fmtOptions = params' ^. LSP.options
    
    -- Get the document content
    mvf <- getVirtualFile nuri
    case mvf of
      Nothing -> resp $ Right $ InR Null
      Just vf -> do
        let content = virtualFileText vf
            selectedText = extractTextRange content range
        
        -- Extract formatting configuration and format the selected range from workspace settings
        let storedConfig = st ^. lsFormattingConfig
            config = extractFormattingConfig fmtOptions storedConfig
        
        case formatPactDocumentWithConfig config selectedText of
          Nothing -> resp $ Right $ InR Null
          Just formattedText -> do
            let textEdit = TextEdit range formattedText
            resp $ Right $ InL [textEdit]

-- | Extract text from a specific range in the document
extractTextRange :: Text -> Range -> Text
extractTextRange content range = 
  let allLines = T.lines content
      startLine = fromIntegral $ range ^. LSP.start . LSP.line
      endLine = fromIntegral $ range ^. LSP.end . LSP.line
      startChar = fromIntegral $ range ^. LSP.start . LSP.character
      endChar = fromIntegral $ range ^. LSP.end . LSP.character
      
      selectedLines = take (endLine - startLine + 1) $ drop startLine allLines
  in case selectedLines of
    [] -> ""
    [singleLine] -> T.take (endChar - startChar) $ T.drop startChar singleLine
    firstLine:restLines -> 
      case reverse restLines of
        [] -> T.drop startChar firstLine
        lastLine:middleLines -> 
          let firstPart = T.drop startChar firstLine
              lastPart = T.take endChar lastLine
              middlePart = T.intercalate "\n" (reverse middleLines)
              allParts = filter (not . T.null) [firstPart, middlePart, lastPart]
          in T.intercalate "\n" allParts

-- | Format a Pact document with specific configuration using the standalone formatter
formatPactDocumentWithConfig :: PactFormattingConfig -> Text -> Maybe Text
formatPactDocumentWithConfig config content = 
  let formatterConfig = toFormatterConfig config
  in case Formatter.formatPactCodeWithConfig formatterConfig content of
       Left _ -> Nothing  -- Parsing failed, return Nothing to skip formatting
       Right formatted -> Just formatted

-- | Get LSState
getState :: LSM LSState
getState = lift ask >>= liftIO . readMVar