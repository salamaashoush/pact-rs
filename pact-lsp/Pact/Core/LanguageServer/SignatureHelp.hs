{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

module Pact.Core.LanguageServer.SignatureHelp
  ( signatureHelpHandler
  ) where

import Control.Lens hiding (Iso, parts)
import Control.Monad.Trans (lift)
import Control.Monad.IO.Class
import Control.Monad.Reader (ask)
import Control.Concurrent.MVar
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe
import qualified Data.Map.Strict as M
import qualified Data.List as L
import Control.Applicative ((<|>))
import Language.LSP.Server
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types hiding (maybeToNull)
import qualified Language.LSP.Protocol.Lens as LSP
import Language.LSP.VFS

import Pact.Core.Builtin
import Pact.Core.Names
import Pact.Core.Info
import Pact.Core.IR.Term
import Pact.Core.Type
import Pact.Core.Repl.BuiltinDocs
import Pact.Core.Repl.BuiltinDocs.Internal
import Pact.Core.LanguageServer.Types
import Pact.Core.LanguageServer.Utils
import Pact.Core.Environment.Types

-- | Main signature help handler
signatureHelpHandler :: Handlers LSM
signatureHelpHandler = requestHandler SMethod_TextDocumentSignatureHelp $ \req resp ->
  getState >>= \st -> do
    let params' = req ^. LSP.params
        uri' = params' ^. LSP.textDocument . LSP.uri
        nuri = toNormalizedUri uri'
        pos = params' ^. LSP.position
    
    -- Get virtual file to access current text
    mvf <- getVirtualFile nuri
    case mvf of
      Nothing -> resp $ Right $ InR Null
      Just vf -> do
        let content = virtualFileText vf
            lineText = getLineAt content pos
            (functionCall, paramIndex) = extractFunctionCall lineText pos
        
        let topLevels = fromMaybe [] $ preview (lsTopLevel . at nuri . _Just) st
            replState = preview (lsReplState . at nuri . _Just) st
        
        mSigHelp <- generateSignatureHelp functionCall paramIndex topLevels replState
        
        resp $ Right $ maybeToNull mSigHelp

-- | Generate signature help for the function at cursor
generateSignatureHelp 
  :: Maybe Text -- ^ Function name being called
  -> Int -- ^ Current parameter index
  -> [EvalTopLevel ReplCoreBuiltin FileLocSpanInfo] -- ^ Top level definitions
  -> Maybe (ReplState ReplCoreBuiltin) -- ^ REPL state
  -> LSM (Maybe SignatureHelp)
generateSignatureHelp Nothing _ _ _ = pure Nothing
generateSignatureHelp (Just funcName) paramIndex topLevels _replState = do
  -- Look for signatures in different sources
  let builtinSigs = builtinSignatures funcName paramIndex
      userSigs = userDefinedSignatures funcName paramIndex topLevels
      allSigs = builtinSigs ++ userSigs
  
  case allSigs of
    [] -> pure Nothing
    sigs -> pure $ Just $ SignatureHelp
      { _signatures = sigs
      , _activeSignature = Just 0  -- First signature is active
      , _activeParameter = Just $ InL $ fromIntegral paramIndex
      }

-- | Extract function call information from line text
extractFunctionCall :: Text -> Position -> (Maybe Text, Int)
extractFunctionCall lineText (Position _ col) = 
  let beforeCursor = T.take (fromIntegral col) lineText
      -- Find the function call by looking backwards for opening parenthesis
      (funcName, paramIndex) = findFunctionCall beforeCursor
  in (funcName, paramIndex)

-- | Find function call and parameter index
findFunctionCall :: Text -> (Maybe Text, Int)
findFunctionCall text = 
  case findParenRange text of
    Nothing -> (Nothing, 0)
    Just (startPos, _) -> 
      let beforeParen = T.take startPos text
          afterParen = T.drop (startPos + 1) text
          funcName = extractFunctionName beforeParen
          paramIndex = countParameters afterParen
      in (funcName, paramIndex)

-- | Find the range of the current parentheses
findParenRange :: Text -> Maybe (Int, Int)
findParenRange text = 
  let chars = T.unpack text
      positions = zip chars [0..]
  in findParenRangeHelper positions 0 Nothing Nothing

findParenRangeHelper :: [(Char, Int)] -> Int -> Maybe Int -> Maybe Int -> Maybe (Int, Int)
findParenRangeHelper [] _ mStart mEnd = (,) <$> mStart <*> mEnd
findParenRangeHelper ((c, pos):rest) level mStart mEnd = case c of
  '(' -> findParenRangeHelper rest (level + 1) (mStart <|> Just pos) mEnd
  ')' -> if level == 1 
         then findParenRangeHelper rest (level - 1) mStart (Just pos)
         else findParenRangeHelper rest (level - 1) mStart mEnd
  _ -> findParenRangeHelper rest level mStart mEnd

-- | Extract function name before parenthesis
extractFunctionName :: Text -> Maybe Text
extractFunctionName text = 
  let trimmed = T.stripEnd text
      funcName = T.takeWhileEnd isIdentChar trimmed
  in if T.null funcName then Nothing else Just funcName
  where
    isIdentChar c = c `elem` ("-_" :: String) || 
                   (c >= 'a' && c <= 'z') || 
                   (c >= 'A' && c <= 'Z') ||
                   (c >= '0' && c <= '9')

-- | Count parameters in function call
countParameters :: Text -> Int
countParameters text = 
  let params = T.split (== ' ') $ T.strip text
      nonEmptyParams = filter (not . T.null) params
  in length nonEmptyParams

-- | Generate signature help for builtin functions
builtinSignatures :: Text -> Int -> [SignatureInformation]
builtinSignatures funcName paramIndex = 
  case M.lookup funcName builtinDocs of
    Nothing -> []
    Just (MarkdownDoc docs) -> 
      [SignatureInformation
        { _label = funcName <> " (...)"
        , _documentation = Just $ InL docs
        , _parameters = Nothing -- We could extract parameters from docs
        , _activeParameter = Just $ InL $ fromIntegral paramIndex
        }]

-- | Generate signature help for user-defined functions
userDefinedSignatures :: Text -> Int -> [EvalTopLevel ReplCoreBuiltin FileLocSpanInfo] -> [SignatureInformation]
userDefinedSignatures funcName paramIndex topLevels = 
  concatMap (signaturesFromTopLevel funcName paramIndex) topLevels

-- | Extract signatures from top-level definition
signaturesFromTopLevel :: Text -> Int -> EvalTopLevel ReplCoreBuiltin FileLocSpanInfo -> [SignatureInformation]
signaturesFromTopLevel funcName paramIndex = \case
  TLModule m -> signaturesFromModule funcName paramIndex m
  TLInterface i -> signaturesFromInterface funcName paramIndex i
  _ -> []

-- | Extract signatures from module
signaturesFromModule :: Text -> Int -> EvalModule ReplCoreBuiltin FileLocSpanInfo -> [SignatureInformation]
signaturesFromModule funcName paramIndex (Module _ _ defs _ _ _ _ _ _ _) = 
  concatMap (signatureFromDef funcName paramIndex) defs

-- | Extract signatures from interface
signaturesFromInterface :: Text -> Int -> EvalInterface ReplCoreBuiltin FileLocSpanInfo -> [SignatureInformation]
signaturesFromInterface funcName paramIndex (Interface _ idefs _ _ _ _ _) = 
  mapMaybe (signatureFromIfDef funcName paramIndex) idefs

-- | Create signature from definition
signatureFromDef :: Text -> Int -> EvalDef ReplCoreBuiltin FileLocSpanInfo -> [SignatureInformation]
signatureFromDef funcName paramIndex = \case
  Dfun (Defun spec args _ _) -> 
    let fName = _argName spec
    in if fName == funcName
       then [makeSignatureInfo fName args paramIndex]
       else []
  DCap (DefCap spec args _ _ _) ->
    let capName = _argName spec
    in if capName == funcName
       then [makeSignatureInfo capName args paramIndex]
       else []
  DPact (DefPact spec args _ _) ->
    let pName = _argName spec
    in if pName == funcName
       then [makeSignatureInfo pName args paramIndex]
       else []
  _ -> []

-- | Create signature from interface definition
signatureFromIfDef :: Text -> Int -> EvalIfDef ReplCoreBuiltin FileLocSpanInfo -> Maybe SignatureInformation
signatureFromIfDef funcName paramIndex = \case
  IfDfun (IfDefun spec args _) ->
    let ifFuncName = _argName spec
    in if ifFuncName == funcName
       then Just $ makeSignatureInfo ifFuncName args paramIndex
       else Nothing
  IfDCap (IfDefCap spec args _ _) ->
    let icapName = _argName spec  
    in if icapName == funcName
       then Just $ makeSignatureInfo icapName args paramIndex
       else Nothing
  IfDPact (IfDefPact spec args _) ->
    let ipName = _argName spec
    in if ipName == funcName
       then Just $ makeSignatureInfo ipName args paramIndex
       else Nothing
  _ -> Nothing

-- | Create signature information
makeSignatureInfo :: Text -> [Arg Type FileLocSpanInfo] -> Int -> SignatureInformation
makeSignatureInfo funcName args paramIndex = 
  let paramLabels = map _argName args
      paramInfos = zipWith makeParameterInfo paramLabels [0..]
      signature = funcName <> " (" <> T.intercalate " " paramLabels <> ")"
  in SignatureInformation
    { _label = signature
    , _documentation = Nothing
    , _parameters = Just paramInfos
    , _activeParameter = Just $ InL $ fromIntegral paramIndex
    }

-- | Create parameter information
makeParameterInfo :: Text -> Int -> ParameterInformation
makeParameterInfo paramName _idx = ParameterInformation
  { _label = InL paramName
  , _documentation = Nothing
  }

-- | Get the line at the given position
getLineAt :: Text -> Position -> Text
getLineAt content (Position lineNum _) = 
  let lines' = T.lines content
  in if fromIntegral lineNum < L.length lines'
     then lines' !! fromIntegral lineNum
     else ""

-- | Get LSState
getState :: LSM LSState
getState = lift ask >>= liftIO . readMVar