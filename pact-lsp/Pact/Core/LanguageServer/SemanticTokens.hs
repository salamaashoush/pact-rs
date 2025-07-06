{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Pact.Core.LanguageServer.SemanticTokens
  ( semanticTokensFullHandler
  , semanticTokensRangeHandler
  ) where

import Control.Lens hiding (List)
import Control.Monad.Trans (lift)
import Control.Monad.IO.Class
import Control.Monad.Reader (ask)
import Control.Concurrent.MVar
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word32)
import Language.LSP.Server
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types as LSP hiding (maybeToNull)
import qualified Language.LSP.Protocol.Lens as LSP
import qualified Data.Vector as V

import Pact.Core.Names
import Pact.Core.Info
import Pact.Core.Type
import Pact.Core.IR.Term
import Pact.Core.Builtin
import Pact.Core.Literal
import Pact.Core.LanguageServer.Types
import Pact.Core.Pretty (renderText)
import Data.List (sortOn)
import Data.List.NonEmpty (toList)

-- | Token types we support
data TokenType
  = TokenNamespace
  | TokenModule
  | TokenInterface
  | TokenFunction
  | TokenVariable
  | TokenConstant
  | TokenParameter
  | TokenKeyword
  | TokenString
  | TokenNumber
  | TokenCapability
  | TokenTable
  | TokenSchema
  deriving (Eq, Ord, Enum)

-- | Token modifiers we support
data TokenModifier
  = ModifierDeclaration
  | ModifierReadonly
  | ModifierAbstract
  | ModifierAsync
  | ModifierDefaultLibrary
  deriving (Eq, Ord, Enum)

-- | A semantic token with position and type info
data SemanticToken = SemanticToken
  { _stLine :: Int
  , _stColumn :: Int
  , _stLength :: Int
  , _stType :: TokenType
  , _stModifiers :: [TokenModifier]
  } deriving (Eq)

-- | Handle full document semantic tokens request
semanticTokensFullHandler :: Handlers LSM
semanticTokensFullHandler = requestHandler SMethod_TextDocumentSemanticTokensFull $ \req resp -> 
  getState >>= \st -> do
    let uri' = req ^. LSP.params . LSP.textDocument . LSP.uri
        nuri = toNormalizedUri uri'
    
    case view (lsTopLevel . at nuri) st of
      Nothing -> resp $ Right $ InR LSP.Null
      Just topLevels -> do
        let tokens = concatMap topLevelToTokens topLevels
            encoded = encodeSemanticTokens $ sortOn (\t -> (_stLine t, _stColumn t)) tokens
        resp $ Right $ InL $ LSP.SemanticTokens Nothing (map fromIntegral $ V.toList encoded)

-- | Handle range-based semantic tokens request
semanticTokensRangeHandler :: Handlers LSM
semanticTokensRangeHandler = requestHandler SMethod_TextDocumentSemanticTokensRange $ \req resp -> 
  getState >>= \st -> do
    let uri' = req ^. LSP.params . LSP.textDocument . LSP.uri
        nuri = toNormalizedUri uri'
        range = req ^. LSP.params . LSP.range
    
    case view (lsTopLevel . at nuri) st of
      Nothing -> resp $ Right $ InR LSP.Null
      Just topLevels -> do
        let tokens = filter (inRange range) $ concatMap topLevelToTokens topLevels
            encoded = encodeSemanticTokens $ sortOn (\t -> (_stLine t, _stColumn t)) tokens
        resp $ Right $ InL $ LSP.SemanticTokens Nothing (map fromIntegral $ V.toList encoded)

-- | Check if a token is within a range
inRange :: Range -> SemanticToken -> Bool
inRange (Range (Position sl sc) (Position el ec)) (SemanticToken l c len _ _) =
  let startLine = fromIntegral sl
      startCol = fromIntegral sc
      endLine = fromIntegral el
      endCol = fromIntegral ec
      tokenEndCol = c + len
  in (l > startLine || (l == startLine && c >= startCol)) &&
     (l < endLine || (l == endLine && tokenEndCol <= endCol))

-- | Convert top-level items to semantic tokens
topLevelToTokens :: EvalTopLevel ReplCoreBuiltin FileLocSpanInfo -> [SemanticToken]
topLevelToTokens = \case
  TLModule m -> moduleToTokens m
  TLInterface iface -> interfaceToTokens iface
  TLTerm term -> termToTokens term
  TLUse _ _ -> []

-- | Convert module to semantic tokens
moduleToTokens :: EvalModule ReplCoreBuiltin FileLocSpanInfo -> [SemanticToken]
moduleToTokens (Module mn _ defs _ _ _ _ _ _ i) =
  let modToken = makeToken i (renderModuleName mn) TokenModule [ModifierDeclaration]
      defTokens = concatMap defToTokens defs
  in modToken : defTokens

-- | Convert interface to semantic tokens
interfaceToTokens :: EvalInterface ReplCoreBuiltin FileLocSpanInfo -> [SemanticToken]
interfaceToTokens (Interface ifn defs _ _ _ _ i) =
  let ifaceToken = makeToken i (renderModuleName ifn) TokenInterface [ModifierDeclaration, ModifierAbstract]
      defTokens = concatMap ifDefToTokens defs
  in ifaceToken : defTokens

-- | Convert definitions to semantic tokens
defToTokens :: EvalDef ReplCoreBuiltin FileLocSpanInfo -> [SemanticToken]
defToTokens = \case
  Dfun (Defun spec args body i) -> 
    let funToken = makeToken i (renderArgName spec) TokenFunction [ModifierDeclaration]
        argTokens = concatMap argToTokens args
        bodyTokens = termToTokens body
    in funToken : argTokens ++ bodyTokens
  DConst (DefConst spec constVal i) -> 
    let constToken = makeToken i (renderArgName spec) TokenConstant [ModifierDeclaration, ModifierReadonly]
        valTokens = case constVal of
          TermConst term -> termToTokens term
          _ -> []
    in constToken : valTokens
  DTable (DefTable tn _ i) -> 
    [makeToken i (renderText tn) TokenTable [ModifierDeclaration]]
  DPact (DefPact spec args steps i) -> 
    let pactToken = makeToken i (renderArgName spec) TokenFunction [ModifierDeclaration, ModifierAsync]
        argTokens = concatMap argToTokens args
        stepTokens = concatMap stepToTokens (toList steps)
    in pactToken : argTokens ++ stepTokens
  DCap (DefCap spec args body _ i) -> 
    let capToken = makeToken i (renderArgName spec) TokenCapability [ModifierDeclaration]
        argTokens = concatMap argToTokens args
        bodyTokens = termToTokens body
    in capToken : argTokens ++ bodyTokens
  DSchema (DefSchema sn _ i) -> 
    [makeToken i (renderText sn) TokenSchema [ModifierDeclaration]]

-- | Convert interface definitions to semantic tokens
ifDefToTokens :: EvalIfDef ReplCoreBuiltin FileLocSpanInfo -> [SemanticToken]
ifDefToTokens = \case
  IfDfun (IfDefun n _ i) -> 
    [makeToken i (renderText n) TokenFunction [ModifierDeclaration, ModifierAbstract]]
  IfDConst (DefConst spec _ i) -> 
    [makeToken i (renderArgName spec) TokenConstant [ModifierDeclaration, ModifierAbstract, ModifierReadonly]]
  IfDCap (IfDefCap n _ _ i) -> 
    [makeToken i (renderText n) TokenCapability [ModifierDeclaration, ModifierAbstract]]
  IfDPact (IfDefPact n _ i) -> 
    [makeToken i (renderText n) TokenFunction [ModifierDeclaration, ModifierAbstract, ModifierAsync]]
  IfDSchema (DefSchema sn _ i) -> 
    [makeToken i (renderText sn) TokenSchema [ModifierDeclaration, ModifierAbstract]]

-- | Convert arguments to semantic tokens
argToTokens :: Arg Type FileLocSpanInfo -> [SemanticToken]
argToTokens (Arg n mty i) = 
  let paramToken = makeToken i (renderText n) TokenParameter []
      typeTokens = maybe [] (const []) mty  -- Type annotations handled separately
  in paramToken : typeTokens


-- | Convert defpact steps to semantic tokens
stepToTokens :: Step Name Type ReplCoreBuiltin FileLocSpanInfo -> [SemanticToken]
stepToTokens = \case
  Step body -> termToTokens body
  StepWithRollback body rollback -> termToTokens body ++ termToTokens rollback
  _ -> []

-- | Convert terms to semantic tokens
termToTokens :: Term Name Type ReplCoreBuiltin FileLocSpanInfo -> [SemanticToken]
termToTokens = go
  where
    go :: Term Name Type ReplCoreBuiltin FileLocSpanInfo -> [SemanticToken]
    go = \case
      Var (Name n ns) i -> 
        let tokenType = case ns of
              NTopLevel _ _ -> TokenFunction
              NBound _ -> TokenVariable
              _ -> TokenVariable
        in [makeToken i n tokenType []]
      Lam args body _ -> 
        let argTokens = concatMap argToTokens args
            bodyTokens = go body
        in argTokens ++ bodyTokens
      Let arg val body _ -> 
        let argToken = argToTokens arg
            valTokens = go val
            bodyTokens = go body
        in argToken ++ valTokens ++ bodyTokens
      App fn args _ -> go fn ++ concatMap go args
      Sequence e1 e2 _ -> go e1 ++ go e2
      Builtin b i -> [makeToken i (renderText b) TokenFunction [ModifierDefaultLibrary]]
      Constant lit i -> case lit of
        LString s -> [makeToken i ("\"" <> s <> "\"") TokenString []]
        LInteger n -> [makeToken i (T.pack $ show n) TokenNumber []]
        LDecimal d -> [makeToken i (T.pack $ show d) TokenNumber []]
        LBool b -> [makeToken i (if b then "true" else "false") TokenKeyword []]
        _ -> []
      ListLit vs _ -> concatMap go vs
      ObjectLit fields _ -> concatMap (go . snd) fields
      BuiltinForm bf _ -> case bf of
        CAnd l r -> go l ++ go r
        COr l r -> go l ++ go r
        CIf c t e -> go c ++ go t ++ go e
        CEnforce c b -> go c ++ go b
        CEnforceOne e1 e2 -> go e1 ++ go e2
        CWithCapability cap body -> go cap ++ go body
        CTry e1 e2 -> go e1 ++ go e2
        CCreateUserGuard e -> go e
        CError e -> go e
        CPure e -> go e
      Nullary tm _ -> go tm
      _ -> []

-- | Create a semantic token from info
makeToken :: FileLocSpanInfo -> Text -> TokenType -> [TokenModifier] -> SemanticToken
makeToken i text ttype mods = 
  let SpanInfo sl sc _ _ = view spanInfo i
  in SemanticToken 
       (fromIntegral sl) 
       (fromIntegral sc) 
       (T.length text)
       ttype
       mods

-- | Encode tokens according to LSP spec (relative positions)
encodeSemanticTokens :: [SemanticToken] -> V.Vector Word32
encodeSemanticTokens tokens = V.fromList $ concatMap encodeToken $ zip (SemanticToken 0 0 0 TokenNamespace [] : tokens) tokens
  where
    encodeToken (prev, curr) =
      let deltaLine = fromIntegral $ _stLine curr - _stLine prev
          deltaStart = if _stLine curr == _stLine prev
                       then fromIntegral $ _stColumn curr - _stColumn prev
                       else fromIntegral $ _stColumn curr
          tokenLength = fromIntegral $ _stLength curr
          tokenType = fromIntegral $ fromEnum $ _stType curr
          tokenModifiers = foldr (\m acc -> acc .|. (1 `shiftL` fromEnum m)) 0 (_stModifiers curr)
      in [deltaLine, deltaStart, tokenLength, tokenType, tokenModifiers]
    
    shiftL :: Word32 -> Int -> Word32
    shiftL = \w n -> w * (2 ^ n)
    
    (.|.) :: Word32 -> Word32 -> Word32
    (.|.) = \a b -> a + b

-- | Render argument name
renderArgName :: Arg ty info -> Text
renderArgName (Arg n _ _) = renderText n

-- | Get LSState
getState :: LSM LSState
getState = lift ask >>= liftIO . readMVar