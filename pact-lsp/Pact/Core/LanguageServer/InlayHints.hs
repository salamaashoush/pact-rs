{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Pact.Core.LanguageServer.InlayHints
  ( inlayHintHandler
  ) where

import Control.Lens hiding (List)
import Control.Monad.Trans (lift)
import Control.Monad.IO.Class
import Control.Monad.Reader (ask)
import Control.Concurrent.MVar
import qualified Data.Text as T
import Data.Maybe (catMaybes, mapMaybe)
import Language.LSP.Server
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types as LSP hiding (maybeToNull)
import qualified Language.LSP.Protocol.Lens as LSP

import Pact.Core.Names
import Pact.Core.Info
import Pact.Core.Type
import Pact.Core.IR.Term
import Pact.Core.Builtin
import Pact.Core.LanguageServer.Types
import Pact.Core.Pretty (renderText)
import qualified Data.Map.Strict as M
import Data.List.NonEmpty (toList)

-- | Handle inlay hint requests
inlayHintHandler :: Handlers LSM
inlayHintHandler = requestHandler SMethod_TextDocumentInlayHint $ \req resp -> 
  getState >>= \st -> do
    let uri' = req ^. LSP.params . LSP.textDocument . LSP.uri
        nuri = toNormalizedUri uri'
        range = req ^. LSP.params . LSP.range
    
    case view (lsTopLevel . at nuri) st of
      Nothing -> resp $ Right $ InR LSP.Null
      Just topLevels -> do
        let hints = concatMap (topLevelToHints range) topLevels
        resp $ Right $ InL hints

-- | Convert top-level items to inlay hints
topLevelToHints :: Range -> EvalTopLevel ReplCoreBuiltin FileLocSpanInfo -> [InlayHint]
topLevelToHints range = \case
  TLModule m -> moduleToHints range m
  TLInterface iface -> interfaceToHints range iface
  TLTerm term -> termToHints range term
  TLUse _ _ -> []

-- | Convert module to inlay hints
moduleToHints :: Range -> EvalModule ReplCoreBuiltin FileLocSpanInfo -> [InlayHint]
moduleToHints range m =
  concatMap (defToHints range) (_mDefs m)

-- | Convert interface to inlay hints
interfaceToHints :: Range -> EvalInterface ReplCoreBuiltin FileLocSpanInfo -> [InlayHint]
interfaceToHints range iface =
  concatMap (ifDefToHints range) (_ifDefns iface)

-- | Convert definitions to inlay hints
defToHints :: Range -> EvalDef ReplCoreBuiltin FileLocSpanInfo -> [InlayHint]
defToHints range = \case
  Dfun (Defun spec args body i) -> 
    let paramHints = parameterHintsForArgs args i
        bodyHints = termToHints range body
        returnHint = returnTypeHint spec i
    in filter (inRange range) $ catMaybes [returnHint] ++ paramHints ++ bodyHints
  DConst (DefConst spec constVal i) -> 
    let typeHint = constTypeHint spec i
        valHints = case constVal of
          TermConst term -> termToHints range term
          _ -> []
    in filter (inRange range) $ catMaybes [typeHint] ++ valHints
  DTable _ -> []
  DPact (DefPact _ args steps i) -> 
    let paramHints = parameterHintsForArgs args i
        stepHints = concatMap (stepToHints range) (toList steps)
    in filter (inRange range) $ paramHints ++ stepHints
  DCap (DefCap _ args body _ i) -> 
    let paramHints = parameterHintsForArgs args i
        bodyHints = termToHints range body
    in filter (inRange range) $ paramHints ++ bodyHints
  DSchema _ -> []

-- | Convert interface definitions to inlay hints
ifDefToHints :: Range -> EvalIfDef ReplCoreBuiltin FileLocSpanInfo -> [InlayHint]
ifDefToHints _ = \case
  IfDfun (IfDefun _ args i) -> parameterHintsForArgs args i
  _ -> []

-- | Convert defpact steps to inlay hints
stepToHints :: Range -> Step Name Type ReplCoreBuiltin FileLocSpanInfo -> [InlayHint]
stepToHints range = \case
  Step body -> termToHints range body
  StepWithRollback body rollback -> termToHints range body ++ termToHints range rollback
  _ -> []

-- | Convert terms to inlay hints
termToHints :: Range -> Term Name Type ReplCoreBuiltin FileLocSpanInfo -> [InlayHint]
termToHints range = go
  where
    go :: Term Name Type ReplCoreBuiltin FileLocSpanInfo -> [InlayHint]
    go = \case
      Lam args body i -> 
        let paramHints = lambdaParameterHints (toList args) i
            bodyHints = go body
        in filter (inRange range) paramHints ++ bodyHints
      Let arg val body i -> 
        let typeHint = letBindingTypeHint arg val i
            valHints = go val
            bodyHints = go body
        in filter (inRange range) (catMaybes [typeHint]) ++ valHints ++ bodyHints
      App fn args i -> 
        let fnHints = go fn
            argHints = concatMap go args
            callHints = functionCallHints fn args i
        in fnHints ++ argHints ++ filter (inRange range) callHints
      Sequence e1 e2 _ -> go e1 ++ go e2
      ListLit vs i -> 
        let elemHints = concatMap go vs
            typeHint = listTypeHint vs i
        in elemHints ++ filter (inRange range) (catMaybes [typeHint])
      ObjectLit fields i -> 
        let fieldHints = concatMap (go . snd) fields
            typeHint = objectTypeHint (M.fromList fields) i
        in fieldHints ++ filter (inRange range) (catMaybes [typeHint])
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

-- | Generate parameter hints for function arguments
parameterHintsForArgs :: [Arg Type FileLocSpanInfo] -> FileLocSpanInfo -> [InlayHint]
parameterHintsForArgs args _ = 
  mapMaybe makeParamHint args
  where
    makeParamHint (Arg _ mty i) = case mty of
      Nothing -> Nothing
      Just ty -> 
        let SpanInfo sl _ _ ec = view spanInfo i
            pos = Position (fromIntegral sl) (fromIntegral ec)
        in Just $ InlayHint
             pos
             (InL $ ": " <> renderType ty)
             (Just InlayHintKind_Type)
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing

-- | Generate parameter hints for lambda arguments
lambdaParameterHints :: [Arg Type FileLocSpanInfo] -> FileLocSpanInfo -> [InlayHint]
lambdaParameterHints = parameterHintsForArgs

-- | Generate return type hint
returnTypeHint :: Arg Type FileLocSpanInfo -> FileLocSpanInfo -> Maybe InlayHint
returnTypeHint (Arg _ mty _) funInfo = case mty of
  Nothing -> Nothing
  Just ty ->
    let SpanInfo sl sc _ _ = view spanInfo funInfo
        -- Place hint after function name
        pos = Position (fromIntegral sl) (fromIntegral sc + 10)  -- Approximate
    in Just $ InlayHint
         pos
         (InL $ " -> " <> renderType ty)
         (Just InlayHintKind_Type)
         Nothing
         Nothing
         Nothing
         Nothing
         Nothing

-- | Generate const type hint
constTypeHint :: Arg Type FileLocSpanInfo -> FileLocSpanInfo -> Maybe InlayHint
constTypeHint (Arg n mty _) i = case mty of
  Nothing -> Nothing
  Just ty ->
    let SpanInfo sl sc _ _ = view spanInfo i
        nameLen = T.length (renderText n)
        pos = Position (fromIntegral sl) (fromIntegral sc + fromIntegral nameLen)
    in Just $ InlayHint
         pos
         (InL $ ": " <> renderType ty)
         (Just InlayHintKind_Type)
         Nothing
         Nothing
         Nothing
         Nothing
         Nothing

-- | Generate let binding type hint
letBindingTypeHint :: Arg Type FileLocSpanInfo -> Term Name Type ReplCoreBuiltin FileLocSpanInfo -> FileLocSpanInfo -> Maybe InlayHint
letBindingTypeHint (Arg n mty _) _ i = case mty of
  Just _ -> Nothing  -- Already has type annotation
  Nothing ->
    let SpanInfo sl sc _ _ = view spanInfo i
        nameLen = T.length (renderText n)
        pos = Position (fromIntegral sl) (fromIntegral sc + fromIntegral nameLen + 4)  -- After "let "
    in Just $ InlayHint
         pos
         (InL ": <inferred>")  -- Would need type inference
         (Just InlayHintKind_Type)
         Nothing
         Nothing
         Nothing
         Nothing
         Nothing

-- | Generate function call parameter hints
functionCallHints :: Term Name Type ReplCoreBuiltin FileLocSpanInfo -> [Term Name Type ReplCoreBuiltin FileLocSpanInfo] -> FileLocSpanInfo -> [InlayHint]
functionCallHints fn args _ = case fn of
  Var (Name _ _) _ -> 
    -- Generate parameter name hints for function calls
    zipWith makeArgHint [0..] args
  _ -> []
  where
    makeArgHint idx arg = 
      let SpanInfo sl sc _ _ = view (termInfo . spanInfo) arg
          pos = Position (fromIntegral sl) (fromIntegral sc)
      in InlayHint
           pos
           (InL $ "arg" <> T.pack (show (idx :: Int)) <> ": ")
           (Just InlayHintKind_Parameter)
           Nothing
           Nothing
           Nothing
           Nothing
           Nothing

-- | Generate list type hint
listTypeHint :: [Term Name Type ReplCoreBuiltin FileLocSpanInfo] -> FileLocSpanInfo -> Maybe InlayHint
listTypeHint _ i = 
  let SpanInfo sl sc _ _ = view spanInfo i
      pos = Position (fromIntegral sl) (fromIntegral sc)
  in Just $ InlayHint
       pos
       (InR [InlayHintLabelPart "list" Nothing Nothing Nothing])
       (Just InlayHintKind_Type)
       Nothing
       Nothing
       Nothing
       Nothing
       Nothing

-- | Generate object type hint  
objectTypeHint :: M.Map Field (Term Name Type ReplCoreBuiltin FileLocSpanInfo) -> FileLocSpanInfo -> Maybe InlayHint
objectTypeHint _ i = 
  let SpanInfo sl sc _ _ = view spanInfo i
      pos = Position (fromIntegral sl) (fromIntegral sc)
  in Just $ InlayHint
       pos
       (InR [InlayHintLabelPart "object" Nothing Nothing Nothing])
       (Just InlayHintKind_Type)
       Nothing
       Nothing
       Nothing
       Nothing
       Nothing

-- | Check if a hint is within a range
inRange :: Range -> InlayHint -> Bool
inRange (Range (Position sl sc) (Position el ec)) hint =
  let Position l c = hint ^. LSP.position
      startLine = fromIntegral sl
      startCol = fromIntegral sc
      endLine = fromIntegral el
      endCol = fromIntegral ec
  in (l > startLine || (l == startLine && c >= startCol)) &&
     (l < endLine || (l == endLine && c <= endCol))

-- | Get LSState
getState :: LSM LSState
getState = lift ask >>= liftIO . readMVar