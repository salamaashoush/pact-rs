{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Pact.Core.LanguageServer.DocumentHighlights
  ( documentHighlightHandler
  , getMatch
  ) where

import Control.Lens hiding (List)
import Control.Monad.Trans (lift)
import Control.Monad.IO.Class
import Control.Monad.Reader (ask)
import Control.Concurrent.MVar
import Data.Text (Text)
import Language.LSP.Server
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types as LSP hiding (maybeToNull)
import qualified Language.LSP.Protocol.Lens as LSP

import Pact.Core.Names
import Pact.Core.Info
import Pact.Core.IR.Term
import Pact.Core.Type
import Pact.Core.Builtin
import Pact.Core.LanguageServer.Types
import Pact.Core.LanguageServer.Utils
import Pact.Core.Pretty (renderText)
import Data.List.NonEmpty (toList)
import Control.Applicative ((<|>))

-- | Handle document highlight requests (highlight all occurrences of symbol under cursor)
documentHighlightHandler :: Handlers LSM
documentHighlightHandler = requestHandler SMethod_TextDocumentDocumentHighlight $ \req resp -> 
  getState >>= \st -> do
    let uri' = req ^. LSP.params . LSP.textDocument . LSP.uri
        nuri = toNormalizedUri uri'
        pos = req ^. LSP.params . LSP.position
    
    case view (lsTopLevel . at nuri) st of
      Nothing -> resp $ Right $ InR LSP.Null
      Just topLevels -> do
        -- Find what's under the cursor
        case getMatch pos topLevels of
          Nothing -> resp $ Right $ InR LSP.Null
          Just posMatch -> do
            -- Find all occurrences of the same symbol
            let highlights = findHighlights posMatch topLevels
            resp $ Right $ InL highlights

-- | Find all highlights for a position match
findHighlights :: PositionMatch ReplCoreBuiltin FileLocSpanInfo -> [EvalTopLevel ReplCoreBuiltin FileLocSpanInfo] -> [DocumentHighlight]
findHighlights posMatch topLevels = case posMatch of
  TermMatch term -> findTermHighlights term topLevels
  _ -> []  -- Other match types not implemented yet

-- | Find all occurrences of a term
findTermHighlights :: Term Name Type ReplCoreBuiltin FileLocSpanInfo -> [EvalTopLevel ReplCoreBuiltin FileLocSpanInfo] -> [DocumentHighlight]
findTermHighlights targetTerm topLevels = case targetTerm of
  Var (Name n ns) _ -> findNameHighlights n ns topLevels
  _ -> []  -- Only highlight variable references for now


-- | Find all occurrences of a name
findNameHighlights :: Text -> NameKind -> [EvalTopLevel ReplCoreBuiltin FileLocSpanInfo] -> [DocumentHighlight]
findNameHighlights name nameKind topLevels = 
  concatMap (findInTopLevel name nameKind) topLevels

-- | Find name occurrences in a top-level item
findInTopLevel :: Text -> NameKind -> EvalTopLevel ReplCoreBuiltin FileLocSpanInfo -> [DocumentHighlight]
findInTopLevel name nameKind = \case
  TLModule m -> findInModule name nameKind m
  TLInterface iface -> findInInterface name nameKind iface
  TLTerm term -> findInTerm name nameKind term
  TLUse _ _ -> []

-- | Find name occurrences in a module
findInModule :: Text -> NameKind -> EvalModule ReplCoreBuiltin FileLocSpanInfo -> [DocumentHighlight]
findInModule name nameKind m =
  concatMap (findInDef name nameKind) (_mDefs m)

-- | Find name occurrences in an interface
findInInterface :: Text -> NameKind -> EvalInterface ReplCoreBuiltin FileLocSpanInfo -> [DocumentHighlight]
findInInterface name nameKind iface =
  concatMap (findInIfDef name nameKind) (_ifDefns iface)

-- | Find name occurrences in a definition
findInDef :: Text -> NameKind -> EvalDef ReplCoreBuiltin FileLocSpanInfo -> [DocumentHighlight]
findInDef name nameKind = \case
  Dfun (Defun spec _ body i) -> 
    let defHighlight = if renderArgName spec == name
                       then [makeHighlight DocumentHighlightKind_Write i]
                       else []
        bodyHighlights = findInTerm name nameKind body
    in defHighlight ++ bodyHighlights
  DConst (DefConst spec constVal i) -> 
    let constHighlight = if renderArgName spec == name
                         then [makeHighlight DocumentHighlightKind_Write i]
                         else []
        termHighlights = case constVal of
                           TermConst term -> findInTerm name nameKind term
                           _ -> []
    in constHighlight ++ termHighlights
  DTable (DefTable tn _ i) -> 
    if renderText tn == name
    then [makeHighlight DocumentHighlightKind_Write i]
    else []
  DPact (DefPact spec _ steps i) -> 
    let pactHighlight = if renderArgName spec == name
                        then [makeHighlight DocumentHighlightKind_Write i]
                        else []
        stepHighlights = concatMap (findInStep name nameKind) (toList steps)
    in pactHighlight ++ stepHighlights
  DCap (DefCap spec _ body _ i) -> 
    let capHighlight = if renderArgName spec == name
                       then [makeHighlight DocumentHighlightKind_Write i]
                       else []
        bodyHighlights = findInTerm name nameKind body
    in capHighlight ++ bodyHighlights
  DSchema (DefSchema sn _ i) -> 
    if renderText sn == name
    then [makeHighlight DocumentHighlightKind_Write i]
    else []

-- | Find name occurrences in interface definitions
findInIfDef :: Text -> NameKind -> EvalIfDef ReplCoreBuiltin FileLocSpanInfo -> [DocumentHighlight]
findInIfDef name _ = \case
  IfDfun (IfDefun n _ i) -> 
    if renderText n == name
    then [makeHighlight DocumentHighlightKind_Write i]
    else []
  IfDConst (DefConst spec _ i) -> 
    if renderArgName spec == name
    then [makeHighlight DocumentHighlightKind_Write i]
    else []
  IfDCap (IfDefCap n _ _ i) -> 
    if renderText n == name
    then [makeHighlight DocumentHighlightKind_Write i]
    else []
  IfDPact (IfDefPact n _ i) -> 
    if renderText n == name
    then [makeHighlight DocumentHighlightKind_Write i]
    else []
  IfDSchema (DefSchema sn _ i) -> 
    if renderText sn == name
    then [makeHighlight DocumentHighlightKind_Write i]
    else []

-- | Find name occurrences in a defpact step
findInStep :: Text -> NameKind -> Step Name Type ReplCoreBuiltin FileLocSpanInfo -> [DocumentHighlight]
findInStep name nameKind step = case step of
  Step body -> findInTerm name nameKind body
  StepWithRollback body _ -> findInTerm name nameKind body
  _ -> []

-- | Find name occurrences in a term
findInTerm :: Text -> NameKind -> Term Name Type ReplCoreBuiltin FileLocSpanInfo -> [DocumentHighlight]
findInTerm targetName targetKind = go
  where
    go :: Term Name Type ReplCoreBuiltin FileLocSpanInfo -> [DocumentHighlight]
    go = \case
      Var (Name n ns) i -> 
        if n == targetName && isCompatibleNameKind ns targetKind
        then [makeHighlight DocumentHighlightKind_Read i]
        else []
      Lam args body _ -> 
        let argNames = map (\(Arg n _ _) -> renderText n) (toList args)
            -- Don't highlight if the name is shadowed by a lambda argument
            bodyHighlights = if targetName `elem` argNames
                             then []
                             else go body
        in bodyHighlights
      Let arg val body _ -> 
        let argName' = case arg of Arg n _ _ -> renderText n
            valHighlights = go val
            -- Don't highlight in body if shadowed by let binding
            bodyHighlights = if targetName == argName'
                             then []
                             else go body
        in valHighlights ++ bodyHighlights
      App fn args _ -> go fn ++ concatMap go args
      Sequence e1 e2 _ -> go e1 ++ go e2
      BuiltinForm bf _ -> case bf of
        CAnd l r -> go l ++ go r
        COr l r -> go l ++ go r
        CIf c t e -> go c ++ go t ++ go e
        CEnforce c mb -> go c ++ go mb
        CEnforceOne e1 e2 -> go e1 ++ go e2
        CWithCapability cap body -> go cap ++ go body
        CTry e1 e2 -> go e1 ++ go e2
        CCreateUserGuard e -> go e
        CError e -> go e
        CPure e -> go e
      Builtin _ _ -> []
      Constant _ _ -> []
      ListLit vs _ -> concatMap go vs
      ObjectLit m _ -> concatMap (go . snd) m
      Nullary tm _ -> go tm
      _ -> []  -- Handle other cases as needed

-- | Check if name kinds are compatible (e.g., NTopLevel matches function references)
isCompatibleNameKind :: NameKind -> NameKind -> Bool
isCompatibleNameKind nk1 nk2 = case (nk1, nk2) of
  (NTopLevel mn1 _, NTopLevel mn2 _) -> mn1 == mn2
  (NBound _, NBound _) -> True
  _ -> False

-- | Create a document highlight
makeHighlight :: DocumentHighlightKind -> FileLocSpanInfo -> DocumentHighlight
makeHighlight kind i = DocumentHighlight
  (spanInfoToRange (view spanInfo i))
  (Just kind)

-- | Render argument name
renderArgName :: Arg ty info -> Text
renderArgName (Arg n _ _) = renderText n

-- | Get LSState
getState :: LSM LSState
getState = lift ask >>= liftIO . readMVar

-- | Find position match in top levels
getMatch :: Position -> [EvalTopLevel ReplCoreBuiltin FileLocSpanInfo] -> Maybe (PositionMatch ReplCoreBuiltin FileLocSpanInfo)
getMatch pos topLevels = foldr (<|>) Nothing (map (topLevelTermAt pos) topLevels)

-- | Convert SpanInfo to Range
spanInfoToRange :: SpanInfo -> Range
spanInfoToRange (SpanInfo sl sc el ec) = mkRange
  (fromIntegral sl) (fromIntegral sc)
  (fromIntegral el) (fromIntegral ec)