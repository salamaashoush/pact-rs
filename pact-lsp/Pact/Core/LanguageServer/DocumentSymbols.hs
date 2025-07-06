{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Pact.Core.LanguageServer.DocumentSymbols
  ( documentSymbolHandler
  ) where

import Control.Lens hiding (List)
import Control.Monad.Trans (lift)
import Control.Monad.IO.Class
import Control.Monad.Reader (ask)
import Control.Concurrent.MVar
import Data.Text (Text)
import Language.LSP.Server
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types as LSP
import qualified Language.LSP.Protocol.Lens as LSPLens

import Pact.Core.Names hiding (argType)
import Pact.Core.Info
import Pact.Core.IR.Term
import Pact.Core.Builtin
import Pact.Core.LanguageServer.Types
import Pact.Core.Pretty (renderText)

-- | Handle document symbol requests (outline view)
documentSymbolHandler :: Handlers LSM
documentSymbolHandler = requestHandler SMethod_TextDocumentDocumentSymbol $ \req resp -> 
  getState >>= \st -> do
    let uri' = req ^. LSPLens.params . LSPLens.textDocument . LSPLens.uri
        nuri = LSP.toNormalizedUri uri'
    
    case view (lsTopLevel . at nuri) st of
      Nothing -> resp $ Right $ LSP.InL []
      Just topLevels -> do
        let symbols = concatMap (topLevelToSymbols uri') topLevels
        resp $ Right $ LSP.InL symbols

-- | Convert a top-level item to symbol information
topLevelToSymbols :: LSP.Uri -> EvalTopLevel ReplCoreBuiltin FileLocSpanInfo -> [LSP.SymbolInformation]
topLevelToSymbols uri = \case
  TLModule m -> [moduleToSymbol uri m] ++ map (defToSymbol uri) (_mDefs m)
  TLInterface iface -> [interfaceToSymbol uri iface] ++ map (ifDefToSymbol uri) (_ifDefns iface)
  TLTerm _ -> []  -- Top-level terms don't need symbols
  TLUse _ _ -> []  -- Use statements don't need symbols

-- | Convert a module to symbol information
moduleToSymbol :: LSP.Uri -> EvalModule ReplCoreBuiltin FileLocSpanInfo -> LSP.SymbolInformation
moduleToSymbol uri m = 
  LSP.SymbolInformation 
    (renderModuleName $ _mName m)
    LSP.SymbolKind_Module
    Nothing
    Nothing
    Nothing
    (LSP.Location uri (spanInfoToRange (view spanInfo $ _mInfo m)))

-- | Convert an interface to symbol information
interfaceToSymbol :: LSP.Uri -> EvalInterface ReplCoreBuiltin FileLocSpanInfo -> LSP.SymbolInformation
interfaceToSymbol uri iface = 
  LSP.SymbolInformation
    (renderModuleName $ _ifName iface)
    LSP.SymbolKind_Interface
    Nothing
    Nothing
    Nothing
    (LSP.Location uri (spanInfoToRange (view spanInfo $ _ifInfo iface)))

-- | Convert a definition to symbol information
defToSymbol :: LSP.Uri -> EvalDef ReplCoreBuiltin FileLocSpanInfo -> LSP.SymbolInformation
defToSymbol uri = \case
  Dfun (Defun spec _ _ i) -> 
    LSP.SymbolInformation
      (renderArgName spec)
      LSP.SymbolKind_Function
      Nothing
      (renderModuleName <$> getContainerModule spec)
      Nothing
      (LSP.Location uri (spanInfoToRange (view spanInfo i)))
  DConst (DefConst spec _ i) -> 
    LSP.SymbolInformation
      (renderArgName spec)
      LSP.SymbolKind_Constant
      Nothing
      (renderModuleName <$> getContainerModule spec)
      Nothing
      (LSP.Location uri (spanInfoToRange (view spanInfo i)))
  DTable (DefTable tn _ i) -> 
    LSP.SymbolInformation
      (renderText tn)
      LSP.SymbolKind_Variable
      Nothing
      Nothing
      Nothing
      (LSP.Location uri (spanInfoToRange (view spanInfo i)))
  DPact (DefPact spec _ _ i) -> 
    LSP.SymbolInformation
      (renderArgName spec)
      LSP.SymbolKind_Function
      Nothing
      (renderModuleName <$> getContainerModule spec)
      Nothing
      (LSP.Location uri (spanInfoToRange (view spanInfo i)))
  DCap (DefCap spec _ _ _ i) -> 
    LSP.SymbolInformation
      (renderArgName spec)
      LSP.SymbolKind_Method
      Nothing
      (renderModuleName <$> getContainerModule spec)
      Nothing
      (LSP.Location uri (spanInfoToRange (view spanInfo i)))
  DSchema (DefSchema schemaName _ i) -> 
    LSP.SymbolInformation
      (renderText schemaName)
      LSP.SymbolKind_Struct
      Nothing
      Nothing
      Nothing
      (LSP.Location uri (spanInfoToRange (view spanInfo i)))

-- | Convert interface definitions to symbol information  
ifDefToSymbol :: LSP.Uri -> EvalIfDef ReplCoreBuiltin FileLocSpanInfo -> LSP.SymbolInformation
ifDefToSymbol uri = \case
  IfDfun (IfDefun n _ i) -> 
    LSP.SymbolInformation
      (renderText n)
      LSP.SymbolKind_Method
      Nothing
      Nothing
      Nothing
      (LSP.Location uri (spanInfoToRange (view spanInfo i)))
  IfDConst (DefConst spec _ i) -> 
    LSP.SymbolInformation
      (renderArgName spec)
      LSP.SymbolKind_Constant
      Nothing
      Nothing
      Nothing
      (LSP.Location uri (spanInfoToRange (view spanInfo i)))
  IfDCap (IfDefCap n _ _ i) -> 
    LSP.SymbolInformation
      (renderText n)
      LSP.SymbolKind_Method
      Nothing
      Nothing
      Nothing
      (LSP.Location uri (spanInfoToRange (view spanInfo i)))
  IfDPact (IfDefPact n _ i) -> 
    LSP.SymbolInformation
      (renderText n)
      LSP.SymbolKind_Method
      Nothing
      Nothing
      Nothing
      (LSP.Location uri (spanInfoToRange (view spanInfo i)))
  IfDSchema (DefSchema sn _ i) -> 
    LSP.SymbolInformation
      (renderText sn)
      LSP.SymbolKind_Struct
      Nothing
      Nothing
      Nothing
      (LSP.Location uri (spanInfoToRange (view spanInfo i)))

-- | Helper to get container module (if available)
getContainerModule :: Arg ty info -> Maybe ModuleName
getContainerModule _ = Nothing  -- Simplified for now

-- | Render argument name
renderArgName :: Arg ty info -> Text
renderArgName (Arg n _ _) = renderText n

-- | Get LSState
getState :: LSM LSState
getState = lift ask >>= liftIO . readMVar

-- | Convert SpanInfo to Range
spanInfoToRange :: SpanInfo -> Range
spanInfoToRange (SpanInfo sl sc el ec) = LSP.mkRange
  (fromIntegral sl) (fromIntegral sc)
  (fromIntegral el) (fromIntegral ec)