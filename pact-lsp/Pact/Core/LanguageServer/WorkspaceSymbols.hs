{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Pact.Core.LanguageServer.WorkspaceSymbols
  ( workspaceSymbolHandler
  ) where

import Control.Lens hiding (List)
import Control.Monad.Trans (lift)
import Control.Monad.IO.Class
import Control.Monad.Reader (ask)
import Control.Concurrent.MVar
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (mapMaybe)
import Language.LSP.Server
import Language.LSP.Protocol.Message
import qualified Language.LSP.Protocol.Types as LSP
import Data.Aeson (Value)
import qualified Language.LSP.Protocol.Lens as LSP

import Pact.Core.Names
import Pact.Core.Pretty (renderText)
import Pact.Core.Info
import Pact.Core.Type
import Pact.Core.IR.Term
import Pact.Core.Builtin
import Pact.Core.LanguageServer.Types
import Pact.Core.LanguageServer.Utils
import Pact.Core.Environment.Types
import Pact.Core.Hash
import qualified Data.Map.Strict as M

-- | Handle workspace symbol requests (search across all open files)
workspaceSymbolHandler :: Handlers LSM
workspaceSymbolHandler = requestHandler SMethod_WorkspaceSymbol $ \req resp -> 
  getState >>= \st -> do
    let query = req ^. LSP.params . LSP.query
        allTopLevels = M.toList $ view lsTopLevel st
        
    -- Search for symbols matching the query across all documents
    let symbols = concatMap (searchInDocument query) allTopLevels
    resp $ Right $ LSP.InL (map workspaceSymbolToSymbolInformation symbols)

-- | Search for symbols in a document matching the query
searchInDocument :: Text -> (LSP.NormalizedUri, [EvalTopLevel ReplCoreBuiltin FileLocSpanInfo]) -> [LSP.WorkspaceSymbol]
searchInDocument query (nuri, topLevels) = 
  let uri = LSP.fromNormalizedUri nuri
      matches = concatMap (topLevelToWorkspaceSymbols query uri) topLevels
  in matches

-- | Convert top-level items to workspace symbols if they match the query
topLevelToWorkspaceSymbols :: Text -> LSP.Uri -> EvalTopLevel ReplCoreBuiltin FileLocSpanInfo -> [LSP.WorkspaceSymbol]
topLevelToWorkspaceSymbols query uri = \case
  TLModule m -> moduleToWorkspaceSymbols query uri m
  TLInterface iface -> interfaceToWorkspaceSymbols query uri iface
  TLTerm _ -> []  -- Top-level terms don't contribute to workspace symbols
  TLUse _ _ -> []

-- | Check if a name matches the search query (case-insensitive substring match)
matchesQuery :: Text -> Text -> Bool
matchesQuery query name = 
  T.toLower query `T.isInfixOf` T.toLower name

-- | Convert module and its contents to workspace symbols
moduleToWorkspaceSymbols :: Text -> LSP.Uri -> EvalModule ReplCoreBuiltin FileLocSpanInfo -> [LSP.WorkspaceSymbol]
moduleToWorkspaceSymbols query uri (Module mn _ defs blessed _ _ _ _ _ i) =
  let modName = renderModuleName mn
      modSymbol = if matchesQuery query modName
                   then [moduleToWorkspaceSymbol uri mn i]
                   else []
      defSymbols = concatMap (defToWorkspaceSymbol query uri) defs
      blessedSymbols = []  -- Skip blessed hashes for now
  in modSymbol ++ defSymbols ++ blessedSymbols

-- | Convert interface and its contents to workspace symbols
interfaceToWorkspaceSymbols :: Text -> LSP.Uri -> EvalInterface ReplCoreBuiltin FileLocSpanInfo -> [LSP.WorkspaceSymbol]
interfaceToWorkspaceSymbols query uri (Interface ifn defs _ _ _ _ i) =
  let ifaceName = renderModuleName ifn
      ifaceSymbol = if matchesQuery query ifaceName
                     then [interfaceToWorkspaceSymbol uri ifn i]
                     else []
      defSymbols = concatMap (ifDefToWorkspaceSymbol query uri) defs
  in ifaceSymbol ++ defSymbols

-- | Create workspace symbol for a module
moduleToWorkspaceSymbol :: LSP.Uri -> ModuleName -> FileLocSpanInfo -> LSP.WorkspaceSymbol
moduleToWorkspaceSymbol uri mn i = LSP.WorkspaceSymbol
    (renderModuleName mn)
    LSP.SymbolKind_Module
    Nothing
    Nothing
    (LSP.InL $ LSP.Location uri (spanInfoToRange $ view spanInfo i))
    Nothing

-- | Create workspace symbol for an interface
interfaceToWorkspaceSymbol :: LSP.Uri -> ModuleName -> FileLocSpanInfo -> LSP.WorkspaceSymbol
interfaceToWorkspaceSymbol uri ifn i = LSP.WorkspaceSymbol
              (renderModuleName ifn)
              LSP.SymbolKind_Interface
              Nothing
              Nothing
              (LSP.InL $ LSP.Location uri (spanInfoToRange $ view spanInfo i))
              Nothing

-- | Convert definitions to workspace symbols
defToWorkspaceSymbol :: Text -> LSP.Uri -> EvalDef ReplCoreBuiltin FileLocSpanInfo -> [LSP.WorkspaceSymbol]
defToWorkspaceSymbol query uri = \case
  Dfun (Defun spec _ _ i) -> 
    let name = renderArgName spec
    in if matchesQuery query name
       then [LSP.WorkspaceSymbol
              name
              LSP.SymbolKind_Function
              Nothing
              Nothing
              (LSP.InL $ LSP.Location uri (spanInfoToRange $ view spanInfo i))
              Nothing]
       else []
  DConst (DefConst spec _ i) -> 
    let name = renderArgName spec
    in if matchesQuery query name
       then [LSP.WorkspaceSymbol
              name
              LSP.SymbolKind_Constant
              Nothing
              Nothing
              (LSP.InL $ LSP.Location uri (spanInfoToRange $ view spanInfo i))
              Nothing]
       else []
  DTable (DefTable tn _ i) -> 
    let name = renderText tn
    in if matchesQuery query name
       then [LSP.WorkspaceSymbol
              name
              LSP.SymbolKind_Variable
              Nothing
              Nothing
              (LSP.InL $ LSP.Location uri (spanInfoToRange $ view spanInfo i))
              Nothing]
       else []
  DPact (DefPact spec _ _ i) -> 
    let name = renderArgName spec
    in if matchesQuery query name
       then [LSP.WorkspaceSymbol
              name
              LSP.SymbolKind_Function
              Nothing
              Nothing
              (LSP.InL $ LSP.Location uri (spanInfoToRange $ view spanInfo i))
              Nothing]
       else []
  DCap (DefCap spec _ _ _ i) -> 
    let name = renderArgName spec
    in if matchesQuery query name
       then [LSP.WorkspaceSymbol
              name
              LSP.SymbolKind_Method
              Nothing
              Nothing
              (LSP.InL $ LSP.Location uri (spanInfoToRange $ view spanInfo i))
              Nothing]
       else []
  DSchema (DefSchema sn _ i) -> 
    let name = renderText sn
    in if matchesQuery query name
       then [LSP.WorkspaceSymbol
              name
              LSP.SymbolKind_Struct
              Nothing
              Nothing
              (LSP.InL $ LSP.Location uri (spanInfoToRange $ view spanInfo i))
              Nothing]
       else []

-- | Convert interface definitions to workspace symbols
ifDefToWorkspaceSymbol :: Text -> LSP.Uri -> EvalIfDef ReplCoreBuiltin FileLocSpanInfo -> [LSP.WorkspaceSymbol]
ifDefToWorkspaceSymbol query uri = \case
  IfDfun (IfDefun n _ i) -> 
    if matchesQuery query (renderText n)
    then [LSP.WorkspaceSymbol
           (renderText n)
           LSP.SymbolKind_Method
           Nothing
           Nothing
           (LSP.InL $ LSP.Location uri (spanInfoToRange $ view spanInfo i))
           Nothing]
    else []
  IfDConst (DefConst spec _ i) -> 
    if matchesQuery query (renderArgName spec)
    then [LSP.WorkspaceSymbol
           (renderArgName spec)
           LSP.SymbolKind_Constant
           Nothing
           Nothing
           (LSP.InL $ LSP.Location uri (spanInfoToRange $ view spanInfo i))
           Nothing]
    else []
  IfDCap (IfDefCap n _ _ i) -> 
    if matchesQuery query (renderText n)
    then [LSP.WorkspaceSymbol
           (renderText n)
           LSP.SymbolKind_Method
           Nothing
           Nothing
           (LSP.InL $ LSP.Location uri (spanInfoToRange $ view spanInfo i))
           Nothing]
    else []
  IfDPact (IfDefPact n _ i) -> 
    if matchesQuery query (renderText n)
    then [LSP.WorkspaceSymbol
           (renderText n)
           LSP.SymbolKind_Method
           Nothing
           Nothing
           (LSP.InL $ LSP.Location uri (spanInfoToRange $ view spanInfo i))
           Nothing]
    else []
  IfDSchema (DefSchema sn _ i) -> 
    if matchesQuery query (renderText sn)
    then [LSP.WorkspaceSymbol
           (renderText sn)
           LSP.SymbolKind_Struct
           Nothing
           Nothing
           (LSP.InL $ LSP.Location uri (spanInfoToRange $ view spanInfo i))
           Nothing]
    else []

-- | Create workspace symbol for a term
termToWorkspaceSymbol :: LSP.Uri -> Text -> Maybe Type -> FileLocSpanInfo -> LSP.WorkspaceSymbol
termToWorkspaceSymbol uri name _ i = LSP.WorkspaceSymbol
              name
              LSP.SymbolKind_Variable
              Nothing
              Nothing
              (LSP.InL $ LSP.Location uri (spanInfoToRange $ view spanInfo i))
              Nothing

-- | Convert blessed hashes to workspace symbols
blessedToWorkspaceSymbol :: Text -> LSP.Uri -> (Text, Hash) -> Maybe LSP.WorkspaceSymbol
blessedToWorkspaceSymbol query _ (modName, _) = 
  if matchesQuery query ("blessed:" <> modName)
  then Just $ LSP.WorkspaceSymbol
              ("blessed: " <> modName)
              LSP.SymbolKind_Key
              Nothing
              Nothing
              (LSP.InL $ LSP.Location (LSP.fromNormalizedUri $ LSP.toNormalizedUri $ LSP.Uri "") (LSP.mkRange 0 0 0 0))
              Nothing
  else Nothing

-- | Render argument name
renderArgName :: Arg ty info -> Text
renderArgName (Arg n _ _) = renderText n

-- | Get LSState
getState :: LSM LSState
getState = lift ask >>= liftIO . readMVar

-- | Convert WorkspaceSymbol to SymbolInformation
workspaceSymbolToSymbolInformation :: LSP.WorkspaceSymbol -> LSP.SymbolInformation
workspaceSymbolToSymbolInformation (LSP.WorkspaceSymbol name kind tags _deprecated location _containerName) =
  case location of
    LSP.InL loc -> LSP.SymbolInformation name kind tags Nothing Nothing loc
    LSP.InR _ -> LSP.SymbolInformation name kind tags Nothing Nothing (LSP.Location (LSP.Uri "") (LSP.mkRange 0 0 0 0))

-- | Convert SpanInfo to Range
spanInfoToRange :: SpanInfo -> LSP.Range
spanInfoToRange (SpanInfo sl sc el ec) = LSP.mkRange
  (fromIntegral sl) (fromIntegral sc)
  (fromIntegral el) (fromIntegral ec)