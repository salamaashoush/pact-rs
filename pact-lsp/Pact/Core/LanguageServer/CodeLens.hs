{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Pact.Core.LanguageServer.CodeLens
  ( codeLensHandler
  ) where

import Control.Lens hiding (List)
import Control.Monad.Trans (lift)
import Control.Monad.IO.Class
import Control.Monad.Reader (ask)
import Control.Concurrent.MVar
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (catMaybes, fromMaybe)
import Language.LSP.Server
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types as LSP hiding (maybeToNull)
import qualified Language.LSP.Protocol.Lens as LSP
import Data.Aeson (toJSON)

import Pact.Core.Names
import Pact.Core.Info
import Pact.Core.Type
import Pact.Core.IR.Term
import Pact.Core.Builtin
import Pact.Core.LanguageServer.Types
import Pact.Core.LanguageServer.Utils
import Pact.Core.Pretty (renderText)
import Pact.Core.Guards
import qualified Data.Map.Strict as M
import Data.List.NonEmpty (toList)

-- | Handle code lens requests
codeLensHandler :: Handlers LSM
codeLensHandler = requestHandler SMethod_TextDocumentCodeLens $ \req resp -> 
  getState >>= \st -> do
    let uri' = req ^. LSP.params . LSP.textDocument . LSP.uri
        nuri = toNormalizedUri uri'
    
    case view (lsTopLevel . at nuri) st of
      Nothing -> resp $ Right $ InL []
      Just topLevels -> do
        let lenses = concatMap (topLevelToLenses uri') topLevels
        resp $ Right $ InL lenses

-- | Convert top-level items to code lenses
topLevelToLenses :: Uri -> EvalTopLevel ReplCoreBuiltin FileLocSpanInfo -> [CodeLens]
topLevelToLenses uri = \case
  TLModule m -> moduleToLenses uri m
  TLInterface iface -> interfaceToLenses uri iface
  TLTerm _ -> []  -- No lenses for top-level terms
  TLUse _ _ -> []

-- | Convert module to code lenses
moduleToLenses :: Uri -> EvalModule ReplCoreBuiltin FileLocSpanInfo -> [CodeLens]
moduleToLenses uri m =
  let modLens = moduleInfoLens uri (_mName m) (governanceToFlag $ _mGovernance m) (_mInfo m)
      defLenses = concatMap (defToLenses uri) (_mDefs m)
  in catMaybes [modLens] ++ defLenses

-- | Convert interface to code lenses
interfaceToLenses :: Uri -> EvalInterface ReplCoreBuiltin FileLocSpanInfo -> [CodeLens]
interfaceToLenses uri iface =
  let ifaceLens = interfaceInfoLens uri (_ifName iface) (_ifInfo iface)
      defLenses = concatMap (ifDefToLenses uri) (_ifDefns iface)
  in catMaybes [ifaceLens] ++ defLenses

-- | Create module info lens
moduleInfoLens :: Uri -> ModuleName -> Bool -> FileLocSpanInfo -> Maybe CodeLens
moduleInfoLens uri mn isGoverned i =
  let range = spanInfoToRange (view spanInfo i)
      title = "Module: " <> renderModuleName mn <> if isGoverned then " (governed)" else ""
      command = Command
        title
        "pact.showModuleInfo"
        (Just [toJSON $ renderModuleName mn])
  in Just $ CodeLens range (Just command) Nothing

-- | Create interface info lens
interfaceInfoLens :: Uri -> ModuleName -> FileLocSpanInfo -> Maybe CodeLens
interfaceInfoLens uri ifn i =
  let range = spanInfoToRange (view spanInfo i)
      title = "Interface: " <> renderModuleName ifn
      command = Command
        title
        "pact.showInterfaceInfo"
        (Just [toJSON $ renderModuleName ifn])
  in Just $ CodeLens range (Just command) Nothing

-- | Convert definitions to code lenses
defToLenses :: Uri -> EvalDef ReplCoreBuiltin FileLocSpanInfo -> [CodeLens]
defToLenses uri = \case
  Dfun (Defun spec args _ i) -> 
    catMaybes
      [ runFunctionLens uri (renderArgName spec) i
      , testFunctionLens uri (renderArgName spec) i
      , typeSignatureLens uri spec args i
      ]
  DConst (DefConst spec _ i) -> 
    catMaybes [typeSignatureLens uri spec [] i]
  DTable (DefTable tn schema i) -> 
    case schema of
      ResolvedTable (Schema _ fields) -> 
        catMaybes [schemaInfoLens uri tn (Schema (QualifiedName tn (ModuleName "unknown" Nothing)) fields) i]
      _ -> []
  DPact (DefPact spec args steps i) -> 
    catMaybes
      [ runPactLens uri (renderArgName spec) i
      , pactStepsLens uri (renderArgName spec) (length $ toList steps) i
      ]
  DCap (DefCap spec args _ _ i) -> 
    catMaybes
      [ capabilityInfoLens uri (renderArgName spec) i
      , typeSignatureLens uri spec args i
      ]
  DSchema (DefSchema sn schema i) -> 
    catMaybes [schemaInfoLens uri (renderText sn) (Schema (QualifiedName sn (ModuleName "unknown" Nothing)) schema) i]

-- | Convert interface definitions to code lenses
ifDefToLenses :: Uri -> EvalIfDef ReplCoreBuiltin FileLocSpanInfo -> [CodeLens]
ifDefToLenses uri = \case
  IfDfun (IfDefun spec args i) -> 
    catMaybes [interfaceMethodLens uri (renderArgName spec) args i]
  IfDConst (DefConst spec _ i) -> 
    catMaybes [interfaceConstLens uri (renderArgName spec) (fromMaybe TyAny $ localArgType spec) i]
  IfDCap (IfDefCap spec args _ i) -> 
    catMaybes [interfaceCapLens uri (renderArgName spec) args (fromMaybe TyAny $ localArgType spec) i]
  IfDPact (IfDefPact spec args i) -> 
    catMaybes [interfacePactLens uri (renderArgName spec) args (fromMaybe TyAny $ localArgType spec) i]
  IfDSchema (DefSchema sn schema i) -> 
    catMaybes [schemaInfoLens uri (renderText sn) (Schema (QualifiedName sn (ModuleName "unknown" Nothing)) schema) i]

-- | Create run function lens
runFunctionLens :: Uri -> Text -> FileLocSpanInfo -> Maybe CodeLens
runFunctionLens uri fname i =
  let range = spanInfoToRange (view spanInfo i)
      title = "▶ Run " <> fname
      command = Command
        title
        "pact.runFunction"
        (Just [toJSON fname, toJSON uri])
  in Just $ CodeLens range (Just command) Nothing

-- | Create test function lens
testFunctionLens :: Uri -> Text -> FileLocSpanInfo -> Maybe CodeLens
testFunctionLens uri fname i =
  if "test" `T.isInfixOf` T.toLower fname
  then let range = spanInfoToRange (view spanInfo i)
           title = "🧪 Test " <> fname
           command = Command
             title
             "pact.testFunction"
             (Just [toJSON fname, toJSON uri])
       in Just $ CodeLens range (Just command) Nothing
  else Nothing

-- | Create type signature lens
typeSignatureLens :: Uri -> Arg Type FileLocSpanInfo -> [Arg Type FileLocSpanInfo] -> FileLocSpanInfo -> Maybe CodeLens
typeSignatureLens uri spec args i =
  let range = spanInfoToRange (view spanInfo i)
      argTypes = T.intercalate " → " $ map renderArgType args ++ [maybe "?" renderType (localArgType spec)]
      title = "📝 " <> argTypes
      command = Command
        title
        "pact.showTypeInfo"
        (Just [toJSON $ renderArgName spec])
  in Just $ CodeLens range (Just command) Nothing
  where
    renderArgType (Arg _ mty _) = maybe "_" renderType mty

-- | Create run pact lens
runPactLens :: Uri -> Text -> FileLocSpanInfo -> Maybe CodeLens
runPactLens uri pname i =
  let range = spanInfoToRange (view spanInfo i)
      title = "▶ Execute pact " <> pname
      command = Command
        title
        "pact.executePact"
        (Just [toJSON pname, toJSON uri])
  in Just $ CodeLens range (Just command) Nothing

-- | Create pact steps lens
pactStepsLens :: Uri -> Text -> Int -> FileLocSpanInfo -> Maybe CodeLens
pactStepsLens uri pname numSteps i =
  let range = spanInfoToRange (view spanInfo i)
      title = "📋 " <> T.pack (show numSteps) <> " steps"
      command = Command
        title
        "pact.showPactSteps"
        (Just [toJSON pname])
  in Just $ CodeLens range (Just command) Nothing

-- | Create capability info lens
capabilityInfoLens :: Uri -> Text -> FileLocSpanInfo -> Maybe CodeLens
capabilityInfoLens uri cname i =
  let range = spanInfoToRange (view spanInfo i)
      title = "🔐 Capability: " <> cname
      command = Command
        title
        "pact.showCapabilityInfo"
        (Just [toJSON cname])
  in Just $ CodeLens range (Just command) Nothing

-- | Create table schema lens
tableSchemaLens :: Uri -> TableName -> Type -> FileLocSpanInfo -> Maybe CodeLens
tableSchemaLens uri tn schema i =
  let range = spanInfoToRange (view spanInfo i)
      title = "📊 Schema: " <> renderType schema
      command = Command
        title
        "pact.showTableSchema"
        (Just [toJSON $ renderText tn, toJSON $ renderType schema])
  in Just $ CodeLens range (Just command) Nothing

-- | Create schema info lens
schemaInfoLens :: Uri -> Text -> Schema -> FileLocSpanInfo -> Maybe CodeLens
schemaInfoLens uri sname (Schema _ fields) i =
  let range = spanInfoToRange (view spanInfo i)
      fieldCount = M.size fields
      title = "📋 Schema " <> sname <> " (" <> T.pack (show fieldCount) <> " fields)"
      command = Command
        title
        "pact.showSchemaFields"
        (Just [toJSON sname])
  in Just $ CodeLens range (Just command) Nothing

-- | Create interface method lens
interfaceMethodLens :: Uri -> Text -> [Arg Type FileLocSpanInfo] -> FileLocSpanInfo -> Maybe CodeLens
interfaceMethodLens uri mname args i =
  let range = spanInfoToRange (view spanInfo i)
      title = "🔌 Interface method: " <> mname
      command = Command
        title
        "pact.showInterfaceMethod"
        (Just [toJSON mname])
  in Just $ CodeLens range (Just command) Nothing

-- | Create interface const lens
interfaceConstLens :: Uri -> Text -> Type -> FileLocSpanInfo -> Maybe CodeLens
interfaceConstLens uri cname ty i =
  let range = spanInfoToRange (view spanInfo i)
      title = "🔌 Interface const: " <> cname <> " : " <> renderType ty
      command = Command
        title
        "pact.showInterfaceConst"
        (Just [toJSON cname])
  in Just $ CodeLens range (Just command) Nothing

-- | Create interface capability lens
interfaceCapLens :: Uri -> Text -> [Arg Type FileLocSpanInfo] -> Type -> FileLocSpanInfo -> Maybe CodeLens
interfaceCapLens uri cname args ty i =
  let range = spanInfoToRange (view spanInfo i)
      title = "🔌🔐 Interface capability: " <> cname
      command = Command
        title
        "pact.showInterfaceCapability"
        (Just [toJSON cname])
  in Just $ CodeLens range (Just command) Nothing

-- | Create interface pact lens
interfacePactLens :: Uri -> Text -> [Arg Type FileLocSpanInfo] -> Type -> FileLocSpanInfo -> Maybe CodeLens
interfacePactLens uri pname args ty i =
  let range = spanInfoToRange (view spanInfo i)
      title = "🔌📜 Interface pact: " <> pname
      command = Command
        title
        "pact.showInterfacePact"
        (Just [toJSON pname])
  in Just $ CodeLens range (Just command) Nothing

-- | Render argument name
renderArgName :: Arg ty info -> Text
renderArgName (Arg n _ _) = renderText n

-- | Convert governance to a simple boolean flag
governanceToFlag :: Governance Name -> Bool
governanceToFlag = \case
  KeyGov _ -> False
  CapGov _ -> True

-- | Get argument type (avoiding name conflict with Pact.Core.Names.argType)
localArgType :: Arg ty info -> Maybe ty
localArgType (Arg _ mty _) = mty


-- | Get LSState
getState :: LSM LSState
getState = lift ask >>= liftIO . readMVar

-- | Convert SpanInfo to Range
spanInfoToRange :: SpanInfo -> Range
spanInfoToRange (SpanInfo sl sc el ec) = mkRange
  (fromIntegral sl) (fromIntegral sc)
  (fromIntegral el) (fromIntegral ec)