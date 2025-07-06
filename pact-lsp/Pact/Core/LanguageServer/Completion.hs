{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

module Pact.Core.LanguageServer.Completion
  ( completionHandler
  , completionItemResolveHandler
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
import Language.LSP.Server
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
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
import Pact.Core.Environment.Types

-- | Main completion handler
completionHandler :: Handlers LSM
completionHandler = requestHandler SMethod_TextDocumentCompletion $ \req resp ->
  getState >>= \st -> do
    let params' = req ^. LSP.params
        uri' = params' ^. LSP.textDocument . LSP.uri
        nuri = toNormalizedUri uri'
        pos = params' ^. LSP.position
        -- Check if completion was triggered by specific characters
        mTrigger = params' ^? LSP.context . _Just . LSP.triggerCharacter . _Just
    
    -- Get virtual file to access current text
    mvf <- getVirtualFile nuri
    case mvf of
      Nothing -> resp $ Right $ InL []
      Just vf -> do
        let content = virtualFileText vf
            lineText = getLineAt content pos
            (prefix, beforeCursor) = getPrefixAndContext lineText pos
        
        let topLevels = fromMaybe [] $ preview (lsTopLevel . at nuri . _Just) st
            replState = preview (lsReplState . at nuri . _Just) st
        
        completionItems <- generateCompletions mTrigger prefix beforeCursor topLevels replState
        
        resp $ Right $ InL completionItems

-- | Completion item resolve handler (for lazy loading of documentation)
completionItemResolveHandler :: Handlers LSM
completionItemResolveHandler = requestHandler SMethod_CompletionItemResolve $ \req resp -> do
  let compItem = req ^. LSP.params
  -- Enrich the completion item with full documentation if needed
  enrichedItem <- resolveCompletionItem compItem
  resp $ Right enrichedItem

-- | Generate completion items based on context
generateCompletions 
  :: Maybe Text -- ^ Trigger character
  -> Text -- ^ Prefix to match
  -> Text -- ^ Text before cursor
  -> [EvalTopLevel ReplCoreBuiltin FileLocSpanInfo] -- ^ Top level definitions
  -> Maybe (ReplState ReplCoreBuiltin) -- ^ REPL state
  -> LSM [CompletionItem]
generateCompletions mTrigger prefix beforeCursor topLevels _replState = do
  let ctx = analyzeContext beforeCursor mTrigger
  
  completions <- case ctx of
    KeywordContext -> pure $ keywordCompletions prefix
    BuiltinContext -> pure $ builtinCompletions prefix
    ModuleContext modName -> moduleCompletions modName prefix topLevels
    GeneralContext -> do
      -- Combine all completion sources
      let kws = if mTrigger == Just "(" then [] else keywordCompletions prefix
          bis = builtinCompletions prefix
          uds = userDefinedCompletions prefix topLevels
          mods = moduleNameCompletions prefix topLevels
          -- Deduplicate by label
          allCompletions = kws ++ bis ++ uds ++ mods
          deduped = M.elems $ M.fromList [(compItem ^. LSP.label, compItem) | compItem <- allCompletions]
      pure deduped
  
  -- Sort by relevance and limit results
  let sorted = sortCompletions prefix completions
  pure $ take 100 sorted

-- | Sort completions by relevance
sortCompletions :: Text -> [CompletionItem] -> [CompletionItem]
sortCompletions prefix items = 
  let prefixLower = T.toLower prefix
      scoredItems = [(scoreCompletion prefixLower item, item) | item <- items]
      sorted = L.sortOn fst scoredItems
  in map snd sorted
  where
    scoreCompletion :: Text -> CompletionItem -> (Int, Text)
    scoreCompletion pref item = 
      let label = item ^. LSP.label
          labelLower = T.toLower label
      in if T.null pref
         then (0, labelLower)  -- No prefix, alphabetical order
         else if labelLower == pref
         then (-3, labelLower) -- Exact match
         else if T.isPrefixOf pref labelLower
         then (-2 + T.length label, labelLower) -- Prefix match (shorter is better)
         else if pref `T.isInfixOf` labelLower
         then (-1 + T.length label, labelLower) -- Contains match
         else (999, labelLower) -- No match (shouldn't happen due to filtering)

-- | Analyze context to determine what completions to offer
data PactCompletionContext
  = KeywordContext
  | BuiltinContext
  | ModuleContext ModuleName
  | GeneralContext
  deriving (Show, Eq)

analyzeContext :: Text -> Maybe Text -> PactCompletionContext
analyzeContext beforeCursor mTrigger = case mTrigger of
  Just "." -> 
    -- Module member access
    case extractModuleName beforeCursor of
      Just modName -> ModuleContext modName
      Nothing -> GeneralContext
  Just "(" ->
    -- After opening paren, show all functions and builtins
    GeneralContext
  Just " " ->
    -- After space, context-sensitive
    if isInKeywordPosition beforeCursor
    then KeywordContext
    else GeneralContext
  _ -> 
    -- Check if we're in a specific context
    if isInKeywordPosition beforeCursor
    then KeywordContext
    else GeneralContext

-- | Extract module name from text like "coin." or "free.my-module."
extractModuleName :: Text -> Maybe ModuleName
extractModuleName t = 
  case T.words t of
    [] -> Nothing
    ws -> 
      let lastWord = last ws
          splitParts = T.split (== '.') lastWord
      in case splitParts of
        [ns, modName, ""] -> Just $ ModuleName modName (Just $ NamespaceName ns)
        [modName, ""] -> Just $ ModuleName modName Nothing
        _ -> Nothing

-- | Check if we're in a position where keywords are expected
isInKeywordPosition :: Text -> Bool
isInKeywordPosition t = 
  let trimmed = T.strip t
  in T.null trimmed || T.last trimmed == '('

-- | Generate keyword completions
keywordCompletions :: Text -> [CompletionItem]
keywordCompletions prefix = 
  let keywords = 
        [ ("module", "Define a module", CompletionItemKind_Keyword)
        , ("interface", "Define an interface", CompletionItemKind_Interface)
        , ("defun", "Define a function", CompletionItemKind_Function)
        , ("defconst", "Define a constant", CompletionItemKind_Constant)
        , ("defcap", "Define a capability", CompletionItemKind_Function)
        , ("defpact", "Define a pact", CompletionItemKind_Function)
        , ("defschema", "Define a schema", CompletionItemKind_Struct)
        , ("deftable", "Define a table", CompletionItemKind_Variable)
        , ("let", "Create local bindings", CompletionItemKind_Keyword)
        , ("let*", "Create sequential local bindings", CompletionItemKind_Keyword)
        , ("if", "Conditional expression", CompletionItemKind_Keyword)
        , ("enforce", "Enforce a condition", CompletionItemKind_Keyword)
        , ("enforce-one", "Enforce one of multiple conditions", CompletionItemKind_Keyword)
        , ("with-capability", "Execute with capability", CompletionItemKind_Keyword)
        , ("compose-capability", "Compose capabilities", CompletionItemKind_Keyword)
        , ("install-capability", "Install capability", CompletionItemKind_Keyword)
        , ("emit-event", "Emit an event", CompletionItemKind_Keyword)
        , ("create-table", "Create a table", CompletionItemKind_Keyword)
        , ("with-read", "Read from table with binding", CompletionItemKind_Keyword)
        , ("select", "Select from table", CompletionItemKind_Keyword)
        , ("where", "Filter clause", CompletionItemKind_Keyword)
        , ("and", "Logical AND", CompletionItemKind_Operator)
        , ("or", "Logical OR", CompletionItemKind_Operator)
        , ("not", "Logical NOT", CompletionItemKind_Operator)
        , ("true", "Boolean true", CompletionItemKind_Value)
        , ("false", "Boolean false", CompletionItemKind_Value)
        ]
  in [ makeKeywordCompletion label desc itemKind
     | (label, desc, itemKind) <- keywords
     , T.isPrefixOf (T.toLower prefix) (T.toLower label)
     ]

-- | Generate builtin function completions
builtinCompletions :: Text -> [CompletionItem]
builtinCompletions prefix = 
  [ makeBuiltinCompletion bName docs
  | (bName, docs) <- M.toList builtinDocs
  , T.isPrefixOf (T.toLower prefix) (T.toLower bName)
  ]

-- | Generate user-defined completions from top-level definitions
userDefinedCompletions :: Text -> [EvalTopLevel ReplCoreBuiltin FileLocSpanInfo] -> [CompletionItem]
userDefinedCompletions prefix topLevels = 
  concatMap (completionsFromTopLevel prefix) topLevels

-- | Extract completions from a top-level definition
completionsFromTopLevel :: Text -> EvalTopLevel ReplCoreBuiltin FileLocSpanInfo -> [CompletionItem]
completionsFromTopLevel prefix = \case
  TLModule m -> completionsFromModule prefix m
  TLInterface i -> completionsFromInterface prefix i
  _ -> []

-- | Extract completions from a module
completionsFromModule :: Text -> EvalModule ReplCoreBuiltin FileLocSpanInfo -> [CompletionItem]
completionsFromModule prefix (Module _ _ defs _ _ _ _ _ _ _) = 
  concatMap (completionFromDef prefix) defs

-- | Extract completions from an interface
completionsFromInterface :: Text -> EvalInterface ReplCoreBuiltin FileLocSpanInfo -> [CompletionItem]
completionsFromInterface prefix (Interface _ idefs _ _ _ _ _) = 
  mapMaybe (completionFromIfDef prefix) idefs

-- | Create completion from a definition
completionFromDef :: Text -> EvalDef ReplCoreBuiltin FileLocSpanInfo -> [CompletionItem]
completionFromDef prefix = \case
  Dfun (Defun spec args _ _) -> 
    let fName = _argName spec
    in [makeUserFunctionCompletion fName args | T.isPrefixOf (T.toLower prefix) (T.toLower fName)]
  DConst (DefConst spec _ _) ->
    let cName = _argName spec
    in [makeConstantCompletion cName | T.isPrefixOf (T.toLower prefix) (T.toLower cName)]
  DCap (DefCap spec args _ _ _) ->
    let capName = _argName spec
    in [makeCapabilityCompletion capName args | T.isPrefixOf (T.toLower prefix) (T.toLower capName)]
  DSchema (DefSchema sName _ _) ->
    [makeSchemaCompletion sName | T.isPrefixOf (T.toLower prefix) (T.toLower sName)]
  DTable (DefTable tName _ _) ->
    [makeTableCompletion tName | T.isPrefixOf (T.toLower prefix) (T.toLower tName)]
  DPact (DefPact spec args _ _) ->
    let pName = _argName spec
    in [makePactCompletion pName args | T.isPrefixOf (T.toLower prefix) (T.toLower pName)]

-- | Create completion from interface definition
completionFromIfDef :: Text -> EvalIfDef ReplCoreBuiltin FileLocSpanInfo -> Maybe CompletionItem
completionFromIfDef prefix = \case
  IfDfun (IfDefun spec args _) ->
    let ifFuncName = _argName spec
    in if T.isPrefixOf (T.toLower prefix) (T.toLower ifFuncName)
       then Just $ makeUserFunctionCompletion ifFuncName args
       else Nothing
  IfDConst (DefConst spec _ _) ->
    let icName = _argName spec
    in if T.isPrefixOf (T.toLower prefix) (T.toLower icName)
       then Just $ makeConstantCompletion icName
       else Nothing
  IfDCap (IfDefCap spec args _ _) ->
    let icapName = _argName spec  
    in if T.isPrefixOf (T.toLower prefix) (T.toLower icapName)
       then Just $ makeCapabilityCompletion icapName args
       else Nothing
  IfDSchema (DefSchema isName _ _) ->
    if T.isPrefixOf (T.toLower prefix) (T.toLower isName)
    then Just $ makeSchemaCompletion isName
    else Nothing
  IfDPact (IfDefPact spec args _) ->
    let ipName = _argName spec
    in if T.isPrefixOf (T.toLower prefix) (T.toLower ipName)
       then Just $ makePactCompletion ipName args
       else Nothing

-- | Module name completions (for completing module names themselves)
moduleNameCompletions :: Text -> [EvalTopLevel ReplCoreBuiltin FileLocSpanInfo] -> [CompletionItem]
moduleNameCompletions prefix topLevels = 
  [ makeModuleCompletion modName
  | modName <- getAvailableModules topLevels
  , T.isPrefixOf (T.toLower prefix) (T.toLower modName)
  ]

-- | Module member completions
moduleCompletions :: ModuleName -> Text -> [EvalTopLevel ReplCoreBuiltin FileLocSpanInfo] -> LSM [CompletionItem]
moduleCompletions modName prefix topLevels = do
  -- Find the module
  case findModule modName topLevels of
    Just m -> pure $ completionsFromModule prefix m
    Nothing -> pure []

findModule :: ModuleName -> [EvalTopLevel ReplCoreBuiltin FileLocSpanInfo] -> Maybe (EvalModule ReplCoreBuiltin FileLocSpanInfo)
findModule modName topLevels = 
  listToMaybe
    [ m | TLModule m@(Module mname _ _ _ _ _ _ _ _ _) <- topLevels
    , mname == modName
    ]

-- | Helper functions to create completion items
makeKeywordCompletion :: Text -> Text -> CompletionItemKind -> CompletionItem
makeKeywordCompletion label' desc itemKind = CompletionItem
  { _label = label'
  , _labelDetails = Nothing
  , _kind = Just itemKind
  , _tags = Nothing
  , _detail = Just desc
  , _documentation = Nothing
  , _deprecated = Nothing
  , _preselect = Nothing
  , _sortText = Nothing
  , _filterText = Nothing
  , _insertText = Nothing
  , _insertTextFormat = Nothing
  , _insertTextMode = Nothing
  , _textEdit = Nothing
  , _textEditText = Nothing
  , _additionalTextEdits = Nothing
  , _commitCharacters = Nothing
  , _command = Nothing
  , _data_ = Nothing
  }

makeBuiltinCompletion :: Text -> MarkdownDoc -> CompletionItem
makeBuiltinCompletion funcName (MarkdownDoc docs) = CompletionItem
  { _label = funcName
  , _labelDetails = Nothing
  , _kind = Just CompletionItemKind_Function
  , _tags = Nothing
  , _detail = Just $ "builtin: " <> extractSignature docs
  , _documentation = Just $ InL docs
  , _deprecated = Nothing
  , _preselect = Nothing
  , _sortText = Nothing
  , _filterText = Nothing
  , _insertText = Just $ funcName <> " $0"
  , _insertTextFormat = Just InsertTextFormat_Snippet
  , _insertTextMode = Nothing
  , _textEdit = Nothing
  , _textEditText = Nothing
  , _additionalTextEdits = Nothing
  , _commitCharacters = Nothing
  , _command = Nothing
  , _data_ = Nothing
  }
  where
    -- Extract function signature from docs if available
    extractSignature :: Text -> Text
    extractSignature doc = 
      case T.lines doc of
        (sig:_) | "(" `T.isPrefixOf` sig -> T.takeWhile (/= '\n') sig
        _ -> funcName

makeUserFunctionCompletion :: Text -> [Arg Type FileLocSpanInfo] -> CompletionItem
makeUserFunctionCompletion funcName args = CompletionItem
  { _label = funcName
  , _labelDetails = Nothing
  , _kind = Just CompletionItemKind_Function
  , _tags = Nothing
  , _detail = Just $ "(" <> T.intercalate " " (map _argName args) <> ")"
  , _documentation = Nothing
  , _deprecated = Nothing
  , _preselect = Nothing
  , _sortText = Nothing
  , _filterText = Nothing
  , _insertText = Just $ funcName <> " $0"
  , _insertTextFormat = Just InsertTextFormat_Snippet
  , _insertTextMode = Nothing
  , _textEdit = Nothing
  , _textEditText = Nothing
  , _additionalTextEdits = Nothing
  , _commitCharacters = Nothing
  , _command = Nothing
  , _data_ = Nothing
  }

makeConstantCompletion :: Text -> CompletionItem
makeConstantCompletion constName = CompletionItem
  { _label = constName
  , _labelDetails = Nothing
  , _kind = Just CompletionItemKind_Constant
  , _tags = Nothing
  , _detail = Just "constant"
  , _documentation = Nothing
  , _deprecated = Nothing
  , _preselect = Nothing
  , _sortText = Nothing
  , _filterText = Nothing
  , _insertText = Nothing
  , _insertTextFormat = Nothing
  , _insertTextMode = Nothing
  , _textEdit = Nothing
  , _textEditText = Nothing
  , _additionalTextEdits = Nothing
  , _commitCharacters = Nothing
  , _command = Nothing
  , _data_ = Nothing
  }

makeCapabilityCompletion :: Text -> [Arg Type FileLocSpanInfo] -> CompletionItem
makeCapabilityCompletion capName args = CompletionItem
  { _label = capName
  , _labelDetails = Nothing
  , _kind = Just CompletionItemKind_Function
  , _tags = Nothing
  , _detail = Just $ "capability (" <> T.intercalate " " (map _argName args) <> ")"
  , _documentation = Nothing
  , _deprecated = Nothing
  , _preselect = Nothing
  , _sortText = Nothing
  , _filterText = Nothing
  , _insertText = Just $ capName <> " $0"
  , _insertTextFormat = Just InsertTextFormat_Snippet
  , _insertTextMode = Nothing
  , _textEdit = Nothing
  , _textEditText = Nothing
  , _additionalTextEdits = Nothing
  , _commitCharacters = Nothing
  , _command = Nothing
  , _data_ = Nothing
  }

makeSchemaCompletion :: Text -> CompletionItem
makeSchemaCompletion schemaName = CompletionItem
  { _label = schemaName
  , _labelDetails = Nothing
  , _kind = Just CompletionItemKind_Struct
  , _tags = Nothing
  , _detail = Just "schema"
  , _documentation = Nothing
  , _deprecated = Nothing
  , _preselect = Nothing
  , _sortText = Nothing
  , _filterText = Nothing
  , _insertText = Nothing
  , _insertTextFormat = Nothing
  , _insertTextMode = Nothing
  , _textEdit = Nothing
  , _textEditText = Nothing
  , _additionalTextEdits = Nothing
  , _commitCharacters = Nothing
  , _command = Nothing
  , _data_ = Nothing
  }

makeTableCompletion :: Text -> CompletionItem
makeTableCompletion tblName = CompletionItem
  { _label = tblName
  , _labelDetails = Nothing
  , _kind = Just CompletionItemKind_Variable
  , _tags = Nothing
  , _detail = Just "table"
  , _documentation = Nothing
  , _deprecated = Nothing
  , _preselect = Nothing
  , _sortText = Nothing
  , _filterText = Nothing
  , _insertText = Nothing
  , _insertTextFormat = Nothing
  , _insertTextMode = Nothing
  , _textEdit = Nothing
  , _textEditText = Nothing
  , _additionalTextEdits = Nothing
  , _commitCharacters = Nothing
  , _command = Nothing
  , _data_ = Nothing
  }

makeModuleCompletion :: Text -> CompletionItem
makeModuleCompletion modName = CompletionItem
  { _label = modName
  , _labelDetails = Nothing
  , _kind = Just CompletionItemKind_Module
  , _tags = Nothing
  , _detail = Just "module"
  , _documentation = Nothing
  , _deprecated = Nothing
  , _preselect = Nothing
  , _sortText = Nothing
  , _filterText = Nothing
  , _insertText = Just $ modName <> "."
  , _insertTextFormat = Nothing
  , _insertTextMode = Nothing
  , _textEdit = Nothing
  , _textEditText = Nothing
  , _additionalTextEdits = Nothing
  , _commitCharacters = Just ["."]
  , _command = Nothing
  , _data_ = Nothing
  }

makePactCompletion :: Text -> [Arg Type FileLocSpanInfo] -> CompletionItem
makePactCompletion pactName args = CompletionItem
  { _label = pactName
  , _labelDetails = Nothing
  , _kind = Just CompletionItemKind_Function
  , _tags = Nothing
  , _detail = Just $ "pact (" <> T.intercalate " " (map _argName args) <> ")"
  , _documentation = Nothing
  , _deprecated = Nothing
  , _preselect = Nothing
  , _sortText = Nothing
  , _filterText = Nothing
  , _insertText = Just $ pactName <> " $0"
  , _insertTextFormat = Just InsertTextFormat_Snippet
  , _insertTextMode = Nothing
  , _textEdit = Nothing
  , _textEditText = Nothing
  , _additionalTextEdits = Nothing
  , _commitCharacters = Nothing
  , _command = Nothing
  , _data_ = Nothing
  }

-- | Resolve a completion item with additional information
resolveCompletionItem :: CompletionItem -> LSM CompletionItem
resolveCompletionItem compItem = do
  -- For now, just return the item as-is
  -- In the future, we can lazy-load documentation here
  pure compItem

-- | Get the line at the given position
getLineAt :: Text -> Position -> Text
getLineAt content (Position lineNum _) = 
  let lines' = T.lines content
  in if fromIntegral lineNum < L.length lines'
     then lines' !! fromIntegral lineNum
     else ""

-- | Get prefix and context before cursor
getPrefixAndContext :: Text -> Position -> (Text, Text)
getPrefixAndContext lineText (Position _ col) = 
  let beforeCursor = T.take (fromIntegral col) lineText
      -- Extract the current word being typed
      prefix = T.takeWhileEnd isIdentChar beforeCursor
  in (prefix, beforeCursor)
  where
    isIdentChar c = c `elem` ("-_" :: String) || 
                   (c >= 'a' && c <= 'z') || 
                   (c >= 'A' && c <= 'Z') ||
                   (c >= '0' && c <= '9')

-- | Get available module names for completion
getAvailableModules :: [EvalTopLevel ReplCoreBuiltin FileLocSpanInfo] -> [Text]
getAvailableModules topLevels = L.nub
  [ renderModName mname
  | TLModule (Module mname _ _ _ _ _ _ _ _ _) <- topLevels
  ]
  where
    renderModName :: ModuleName -> Text
    renderModName (ModuleName n mns) = 
      case mns of
        Nothing -> n
        Just (NamespaceName ns) -> ns <> "." <> n

-- | Get LSState
getState :: LSM LSState
getState = lift ask >>= liftIO . readMVar