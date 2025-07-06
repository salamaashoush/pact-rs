{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pact.Core.LanguageServer.CodeActions
  ( codeActionHandler
  ) where

import Control.Lens hiding (List)
import Control.Monad.Trans (lift)
import Control.Monad.IO.Class
import Control.Monad.Reader (ask)
import Control.Concurrent.MVar
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (catMaybes)
import Language.LSP.Server
import Language.LSP.VFS
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types as LSP
import qualified Language.LSP.Protocol.Lens as LSPLens

import Pact.Core.Names hiding (argType)
import Pact.Core.Info
import Pact.Core.Type
import Pact.Core.IR.Term
import Pact.Core.Builtin
import Pact.Core.LanguageServer.Types
import Pact.Core.LanguageServer.Utils
import Pact.Core.Pretty (renderCompactText)

-- Import getMatch from References
import Pact.Core.LanguageServer.References (getMatch)

-- | Handle code action requests (quick fixes, refactorings)
codeActionHandler :: Handlers LSM
codeActionHandler = requestHandler SMethod_TextDocumentCodeAction $ \req resp -> do
  st <- getState
  let params' = req ^. LSPLens.params
      uri' = params' ^. LSPLens.textDocument . LSPLens.uri
      nuri = LSP.toNormalizedUri uri'
      range = params' ^. LSPLens.range
      context = params' ^. LSPLens.context
      diagnostics = context ^. LSPLens.diagnostics
  
  -- Get virtual file to access content
  mvf <- getVirtualFile nuri
  
  case mvf of
    Nothing -> resp $ Right $ LSP.InL []
    Just vf -> do
      let content = virtualFileText vf
      
      -- Generate code actions based on diagnostics
      diagnosticActions <- generateDiagnosticActions uri' diagnostics
      
      -- Generate code actions based on cursor position
      case view (lsTopLevel . at nuri) st of
        Nothing -> resp $ Right $ LSP.InL (map LSP.InR diagnosticActions)
        Just topLevels -> do
          -- Find what's at the cursor position
          let cursorActions = case getMatchInRange range topLevels of
                Nothing -> []
                Just posMatch -> generateCursorActions uri' content posMatch
          
          let actions = map LSP.InR (diagnosticActions ++ cursorActions)
          resp $ Right $ LSP.InL actions

-- | Get match within a range
getMatchInRange :: LSP.Range -> [EvalTopLevel ReplCoreBuiltin FileLocSpanInfo] -> Maybe (PositionMatch ReplCoreBuiltin FileLocSpanInfo)
getMatchInRange range topLevels = 
  let startPos = range ^. LSPLens.start
  in getMatch startPos topLevels

-- | Generate code actions for diagnostics
generateDiagnosticActions :: LSP.Uri -> [LSP.Diagnostic] -> LSM [LSP.CodeAction]
generateDiagnosticActions uri diagnostics = pure $ concatMap (diagnosticToActions uri) diagnostics

-- | Convert a diagnostic to code actions
diagnosticToActions :: LSP.Uri -> LSP.Diagnostic -> [LSP.CodeAction]
diagnosticToActions uri diag = 
  let msg = diag ^. LSPLens.message
      range = diag ^. LSPLens.range
  in catMaybes
     [ generateImportAction uri range msg
     , generateTypeAnnotationAction uri range msg
     , generateCapabilityAction uri range msg
     ]

-- | Generate import/use statement action
generateImportAction :: LSP.Uri -> LSP.Range -> Text -> Maybe LSP.CodeAction
generateImportAction uri _ msg
  | "not in scope" `T.isInfixOf` msg = 
      let moduleName = extractModuleName msg
          title = "Import module " <> moduleName
          textEdit = LSP.TextEdit (mkRange 0 0 0 0) ("(use " <> moduleName <> ")\n")
          workspaceEdit = LSP.WorkspaceEdit 
            Nothing
            (Just 
              [LSP.InL $ TextDocumentEdit
                (OptionalVersionedTextDocumentIdentifier uri (LSP.InR LSP.Null))
                [LSP.InL textEdit]])
            Nothing
          action = LSP.CodeAction
            title
            (Just LSP.CodeActionKind_QuickFix)
            Nothing  -- No diagnostics reference
            Nothing
            Nothing
            (Just workspaceEdit)
            Nothing
            Nothing
      in Just action
  | otherwise = Nothing

-- | Generate type annotation action
generateTypeAnnotationAction :: LSP.Uri -> Range -> Text -> Maybe LSP.CodeAction
generateTypeAnnotationAction uri range msg
  | "missing type annotation" `T.isInfixOf` msg = 
      let title = "Add type annotation"
          -- Create a placeholder type annotation
          placeholderType = ":string"
          endPos = range ^. LSPLens.end
          insertPos = Range endPos endPos
          textEdit = LSP.TextEdit insertPos placeholderType
          workspaceEdit = LSP.WorkspaceEdit Nothing 
                (Just [LSP.InL $ TextDocumentEdit
                  (OptionalVersionedTextDocumentIdentifier uri (LSP.InR LSP.Null))
                  [LSP.InL textEdit]])
                Nothing
          action = LSP.CodeAction
            title
            (Just LSP.CodeActionKind_QuickFix)
            Nothing
            Nothing
            Nothing
            (Just workspaceEdit)
            Nothing
            Nothing
      in Just action
  | otherwise = Nothing

-- | Generate capability-related action
generateCapabilityAction :: LSP.Uri -> Range -> Text -> Maybe LSP.CodeAction
generateCapabilityAction _ _ msg
  | "capability" `T.isInfixOf` msg = 
      let title = "Add capability"
          action = LSP.CodeAction
            title
            (Just LSP.CodeActionKind_QuickFix)
            Nothing
            Nothing
            Nothing
            Nothing  -- No workspace edit for now
            Nothing
            Nothing
      in Just action
  | otherwise = Nothing

-- | Generate cursor-based actions
generateCursorActions :: LSP.Uri -> Text -> PositionMatch ReplCoreBuiltin FileLocSpanInfo -> [LSP.CodeAction]
generateCursorActions uri content = \case
  TermMatch t -> generateTermActions uri content t
  DefunMatch _ -> [] -- Could add function-specific actions
  ConstMatch _ -> [] -- Could add constant-specific actions
  DefCapMatch _ -> [] -- Could add capability-specific actions
  _ -> []

-- | Generate actions for terms
generateTermActions :: LSP.Uri -> Text -> Term Name Type ReplCoreBuiltin FileLocSpanInfo -> [LSP.CodeAction]
generateTermActions uri _ term = catMaybes
  [ generateRenameAction uri (view spanInfo $ termInfo' term) (termName term)
  , generateExtractFunctionAction uri (view spanInfo $ termInfo' term)
  , generateInlineVariableAction uri (view spanInfo $ termInfo' term) (termName term)
  , generateAddTypeAction uri (view spanInfo $ termInfo' term) (termName term)
  ]
  where
    termName :: Term Name Type ReplCoreBuiltin FileLocSpanInfo -> Text
    termName = \case
      Var (Name n _) _ -> renderCompactText n
      _ -> "expression"
    
    termInfo' :: Term Name Type ReplCoreBuiltin FileLocSpanInfo -> FileLocSpanInfo
    termInfo' = \case
      Var _ i -> i
      Lam _ _ i -> i
      Let _ _ _ i -> i
      App _ _ i -> i
      Sequence _ _ i -> i
      Builtin _ i -> i
      BuiltinForm _ i -> i
      Constant _ i -> i
      ListLit _ i -> i
      ObjectLit _ i -> i
      Nullary _ i -> i
      Pact.Core.IR.Term.InlineValue _ i -> i

-- | Generate rename action
generateRenameAction :: LSP.Uri -> SpanInfo -> Text -> Maybe LSP.CodeAction
generateRenameAction _ _ name = 
  let title = "Rename '" <> name <> "'"
      action = LSP.CodeAction
        title
        (Just LSP.CodeActionKind_Refactor)
        Nothing
        Nothing
        Nothing
        Nothing  -- Would trigger rename workflow
        Nothing
        Nothing
  in Just action

-- | Generate extract function action
generateExtractFunctionAction :: LSP.Uri -> SpanInfo -> Maybe LSP.CodeAction
generateExtractFunctionAction _ _ = 
  let title = "Extract function"
      action = LSP.CodeAction
        title
        (Just LSP.CodeActionKind_RefactorExtract)
        Nothing
        Nothing
        Nothing
        Nothing  -- Would need complex refactoring logic
        Nothing
        Nothing
  in Just action

-- | Generate inline variable action
generateInlineVariableAction :: LSP.Uri -> SpanInfo -> Text -> Maybe LSP.CodeAction
generateInlineVariableAction _ _ name = 
  let title = "Inline '" <> name <> "'"
      action = LSP.CodeAction
        title
        (Just LSP.CodeActionKind_RefactorInline)
        Nothing
        Nothing
        Nothing
        Nothing  -- Would need inline logic
        Nothing
        Nothing
  in Just action

-- | Generate add type annotation action
generateAddTypeAction :: LSP.Uri -> SpanInfo -> Text -> Maybe LSP.CodeAction
generateAddTypeAction uri si name = 
  let title = "Add type to '" <> name <> "'"
      range = spanInfoToRange si
      placeholderType = ":string"
      endPos = range ^. LSPLens.end
      insertPos = Range endPos endPos
      textEdit = LSP.TextEdit insertPos placeholderType
      workspaceEdit = LSP.WorkspaceEdit Nothing 
            (Just [LSP.InL $ TextDocumentEdit
              (OptionalVersionedTextDocumentIdentifier uri (LSP.InR LSP.Null))
              [LSP.InL textEdit]])
            Nothing
      action = LSP.CodeAction
        title
        (Just LSP.CodeActionKind_QuickFix)
        Nothing
        Nothing
        Nothing
        (Just workspaceEdit)
        Nothing
        Nothing
  in Just action

-- | Extract module name from error message
extractModuleName :: Text -> Text
extractModuleName msg = 
  case T.words msg of
    (_:_:"module":name:_) -> T.filter (/= '\'') name
    _ -> "unknown-module"

-- | Get LSState
getState :: LSM LSState
getState = lift ask >>= liftIO . readMVar

-- | Convert SpanInfo to Range
spanInfoToRange :: SpanInfo -> Range
spanInfoToRange (SpanInfo sl sc el ec) = LSP.mkRange
  (fromIntegral sl) (fromIntegral sc)
  (fromIntegral el) (fromIntegral ec)