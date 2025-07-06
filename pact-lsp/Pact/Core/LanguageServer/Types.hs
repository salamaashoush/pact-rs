{-# LANGUAGE TemplateHaskell #-}

module Pact.Core.LanguageServer.Types 
  ( LSState(..)
  , lsReplState
  , lsTopLevel
  , lsFormattingConfig
  , LSM
  ) where

import Control.Lens
import Control.Monad.Reader (ReaderT)
import Control.Concurrent.MVar
import qualified Data.Map.Strict as M
import Language.LSP.Server
import Language.LSP.Protocol.Types

import Pact.Core.Builtin
import Pact.Core.Info
import Pact.Core.IR.Term
import Pact.Core.Environment.Types
import qualified Data.Aeson as Aeson

data LSState =
  LSState
  { _lsReplState :: M.Map NormalizedUri (ReplState ReplCoreBuiltin)
  -- ^ Post-Compilation State for each opened file
  , _lsTopLevel :: M.Map NormalizedUri [EvalTopLevel ReplCoreBuiltin FileLocSpanInfo]
  -- ^ Top-level terms for each opened file. Used to find the match of a
  --   particular (cursor) position inside the file.
  , _lsFormattingConfig :: Maybe Aeson.Value
  -- ^ Custom formatting configuration from workspace settings
  }

makeLenses ''LSState

type LSM = LspT () (ReaderT (MVar LSState) IO)