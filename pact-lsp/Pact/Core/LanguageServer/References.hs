{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Pact.Core.LanguageServer.References
  ( findReferencesHandler
  , getMatch
  ) where

import Control.Lens hiding (Iso, parts)
import Control.Monad.Trans (lift)
import Control.Monad.IO.Class
import Control.Monad.Reader (ask)
import Control.Concurrent.MVar
import Data.Maybe
import qualified Data.Map.Strict as M
import Data.Monoid (Alt(..))
import Language.LSP.Server
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types hiding (maybeToNull)
import qualified Language.LSP.Protocol.Lens as LSP

import Pact.Core.Builtin
import Pact.Core.Names
import Pact.Core.Info
import Pact.Core.IR.Term
import Pact.Core.LanguageServer.Types
import Pact.Core.LanguageServer.Utils
import Pact.Core.LanguageServer.Renaming
import Pact.Core.Repl.Utils (replTLDefPos)

-- | Main find references handler
findReferencesHandler :: Handlers LSM
findReferencesHandler = requestHandler SMethod_TextDocumentReferences $ \req resp ->
  getState >>= \st -> do
    let params' = req ^. LSP.params
        uri' = params' ^. LSP.textDocument . LSP.uri
        nuri = toNormalizedUri uri'
        pos = params' ^. LSP.position
        includeDecl = params' ^. LSP.context . LSP.includeDeclaration
    
    -- Use the existing getMatch function to find what's at the cursor
    case getMatch pos =<< view (lsTopLevel . at nuri) st of
      Nothing -> resp $ Right $ InL []
      Just termMatch -> do
        -- Use the existing getRenameSpanInfo infrastructure to find all occurrences
        let allTopLevels = concatMap snd $ M.toList $ view lsTopLevel st
            spanInfos = getRenameSpanInfo allTopLevels termMatch
        
        -- Convert SpanInfos to Locations
        let locations = mapMaybe (spanInfoToLocation nuri) spanInfos
        
        -- Filter declaration if not requested
        finalLocations <- if includeDecl
          then pure locations
          else filterDeclaration termMatch st locations
        
        resp $ Right $ InL finalLocations

-- | Convert SpanInfo to Location (simplified approach)
spanInfoToLocation :: NormalizedUri -> SpanInfo -> Maybe Location
spanInfoToLocation uri span' = 
  Just $ Location (fromNormalizedUri uri) (spanInfoToLSPRange span')

-- | Filter out declaration from results if not requested
filterDeclaration 
  :: PositionMatch ReplCoreBuiltin FileLocSpanInfo 
  -> LSState 
  -> [Location] 
  -> LSM [Location]
filterDeclaration termMatch st locations = case termMatch of
  TermMatch (Var (Name n (NTopLevel mn _)) _) -> do
    let qn = QualifiedName n mn
    -- Find the declaration position from REPL state
    case listToMaybe $ M.elems $ view lsReplState st of
      Just replState -> case M.lookup qn (view replTLDefPos replState) of
        Just defFileSpan -> do
          let declRange = spanInfoToLSPRange (view spanInfo defFileSpan)
          -- Filter out any location that matches the declaration range
          pure $ filter (\(Location _ range) -> range /= declRange) locations
        Nothing -> pure locations
      Nothing -> pure locations
  DefunMatch (Defun spec _ _ _) -> do
    let declRange = spanInfoToLSPRange (view (argInfo . spanInfo) spec)
    pure $ filter (\(Location _ range) -> range /= declRange) locations
  _ -> pure locations

-- | Convert SpanInfo to LSP Range
spanInfoToLSPRange :: SpanInfo -> Range
spanInfoToLSPRange (SpanInfo sl sc el ec) = Range
  (Position (fromIntegral sl) (fromIntegral sc))
  (Position (fromIntegral el) (fromIntegral ec))

-- | Get match at position (redefining from main LSP module)
getMatch
  :: HasSpanInfo i
  => Position
  -> [EvalTopLevel ReplCoreBuiltin i]
  -> Maybe (PositionMatch ReplCoreBuiltin i)
getMatch pos tl = getAlt (foldMap (Alt . topLevelTermAt pos) tl)

-- | Get LSState
getState :: LSM LSState
getState = lift ask >>= liftIO . readMVar