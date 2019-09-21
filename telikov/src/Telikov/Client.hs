{-# LANGUAGE CPP #-}
module Main where

import GHCJS.DOM.Types (JSM)
import qualified Telikov.Home as Home
import GHCJS.DOM
import GHCJS.DOM.Document
import GHCJS.DOM.Node hiding (Node)
import Telikov.RPC (rpc2JSM)
import Telikov.Effects (io2jsm, http2JSM, runM)
import Polysemy.State (runState)
import Massaraksh
import Telikov.Header (Exists(..))
import Control.Lens ((&))

mainClient :: JSM ()
mainClient = do
  model <- Home.init & rpc2JSM & io2jsm & runM 
  doc  <- currentDocumentUnchecked
  body <- getBodyUnchecked doc
  StoreHandle store modifyStore <- createStore model
  let sink (Step f) = modifyStore f
      sink (Yield (Exists msg)) = do
        curr <- readLatest store
        (s, _) <- Home.eval msg & http2JSM & runState curr & runM
        modifyStore (const s)
  UIHandle node _ <- unUI Home.view store sink
  appendChild_ body node

main :: IO ()
#ifdef ghcjs_HOST_OS
main = mainClient
#else
main = error "Telicov.Client.main: works only in GHCJS"
#endif
