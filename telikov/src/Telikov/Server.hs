module Main where
import Haste.App hiding (Server)
import Telikov.RPC (TelikovBackend)
import Language.Javascript.JSaddle.Warp (run)
import GHCJS.DOM.Types (JSM)
import qualified Telikov.Home as Home
import Control.Concurrent (forkIO)
import Data.Functor (void)
import GHCJS.DOM
import GHCJS.DOM.Document
import GHCJS.DOM.Node hiding (Node)
import Telikov.RPC (rpc2JSM)
import Telikov.Effects (evaluateMessages, io2jsm, http2JSM, runM, Exists(..))
import Polysemy.State (runState)
import Massaraksh
import Control.Lens ((&))

mainClient :: JSM ()
mainClient = do
  model <- Home.init & http2JSM & rpc2JSM & io2jsm & runM 
  doc  <- currentDocumentUnchecked
  body <- getBodyUnchecked doc
  StoreHandle store modifyStore <- createStore model
  let sink (Step f) = modifyStore f
      sink (Yield (Exists msg)) = do
        curr <- readLatest store
        (s, _) <- Home.eval msg
          & evaluateMessages Home.eval
          & rpc2JSM
          & io2jsm
          & http2JSM
          & runState curr
          & runM
        modifyStore (const s)
  UIHandle node _ <- unUI Home.view store sink
  appendChild_ body node

main :: IO ()
main = runApp [start (Proxy :: Proxy TelikovBackend)] $ pure ()
    
