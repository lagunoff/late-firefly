module Main where
import Haste.App hiding (Server)
import Telikov.RPC (TelikovBackend)
import Language.Javascript.JSaddle.Warp (run)
import GHCJS.DOM.Types (JSM)
import qualified Telikov.Home as Home
import Control.Concurrent (forkIO)
import Control.Monad (void)

mainClient :: JSM ()
mainClient = Home.main =<< Home.init

main :: IO ()
main = do
  void $ forkIO $ run 3709 mainClient
  runApp [start (Proxy :: Proxy TelikovBackend)] $ pure ()
