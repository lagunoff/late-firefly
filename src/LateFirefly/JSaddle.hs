module LateFirefly.JSaddle where

import Language.Javascript.JSaddle.Types (JSM(..))
import Network.Wai.Handler.Warp
import Network.Wai as W
import Language.Javascript.JSaddle.WebSockets
import Language.Javascript.JSaddle.Run (syncPoint)
import Network.WebSockets (defaultConnectionOptions)
import Debug.Trace

debugOr :: Int -> JSM () -> Application -> IO ()
debugOr port f b = do
  debugWrapper $ \withRefresh registerContext ->
    runSettings (setPort port (setTimeout 3600 defaultSettings)) =<<
      jsaddleOr defaultConnectionOptions (registerContext >> f >> syncPoint) (withRefresh $ jsaddleAppWithJsOr (jsaddleJs True) b)
  traceM $ "<a href=\"http://localhost:" <> show port <> "\">run</a>"
