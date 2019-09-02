{-# LANGUAGE CPP #-}
module TheOffice.Utils where
import GHCJS.DOM.Types (JSM)

liftJSM :: JSM () -> IO ()
#ifdef __GHCJS__
liftJSM = id
#else
liftJSM _ = pure ()
#endif

class MonadQuery m where
  query :: (ToRow q, FromRow r) => Query -> q -> m r
