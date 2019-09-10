{-# LANGUAGE CPP #-}
module Telikov.Utils where
import GHCJS.DOM.Types (JSM)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

liftJSM :: JSM () -> IO ()
#ifdef __GHCJS__
liftJSM = id
#else
liftJSM _ = pure ()
#endif

class MonadQuery m where
  query :: (ToRow q, FromRow r) => Query -> q -> m r
