{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StaticPointers    #-}
{-# LANGUAGE TypeFamilies      #-}
module Telikov.RPC where

import Control.Monad.Catch
import Data.Aeson (decode)
import Data.String (fromString)
import Data.Text (Text)
import GHCJS.DOM.Types (JSM)
import Haste.App (MonadClient(..), Node(..), EnvServer, Env, Import, Dispatch, StaticPtr, liftIO)
import Haste.App.Protocol (Endpoint(..), ServerReply(..), ServerException(..), NetworkException(..))
import qualified Haste.App.Remote as Remote
import Database.SQLite.Simple (Connection, open, Query, FromRow, ToRow)
import qualified Database.SQLite.Simple as SQLite
import Control.Monad.Reader.Class (asks)
import Control.Monad.Fail (MonadFail(..))

#ifndef __GHCJS__
import qualified Network.WebSockets as WS

instance MonadClient JSM where
  remoteCall (WebSocket h p) msg n = liftIO $ do
    putStrLn $ "Sending rpc to " <> h <> ":" <> show p
    WS.runClient "127.0.0.1" p "/" $ \ c' -> do
      WS.sendTextData c' (fromString msg :: Text)
      reply <- WS.receiveData c'
      case decode reply of
        Just (ServerReply n' msg) -> return msg
        Just (ServerEx _ msg)     -> throwM (ServerException msg)
        Nothing                   -> throwM (NetworkException "Cannot decode ServerReply")
#else
import qualified JavaScript.Web.WebSocket as WS
import qualified Data.JSString as J
import Control.Concurrent (newEmptyMVar, takeMVar, putMVar)
import qualified JavaScript.Web.MessageEvent as ME
import qualified Data.ByteString.Lazy.UTF8 as L
import Data.Aeson (eitherDecode, object, Result(..))

instance MonadClient JSM where
  remoteCall ep@(LocalNode{}) pkt n = do
    error "remoteCall: LocalNode unimplemented"
  remoteCall ep@(WebSocket{ wsEndpointHost, wsEndpointPort }) pkt n = do
    mvar <- liftIO $ newEmptyMVar
    let handleMessage :: ME.MessageEvent -> IO ()
        handleMessage e = case ME.getData e of
          ME.StringData jsstr -> case eitherDecode (L.fromString . J.unpack $ jsstr) of
            Right (ServerReply _ val) -> putMVar mvar val
            Right (ServerEx _ e) -> error e
            Left e -> error e
          _ -> error "Invalid WS message type"
        wsRequest = WS.WebSocketRequest (J.pack $ "ws://" <> wsEndpointHost <> ":" <> show wsEndpointPort) [] Nothing (Just handleMessage)
    conn <- liftIO $ WS.connect wsRequest
    liftIO $ WS.send (J.pack pkt) conn
    liftIO $ takeMVar mvar

#endif

data RPCEnv = RPCEnv
  { envConnection :: Connection
  }
type Server = EnvServer RPCEnv

class MonadFail m => HasDatabase m where
  query :: (ToRow q, FromRow r) => Query -> q -> m [r]
  query_ :: (FromRow r) => Query -> m [r]
  execute :: (ToRow q) => Query -> q -> m ()
  execute_ :: Query -> m ()

instance Node Server where
  type Env Server = RPCEnv
  init _ = RPCEnv <$> open "test.db"

instance HasDatabase Server where
  query q p = asks envConnection >>= \conn -> liftIO $ SQLite.query conn q p
  query_ q = asks envConnection >>= \conn -> liftIO $ SQLite.query_ conn q
  execute q p = asks envConnection >>= \conn -> liftIO $ SQLite.execute conn q p
  execute_ q = asks envConnection >>= \conn -> liftIO $ SQLite.execute_ conn q

instance MonadFail Server where
  fail = liftIO . error

remote :: forall dom. (Remote.Export dom, Remote.Affinity dom ~ Server) => dom -> Import dom
#ifndef __GHCJS__
remote = Remote.remote
#else
remote = error "expression not available on GHCJS"
#endif

callRPC :: forall cli dom. Dispatch dom cli => StaticPtr (Import dom) -> cli
callRPC = Remote.dispatch
