{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StaticPointers    #-}
{-# LANGUAGE TypeFamilies, LambdaCase, TemplateHaskell, ConstraintKinds      #-}
module Telikov.RPC where

import Control.Monad.Catch
import Data.Aeson (decode)
import Data.String (fromString)
import Data.Text (Text)
import GHCJS.DOM.Types (JSM)
import Haste.App (Mapping(..), MonadClient(..), Node(..), Env, Import, Dispatch, StaticPtr, liftIO)
import Haste.App.Protocol (Endpoint(..), ServerReply(..), ServerException(..), NetworkException(..))
import qualified Haste.App.Remote as Remote
import qualified Database.SQLite.Simple as SQLite
import Control.Monad.Fail (MonadFail(..))
import Telikov.Effects (SQL(..), sql2IO, CurrentTime, time2IO, embed, Embed, Sem, runM)

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

instance Mapping TelikovBackend a where
   invoke env node = runM $ time2IO $ sql2IO (envConnection env) $ node

data RPCEnv = RPCEnv
  { envConnection :: SQLite.Connection
  }
  
type TelikovBackend = Sem '[SQL, CurrentTime, Embed IO]

instance Node TelikovBackend where
  type Env TelikovBackend = RPCEnv
  init _ = RPCEnv <$> SQLite.open "test.db"

instance MonadFail TelikovBackend where
  fail = embed @IO . error

callRPC :: forall cli dom. Dispatch dom cli => StaticPtr (Import dom) -> cli
callRPC = Remote.dispatch
