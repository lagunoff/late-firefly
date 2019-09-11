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
import Haste.App
import Haste.App.Protocol

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
  
type MyS = EnvServer ()

instance Node MyS where
  type Env MyS = ()
  init _ = pure ()
