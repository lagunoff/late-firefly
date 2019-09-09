{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StaticPointers    #-}
{-# LANGUAGE TypeFamilies      #-}
module TheOffice.RPC
  ( greet
  , mainRPC
  ) where

import Control.Monad.Catch
import Data.Aeson                     (Value, decode, encode, fromJSON, toJSON)
import Data.IORef
import Database.MongoDB               (AccessMode, Action, Database, Host,
                                       ObjectId, Selection (..),
                                       Value (Doc, Null, ObjId, ObjId), access,
                                       close, connect, createCollection, host,
                                       insert, master, modify, (=:))
import GHCJS.DOM.Types                (JSM)
import Haste.App
import Haste.App.Protocol
import IWatchTheOffice.Db
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.WebSockets
import Network.WebSockets             as WS

instance MonadClient JSM where
  remoteCall (WebSocket h p) msg n = liftIO $ do
    putStrLn $ "Sending rpc to " <> h <> ":" <> show p
    WS.runClient "127.0.0.1" p "/" $ \ c' -> do
      sendTextData c' (fromString msg)
      reply <- receiveData c'
      case decode reply of
        Just (ServerReply n' msg) -> return msg
        Just (ServerEx _ msg)     -> throwM (ServerException msg)
        Nothing                     -> throwM (NetworkException "Cannot decode ServerReply")

type MyS = EnvServer (IORef Int)

instance Node MyS where
  type Env MyS = IORef Int
  init _ = liftIO $ newIORef 0

greet :: RemotePtr (String -> MyS [Season])
greet = static (native $ remote $ \s -> do
  liftIO $ withPipe (host "127.0.0.1") "telikov" master $ do
    [Only updateId] <- query_ conn "select max(rowid) from updates where finished_at not null" :: IO [Only Int]
    seasons <- query conn "select code, thumbnail, href from iwatchtheoffice_seasons where update_id=?" (Only updateId) :: IO [Season]
    pure seasons
  )

mainRPC = runApp [start (Proxy :: Proxy MyS)] $ pure ()

withPipe :: MonadIO m => Host -> Database -> AccessMode -> Action m a -> m a
withPipe host db mode action = do
  pipe <- liftIO $ connect host
  a <- access pipe mode db action
  liftIO $ close pipe
  pure a
