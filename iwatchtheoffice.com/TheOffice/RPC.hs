{-# LANGUAGE NamedFieldPuns, CPP, StaticPointers, TypeFamilies, FlexibleInstances, OverloadedStrings #-}
module TheOffice.RPC
  ( greet
  , mainRPC
  ) where

import Haste.App
import GHCJS.DOM.Types (JSM)
import Network.WebSockets as WS
import Haste.App.Protocol
import Data.ByteString.Lazy.UTF8 hiding (decode, encode)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.WebSockets
import Data.Aeson (Value, fromJSON, toJSON, decode, encode)
import Control.Monad.Catch
import Data.IORef
import IWatchTheOffice.Db
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.QQ

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
  liftIO $ withConnection "./test.db" $ \conn -> do
    [Only updateId] <- query_ conn "select max(rowid) from updates where finished_at not null" :: IO [Only Int]
    seasons <- query conn "select code, thumbnail, href from iwatchtheoffice_seasons where update_id=?" (Only updateId) :: IO [Season]
    pure seasons
  )

mainRPC = runApp [start (Proxy :: Proxy MyS)] $ pure ()
