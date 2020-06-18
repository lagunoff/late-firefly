{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
module LateFirefly.Backend where

import Control.Monad.Fail (MonadFail)
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Flat as FL
import LateFirefly.DB
import LateFirefly.Prelude
import Options.Generic

data WebOpts = WebOpts
  { webPort :: Int
  , dbPath  :: Text }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ParseRecord)

instance Default WebOpts where
  def = WebOpts 8080 "./late-firefly.sqlite"

-- newtype Backend a = Backend {unBackend :: ReaderT Connection IO a}
--   deriving newtype (Functor, Applicative, Monad, MonadIO, MonadFail)
--   deriving newtype (MonadReader Connection)

-- backend :: ((?conn :: Connection) => IO a) -> Backend a
-- backend io = Backend $ ReaderT \conn -> give conn io

-- type MonadClient = MonadRpc Backend

-- remote
--   :: (Flat a, Flat r) => ((?conn :: Connection) => a -> IO r)
--   -> RemoteMethod Backend a r

-- runBackend :: Connection -> (forall a. Backend a -> IO a)

-- #ifndef ghcjs_HOST_OS
-- remote io = FL.remote @Backend \a -> backend (io a)
-- runBackend conn b = flip runReaderT conn (unBackend b)
-- #else
-- remote _ = error "remote: Unimplemented in GHCJS"
-- runBackend _ = error "runBackend: Unimplemented in GHCJS"
-- #endif

-- instance MonadUnliftIO cli => MonadUnliftIO (RpcT srv cli) where
--   askUnliftIO = RpcT $ ReaderT \e -> do
--     un <- askUnliftIO
--     pure $ UnliftIO \(RpcT (ReaderT f)) -> unliftIO un (f e)
