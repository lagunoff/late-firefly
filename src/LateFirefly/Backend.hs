{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module LateFirefly.Backend where

import Control.Lens hiding (As)
import Control.Monad.Catch as C
import Data.Text as T
import Data.Typeable
import Database.SQLite.Simple (FromRow(..), Connection, Query(..))
import Database.SQLite3 (SQLError(..), Error(..))
import Flat
import GHC.Int
import GHC.StaticPtr
import JavaScript.Web.XMLHttpRequest as XHR
import Language.Haskell.TH as TH
import LateFirefly.DB.QQ
import LateFirefly.Eio
import LateFirefly.Orphans ()
import qualified Database.SQLite.Simple as S
import Data.Aeson as AE

data BackendError
  = SQLError SQLError
  | BEFlatError String
  | The404Error
  deriving stock (Show, Eq, Generic)
  deriving Exception
  deriving anyclass (FromJSON, ToJSON)

data PublicBackendError
  = ErrorCode Int64
  deriving stock (Show, Eq, Generic)
  deriving anyclass Flat

data FrontendError
  = XHRError XHR.XHRError
  | FlatError String
  | BadResponse Text
  | BackendError PublicBackendError
  deriving stock (Show, Eq, Generic)
  deriving Exception

type BackendIO = Eio BackendError

data Backend a r = (Typeable a, Typeable r, Flat a, Flat r) =>
  Backend ((?conn::Connection) => a -> Eio BackendError r)

type UnBackend a r =
  (?conn::Connection) => a -> Eio BackendError r

data SomeBackend = forall a r. SomeBackend (Backend a r)

data RemotePtr a r = RemotePtr
  { rptrStaticPtr :: StaticPtr SomeBackend
  , rptrName      :: Name }

data Progress = Progress
  { inc     :: IO ()
  , display :: Text -> IO () }

deriving stock instance Generic SQLError
deriving stock instance Generic S.Error
deriving anyclass instance FromJSON SQLError
deriving anyclass instance ToJSON SQLError
deriving anyclass instance FromJSON S.Error
deriving anyclass instance ToJSON S.Error

query1 :: (?conn::Connection, As BackendError e, FromRow r) => Sql -> Eio e r
query1 q = withEio (review _S) $ query q >>= \case
  []    -> throwE The404Error
  (x:_) -> pure x
  where
    query :: (?conn::Connection, As S.SQLError e, FromRow r) => Sql -> Eio e [r]
    query (Sql t [] _) = liftEio $ Eio @SQLError $ S.query_ ?conn (Query t)
    query (Sql t p _)  = liftEio $ Eio @SQLError $ S.query ?conn (Query t) p
