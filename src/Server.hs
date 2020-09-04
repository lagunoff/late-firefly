{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Server where

import Control.Lens hiding (As)
import Control.Monad.Catch as C
import Data.Text as T
import Data.Typeable
import Database.SQLite.Simple (FromRow(..), Connection, Query(..))
import Database.SQLite3 (SQLError(..), Error(..))
import GHC.Generics
import GHC.StaticPtr
import Language.Haskell.TH as TH
import qualified Database.SQLite.Simple as S

import "this" DB.QQ
import "this" Eio
import "this" Orphans ()

data ServerError
  = SQLError SQLError
  | The404Error
  deriving stock (Show, Eq, Generic)
  deriving anyclass Exception

type ServerIO = Eio ServerError

data Server a r = (Typeable a, Typeable r) =>
  Server ((?conn::Connection) => a -> ServerIO r)

type UnServer a r = (?conn::Connection) => a -> ServerIO r

data SomeServer = forall a r. SomeServer (Server a r)

data RemotePtr a r = RemotePtr
  { rptrStaticPtr :: StaticPtr SomeServer
  , rptrName      :: Name }

data Progress = Progress
  { inc     :: IO ()
  , display :: Text -> IO () }

query1 :: (?conn::Connection, As ServerError e, FromRow r) => Sql -> Eio e r
query1 q = withEio (review _S) $ query q >>= \case
  []    -> throwE The404Error
  (x:_) -> pure x
  where
    query :: (?conn::Connection, As S.SQLError e, FromRow r) => Sql -> Eio e [r]
    query (Sql t [] _) = liftEio $ Eio @SQLError $ S.query_ ?conn (Query t)
    query (Sql t p _)  = liftEio $ Eio @SQLError $ S.query ?conn (Query t) p

deriving stock instance Generic SQLError
deriving stock instance Generic S.Error
