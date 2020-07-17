{-# LANGUAGE CPP #-}
module LateFirefly.Utils where

import Control.Error
import GHC.Stack
import GHC.Generics
import Data.Typeable
import Flat
import qualified Control.Exception as P
import Data.Text
import JavaScript.Web.XMLHttpRequest as XHR
import Database.SQLite.Simple
import Control.Monad.Catch as C
import Database.SQLite3 as SQLite3
import Data.UUID (UUID)
import Control.Monad.Error
import Control.Monad.Catch as Catch
import Massaraksh (Html)
import LateFirefly.Orphans ()
import LateFirefly.Eio
import GHC.Int
import qualified Control.Exception as Exception
#ifndef __GHCJS__
import Data.Aeson
#endif

newtype Id t = Id {unId :: Int64}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype Flat

newtype UUID5 t = UUID5 {unUUID5 :: UUID}
  deriving stock (Show, Read, Eq, Generic)
  deriving newtype Flat

data BackendError
  = SQLError SQLError
  | BEFlatError String
  deriving stock (Show, Eq, Generic)
  deriving Exception
#ifndef __GHCJS__
  deriving anyclass (FromJSON, ToJSON)
#endif

data PublicBackendError
  = ErrorCode (Id BackendError)
  deriving stock (Show, Eq, Generic)
  deriving anyclass Flat

data FrontendError
  = XHRError XHR.XHRError
  | FlatError String
  | BadResponse Text
  | BackendError PublicBackendError
  deriving stock (Show, Eq, Generic)
  deriving Exception

type Backend a r = (?conn::Connection) => a -> Eio BackendError r

deriving stock instance Generic SQLError
deriving stock instance Generic SQLite3.Error
#ifndef __GHCJS__
deriving anyclass instance FromJSON SQLError
deriving anyclass instance ToJSON SQLError
deriving anyclass instance FromJSON SQLite3.Error
deriving anyclass instance ToJSON SQLite3.Error
#endif

instance MonadError FrontendError Html where
  throwError = Catch.throwM
  catchError = Catch.catch

catchSync :: IO a -> (SomeException -> IO a) -> IO a
catchSync io f = io `Exception.catch` \e ->
  case Exception.fromException e of
    Just (Exception.SomeAsyncException _) -> Exception.throwIO e
    Nothing                               -> f e
