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
import LateFirefly.Orphans
import GHC.Int
#ifndef __GHCJS__
import Data.Aeson
#endif

newtype Id t = Id {unId :: Int64}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype Flat

newtype UUID5 t = UUID5 {unUUID5 :: UUID}
  deriving stock (Show, Read, Eq, Generic)
  deriving newtype Flat

data ThrowException e = ThrowException e
  deriving (Show, Eq, Generic, P.Exception)

maybeThrowStack :: (HasCallStack, ?throw::CallStack) => Maybe a -> a
maybeThrowStack = fromMaybe (safeThrow callStack)

safeThrow :: (?throw::e, Typeable e, Show e) => e -> x
safeThrow = P.throw . ThrowException

unsafeRunThrow :: ((?throw::e) => x) -> x
unsafeRunThrow x = let ?throw = error "unsafeRunThrow: ?throw accessed" in x

throwToEither :: forall e x m. (Typeable e, Show e, MonadCatch m) => ((?throw::e) => m x) -> m (Either e x)
throwToEither f = do
  fmap Right (unsafeRunThrow f) `C.catch` \(ThrowException e) -> pure (Left e)

mapThrow :: (b -> a) -> ((?throw::a) => x) -> (?throw::b) => x
mapThrow f x = let ?throw = f ?throw in x

data BackendError
  = SQLError SQLError
  | BEFlatError String
  deriving stock (Show, Eq, Generic)
#ifndef __GHCJS__
  deriving anyclass (FromJSON, ToJSON)
#endif

data PublicBackendError
  = ErrorCode (Id Int)
  deriving stock (Show, Eq, Generic)
  deriving anyclass Flat

data FrontendError
  = XHRError XHR.XHRError
  | FlatError String
  | BadResponse Text
  | BackendError PublicBackendError
  deriving stock (Show, Eq, Generic)

type Backend a r
  = (?conn::Connection, ?throw::BackendError)
  => a -> IO r

deriving stock instance Generic SQLError
deriving stock instance Generic SQLite3.Error
#ifndef __GHCJS__
deriving anyclass instance FromJSON SQLError
deriving anyclass instance ToJSON SQLError
deriving anyclass instance FromJSON SQLite3.Error
deriving anyclass instance ToJSON SQLite3.Error
#endif
