{-# LANGUAGE CPP #-}
module LateFirefly.DB.Transaction where

import Data.Generics.Product
import Data.Time
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Flat
import LateFirefly.DB.Base as DB
import LateFirefly.DB.QQ
import LateFirefly.DB.TH
import LateFirefly.Prelude
import GHC.Stack
import qualified Database.SQLite.Simple as S
import Database.SQLite3 (ColumnType(..))
import Control.Exception as E
#ifndef __GHCJS__
import Data.Aeson
#endif

data CaughtException e
  = KnownException e
  | UnknownException String
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Flat)
  deriving (FromField, ToField) via JsonField (CaughtException e)
#ifndef __GHCJS__
  deriving anyclass (FromJSON, ToJSON)
#endif

instance
  ( Typeable e
#ifndef __GHCJS__
  , FromJSON e, ToJSON e
#endif
  ) =>
  DbField (CaughtException e) where
    columnInfo _ = ColumnInfo TextColumn False False Nothing

data Transaction = Transaction
  { rowid      :: Id Transaction
  , active     :: Bool
  , startedAt  :: UTCTime
  , finishedAt :: Maybe UTCTime
  , exception  :: Maybe (CaughtException CallStack) }
  deriving stock (Show, Generic)
  deriving anyclass Flat

deriveDb ''Transaction

newVersion :: (?conn::Connection) => ((?version::NewVersion, ?throw::CallStack) => IO a) -> IO (Either (CaughtException CallStack) a)
newVersion act = do
  startedAt <- getCurrentTime
  t <- upsert (Transaction def True startedAt Nothing Nothing)
  let newTr = NewVersion (getField @"rowid" t)
  a <- let ?version = newTr in fmap Right act
    `E.catch` (\(ThrowException cs) -> pure $ Left $ KnownException cs)
    `E.catch` (\(e::SomeException)  -> pure $ Left $ UnknownException $ show e)
  finishedAt <- Just <$> getCurrentTime
  upsert (t {finishedAt, exception = either Just (const Nothing) a})
  a <$ DB.execute
    [sql|update `transaction` set active=0 where active=1 and rowid <> ?|]
    [newTr]

withConnection :: FilePath -> ((?conn::Connection) => IO a) -> IO a
withConnection path act = S.withConnection path \conn -> do
  mapM_ (S.execute_ conn)
    ["PRAGMA journal_mode=WAL"]
  let ?conn = conn in act

newtype NewVersion = NewVersion {unNewVersion :: Id Transaction}
  deriving newtype (ToField, FromField)
