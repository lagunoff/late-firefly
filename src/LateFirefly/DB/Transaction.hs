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
import qualified Database.SQLite.Simple as S

data Transaction = Transaction
  { rowid      :: Id Transaction
  , active     :: Bool
  , startedAt  :: UTCTime
  , finishedAt :: Maybe UTCTime }
  deriving stock (Show, Eq, Generic)
  deriving anyclass Flat

deriveDb ''Transaction

newVersion :: Given Connection => (Given NewVersion => IO a) -> IO a
newVersion act = do
  startedAt <- getCurrentTime
  t <- upsert (Transaction def True startedAt Nothing)
  let newTr = NewVersion (getField @"rowid" t)
  a <- give newTr act
  finishedAt <- Just <$> getCurrentTime
  upsert (t {finishedAt})
  a <$ DB.execute
    [sql|update `transaction` set active=0 where active=1 and rowid <> ?|]
    [newTr]

withConnection :: FilePath -> (Given Connection => IO a) -> IO a
withConnection path act = S.withConnection path \conn -> do
  mapM_ (S.execute_ conn)
    ["PRAGMA journal_mode=WAL"]
  give conn act

newtype NewVersion = NewVersion {unNewVersion :: Id Transaction}
  deriving newtype (ToField, FromField)
