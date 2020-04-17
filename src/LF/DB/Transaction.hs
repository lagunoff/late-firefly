module LF.DB.Transaction where

import LF.DB.Base
import LF.DB.TH
import LF.Prelude
import Flat.Rpc
import Data.Time
import Database.SQLite.Simple
import Data.Generics.Product

data Transaction = Transaction
  { rowid      :: Id Transaction
  , startedAt  :: UTCTime
  , finishedAt :: Maybe UTCTime }
  deriving stock (Show, Eq, Generic)
  deriving anyclass Flat

deriveDb ''Transaction

newVersion :: Given Connection => (Given (Id Transaction) => IO a) -> IO a
newVersion act = do
  startedAt <- getCurrentTime
  t <- upsert (Transaction def startedAt Nothing)
  a <- give (getField @"rowid" t) act
  finishedAt <- Just <$> getCurrentTime
  a <$ upsert (t {finishedAt})

type Version = Id Transaction
