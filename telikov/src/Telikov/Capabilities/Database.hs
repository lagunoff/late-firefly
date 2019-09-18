{-# LANGUAGE TemplateHaskell, LambdaCase #-}
module Telikov.Capabilities.Database where

import Database.SQLite.Simple (Connection, FromRow, ToRow)
import Data.Int (Int64)
import Polysemy (Member, Embed, Sem, makeSem_, embed, interpret)
import qualified Database.SQLite.Simple as SQLite

data SQL m a where
  Query :: (ToRow q, FromRow row) => SQLite.Query -> q -> SQL m [row]
  Query_ :: (FromRow row) => SQLite.Query -> SQL m [row]
  Execute :: (ToRow q) => SQLite.Query -> q -> SQL m ()
  Execute_ :: SQLite.Query -> SQL m ()
  LastInsertRowId :: SQL m Int64
makeSem_ ''SQL

query :: forall row q eff. (FromRow row, ToRow q, Member SQL eff) => SQLite.Query -> q -> Sem eff [row]
query_ :: forall row eff. (FromRow row, Member SQL eff) => SQLite.Query -> Sem eff [row]
execute :: forall q eff. (ToRow q, Member SQL eff) => SQLite.Query -> q -> Sem eff ()
execute_ :: forall eff. (Member SQL eff) => SQLite.Query -> Sem eff ()
lastInsertRowId :: (Member SQL eff) => Sem eff Int64

sql2IO :: Member (Embed IO) r => Connection -> Sem (SQL ': r) a -> Sem r a
sql2IO conn = interpret $ \case
  Query q p -> embed $ SQLite.query conn q p
  Query_ q -> embed $ SQLite.query_ conn q
  Execute q p -> embed $ SQLite.execute conn q p
  Execute_ q -> embed $ SQLite.execute_ conn q
  LastInsertRowId -> embed $ SQLite.lastInsertRowId conn

