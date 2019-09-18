{-# LANGUAGE TemplateHaskell, LambdaCase #-}
module Telikov.Effects
  ( module Network.Wreq
  , module Polysemy
  , UTCTime(..)
  , SQL(..), query, query_, execute, execute_, lastInsertRowId, sql2IO
  , CurrentTime(..), currentTime, time2IO, Http(..), httpGet, http2IO
  , Eff
  ) where 

import Database.SQLite.Simple (Connection, FromRow, ToRow)
import Data.Int (Int64)
import Polysemy
import qualified Database.SQLite.Simple as SQLite
import Data.Time.Clock.POSIX (getCurrentTime)
import Data.Time.Clock (UTCTime(..))
import Network.Wreq (Response, get, responseBody)
import qualified Data.ByteString.Lazy as L

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

data CurrentTime m a where
  CurrentTime :: CurrentTime m UTCTime
makeSem ''CurrentTime

time2IO :: Member (Embed IO) r => Sem (CurrentTime ': r) a -> Sem r a
time2IO = interpret $ \case
  CurrentTime -> embed getCurrentTime

data Http m a where
  HttpGet :: String -> Http m (Response L.ByteString)
makeSem ''Http

http2IO :: Member (Embed IO) r => Sem (Http ': r) a -> Sem r a
http2IO = interpret $ \case
  HttpGet url -> embed (putStrLn $ "GET " <> url) *> embed (get url)

type Eff = Sem
