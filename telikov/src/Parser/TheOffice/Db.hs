{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Parser.TheOffice.Db
  ( Season(..)
  , Episode(..)
  , initSchema
  , HasDatabase(..)
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Foldable (for_)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple (Connection, Query, execute_)
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.QQ (sql)
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.ToRow
import GHC.Generics (Generic)

data Episode = Episode
  { episodeTid              :: Int64
  , episodeSeasonId         :: Int64
  , episodeCode             :: Text
  , episodeName             :: Text
  , episodeHref             :: Text
  , episodeShortDescription :: Text
  , episodeThumbnail        :: Text
  , episodeDescription      :: Text
  , episodeLinks            :: [Text]
  } deriving (Show, Generic, ToJSON, FromJSON)

data Season = Season
  { seasonTid       :: Int64
  , seasonThumbnail :: Text
  , seasonHref      :: Text
  } deriving (Show, Generic, ToJSON, FromJSON)

-- | Whole schema
dbSchema :: Schema ()
dbSchema = Table [sql| create table if not exists transactions
  ( started_at integer not null
  , finished_at integer default null
  ) |]
  :+: (schema :: Schema Season)
  :+: (schema :: Schema Episode)

instance HasSchema Episode where
  schema = Table [sql| create table if not exists episodes
  ( tid integer not null
  , season_id integer not null
  , code text not null
  , name text not null
  , href text not null
  , short_description text not null
  , thumbnail text not null
  , description text not null
  , links text not null
  , foreign key(season_id) references seasons(rowid)
  , foreign key(tid) references transactions(rowid)
  ) |]

instance HasSchema Season where
  schema = Table [sql| create table if not exists seasons
  ( tid integer not null
  , thumbnail text not null
  , href text not null
  , foreign key(tid) references transactions(rowid)
  ) |]

data Schema a where
  Table :: Query -> Schema a
  (:+:) :: Schema a -> Schema b -> Schema ()

foldSchema :: Schema b -> (Query -> a -> a) -> a -> a
foldSchema (Table query) f init = f query init
foldSchema (left :+: right) f init = foldSchema right f $ foldSchema left f init

class HasSchema a where
  schema :: Schema a

class MonadIO m => HasDatabase m where
  getConnection :: m Connection

initSchema :: HasDatabase m => m ()
initSchema = do
  let queries = foldSchema dbSchema (:) []
  conn <- getConnection
  liftIO $ for_ queries (execute_ conn)


instance FromRow Episode where
  fromRow = Episode <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> (T.splitOn "|" <$> (field :: RowParser Text))

instance ToRow Episode where
  toRow (Episode a b c d e f g h i) =
    [toField a, toField b, toField c, toField d, toField e, toField f, toField g, toField h, toField $ T.intercalate "|" i]

instance FromRow Season where
  fromRow = Season <$> field <*> field <*> field

instance ToRow Season where
  toRow (Season a b c) = [toField a, toField b, toField c]
