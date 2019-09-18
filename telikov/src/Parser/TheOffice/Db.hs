{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeApplications      #-}
module Parser.TheOffice.Db
  ( Season(..)
  , Episode(..)
  , initSchema
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Foldable (for_)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple (Query)
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.QQ (sql)
import Database.SQLite.Simple.ToRow
import Telikov.Effects (SQL, execute_, Member, Eff)
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
dbSchema :: [Query]
dbSchema =
  pure [sql| create table if not exists transactions
    ( started_at integer not null
    , finished_at integer default null
    ) |]
  <> schema @Season
  <> schema @Episode

instance HasSchema Episode where
  schema = pure [sql| create table if not exists episodes
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
  schema = pure [sql| create table if not exists seasons
  ( tid integer not null
  , thumbnail text not null
  , href text not null
  , foreign key(tid) references transactions(rowid)
  ) |]

class HasSchema a where
  schema :: [Query]

initSchema :: Member SQL r => Eff r ()
initSchema = for_ dbSchema execute_

instance FromRow Episode where
  fromRow = Episode <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> (T.splitOn "|" <$> (field :: RowParser Text))

instance ToRow Episode where
  toRow (Episode a b c d e f g h i) = toRow (a, b, c, d, e, f, g, h, T.intercalate "|" i)

instance FromRow Season where
  fromRow = Season <$> field <*> field <*> field

instance ToRow Season where
  toRow (Season a b c) = toRow (a, b, c)
