{-# LANGUAGE DuplicateRecordFields #-}
module Parser.TheOffice.Db where

import Data.Aeson (eitherDecodeStrict', FromJSON, ToJSON, encode)
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (traverse_)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Typeable (Typeable)
import Database.SQLite.Simple (ToRow(..), FromRow(..), Query(..), SQLData (..))
import Database.SQLite.Simple.Internal (Field(..))
import Database.SQLite.Simple.FromField (FromField (..), ResultError (..),
                                         returnError)
import Database.SQLite.Simple.Generics
import Database.SQLite.Simple.Ok (Ok (..))
import Database.SQLite.Simple.QQ (sql)
import Database.SQLite.Simple.ToField (ToField (..))
import GHC.Generics (from, to, Generic)
import Telikov.Effects (Member, SQL, Sem, execute_, RowID(..))
import Control.Lens ((&))


data Episode = Episode
  { version           :: RowID "transactions"
  , season_id         :: RowID "seasons"
  , code              :: Text
  , name              :: Text
  , href              :: Text
  , short_description :: Text
  , thumbnail         :: Text
  , description       :: Text
  , links             :: [Text]
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Season = Season
  { version   :: RowID "transactions"
  , thumbnail :: Text
  , href      :: Text
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Whole schema
dbSchema :: Query
dbSchema = [sql|
  create table if not exists transactions (
    started_at  integer not null,
    finished_at integer default null
  );

  create table seasons (
    id        integer not null,
    version   integer not null,
    deleted   integer not null default 0,
    thumbnail text    not null,
    href      text    not null,
    foreign key(version) references transactions(rowid),
    primary key(id, version)
  );
  
  create table if not exists episodes (
    id        integer not null,
    version   integer not null,
    deleted   integer not null default 0,
    season_id integer not null,
    code      text    not null,
    name      text    not null,
    href      text    not null,
    short_description text not null,
    thumbnail   text not null,
    description text not null,
    links       text not null,
    foreign key(season_id) references seasons(id),
    foreign key(version)   references transactions(rowid),
    primary key(id, version)
  );

  create view if not exists seasons_latest as
  select id, thumbnail, href from seasons
  where version=(select max(rowid) from transactions where finished_at not null);
    
  create view if not exists episodes_latest as
  select id, season_id, code, name, href, short_description, thumbnail, description, links from episodes
  where version=(select max(rowid) from transactions where finished_at not null);
    
  create virtual table if not exists seasons_fts using fts5(href, content='seasons_latest');
  create virtual table if not exists episodes_fts using fts5(name, description, content='episodes_latest');
  |]

initSchema :: Member SQL r => Sem r ()
initSchema = dbSchema
  & fromQuery
  & T.splitOn ";" -- FIXME: need proper statement splitting
  & fmap T.strip
  & filter ((/= 0) . T.length)
  & traverse_ (execute_ . Query)

instance {-# OVERLAPPING #-} (Typeable a, FromJSON a) => FromField [a] where
  fromField field@(Field (SQLText txt) _) = case (eitherDecodeStrict' (T.encodeUtf8 txt)) of
    Right val -> Ok val
    Left err -> returnError ConversionFailed field err
  fromField f = returnError ConversionFailed f "need a text"

instance {-# OVERLAPPING #-} (ToJSON a) => ToField [a] where
  toField = SQLText . T.decodeUtf8 . BSL.toStrict . encode

instance FromRow Season  where fromRow = to <$> gFromRow
instance ToRow   Season  where toRow   = gToRow . from
instance FromRow Episode where fromRow = to <$> gFromRow
instance ToRow   Episode where toRow   = gToRow . from
