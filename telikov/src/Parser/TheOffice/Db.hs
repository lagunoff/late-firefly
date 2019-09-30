{-# LANGUAGE DuplicateRecordFields #-}
module Parser.TheOffice.Db where

import Data.Aeson (eitherDecodeStrict', FromJSON, ToJSON, encode)
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (traverse_)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Internal as BS
import Data.Typeable (Typeable)
import Database.SQLite.Simple (ToRow(..), FromRow(..), Query(..), SQLData (..))
import Database.SQLite.Simple.Internal (Field(..))
import Database.SQLite.Simple.FromField (FromField (..), ResultError (..),
                                         returnError)
import Telikov.Database hiding (Query)
import Database.SQLite.Simple.Ok (Ok (..))
import Database.SQLite.Simple.QQ (sql)
import Database.SQLite.Simple.ToField (ToField (..))
import GHC.Generics (from, to, Generic)
import Telikov.Effects (Member, SQL, Sem, execute_)
import Control.Lens ((&), (^.))
import Data.Generics.Product (field)


data Episode = Episode
  { season_id         :: UUID5 Season
  , code              :: Text
  , name              :: Text
  , href              :: Text
  , short_description :: Text
  , thumbnail         :: Text
  , description       :: Text
  , links             :: [Text]
  } deriving (Show, Eq, Generic, ToJSON, FromJSON, Persistent)

data Season = Season
  { thumbnail :: Text
  , href      :: Text
  } deriving (Show, Eq, Generic, ToJSON, FromJSON, Persistent)

-- | Whole schema
dbSchema :: Query
dbSchema = [sql|
  create table if not exists "transaction" (
    started_at  integer not null,
    finished_at integer default null,
    tag         text    default null
  );

  create table if not exists season (
    uuid      text    not null,
    version   integer not null,
    deleted   integer not null default 0,
    thumbnail text    not null,
    href      text    not null,
    foreign key(version) references "transaction"(rowid)
  );
  
  create table if not exists episode (
    uuid      text     not null,
    version   integer  not null,
    deleted   integer  not null default 0,
    season_id text     not null,
    code      text     not null,
    name      text     not null,
    href      text     not null,
    short_description  text not null,
    thumbnail   text not null,
    description text not null,
    links       text not null,
    foreign key(season_id) references season(uuid),
    foreign key(version)   references "transaction"(rowid)
  );

  create view if not exists season_latest as
  select uuid, thumbnail, href from season
  where version=(select max(rowid) from "transaction" where finished_at not null);
    
  create view if not exists episode_latest as
  select uuid, season_id, code, name, href, short_description, thumbnail, description, links from episode
  where version=(select max(rowid) from "transaction" where finished_at not null);
    
  create virtual table if not exists season_fts using fts5(href, content='season_latest', content_rowid='uuid');
  create virtual table if not exists episode_fts using fts5(name, short_description, description, content='episode_latest', content_rowid='uuid');
  |]

initSchema :: Member SQL r => Sem r ()
initSchema = dbSchema
  & fromQuery
  & T.splitOn ";" -- FIXME: need proper statement splitting
  & fmap T.strip
  & filter ((/= 0) . T.length)
  & traverse_ (execute_ . Query)

instance {-# OVERLAPPING #-} (Typeable a, FromJSON a) => FromField [a] where
  fromField f@(Field (SQLText txt) _) = case (eitherDecodeStrict' (T.encodeUtf8 txt)) of
    Right val -> Ok val
    Left err -> returnError ConversionFailed f err
  fromField f = returnError ConversionFailed f "need a text"

instance {-# OVERLAPPING #-} (ToJSON a) => ToField [a] where
  toField = SQLText . T.decodeUtf8 . BSL.toStrict . encode

instance FromRow  Season  where fromRow     = to <$> gFromRow
instance ToRow    Season  where toRow       = gToRow . from
instance HasUUID5 Season  where uuid5Object = BS.unpackBytes . T.encodeUtf8 . ("the-office:season-" <>) . (^. field @"href")
instance FromRow  Episode where fromRow     = to <$> gFromRow
instance ToRow    Episode where toRow       = gToRow . from
instance HasUUID5 Episode where uuid5Object = BS.unpackBytes . T.encodeUtf8 . ("the-office:episode-" <>) . (^. field @"href")
