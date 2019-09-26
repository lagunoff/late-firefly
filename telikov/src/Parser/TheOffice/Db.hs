{-# LANGUAGE DuplicateRecordFields       #-}
module Parser.TheOffice.Db where

import Data.Aeson (eitherDecodeStrict', FromJSON, ToJSON, encode)
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (for_)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Typeable (Typeable)
import Database.SQLite.Simple (ToRow(..), FromRow(..), Query, SQLData (..))
import Database.SQLite.Simple.Internal (Field(..))
import Database.SQLite.Simple.FromField (FromField (..), ResultError (..),
                                         returnError)
import Database.SQLite.Simple.Generics
import Database.SQLite.Simple.Ok (Ok (..))
import Database.SQLite.Simple.QQ (sql)
import Database.SQLite.Simple.ToField (ToField (..))
import GHC.Generics (from, to, Generic)
import Telikov.Effects (Member, SQL, Sem, execute_)

data Episode = Episode
  { tid               :: Int64
  , season_id         :: Int64
  , code              :: Text
  , name              :: Text
  , href              :: Text
  , short_description :: Text
  , thumbnail         :: Text
  , description       :: Text
  , links             :: [Text]
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Season = Season
  { tid       :: Int64
  , thumbnail :: Text
  , href      :: Text
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Whole schema
dbSchema :: [Query]
dbSchema =
  [ [sql|create table if not exists transactions
      ( started_at integer not null
      , finished_at integer default null
      )
    |]
  , [sql|create table if not exists episodes
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
      )
    |]
  , [sql| create table if not exists seasons
      ( tid integer not null
      , thumbnail text not null
      , href text not null
      , foreign key(tid) references transactions(rowid)
      )
    |]
  ]

initSchema :: Member SQL r => Sem r ()
initSchema = for_ dbSchema execute_

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
