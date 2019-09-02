{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass, CPP #-}
module IWatchTheOffice.Db where

import GHC.Generics (Generic)
import Database.SQLite.Simple.ToRow
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToField
import Data.List.Split (splitOn)
import Data.List (intercalate)
import Data.Aeson

data Episode = Episode
  { episode_code              :: !String
  , episode_name              :: !String
  , episode_href              :: !String
  , episode_short_description :: !String
  , episode_thumbnail         :: !String
  , episode_description       :: !String
  , episode_links             :: ![String]
  } deriving (Show, Generic)

instance FromRow Episode where
  fromRow = Episode <$> field <*> field <*> field <*> field <*> field <*> field <*> (splitOn "|" <$> (field :: RowParser String))
instance ToJSON Episode
instance FromJSON Episode

instance ToRow Episode where
  toRow (Episode a b c d e f g) =
    [toField a, toField b, toField c, toField d, toField e, toField f, toField $ intercalate "|" g]

data Season = Season
  { season_code      :: !String
  , season_thumbnail :: !String
  , season_href      :: !String
  , season_episodes  :: ![Episode]
  } deriving (Show, Generic)

instance FromRow Season where
  fromRow = Season <$> field <*> field <*> field <*> pure []

instance ToRow Season where
  toRow (Season a b c _) = [toField a, toField b, toField c]
  
instance ToJSON Season
instance FromJSON Season
