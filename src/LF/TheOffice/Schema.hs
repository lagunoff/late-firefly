module LF.TheOffice.Schema where

import LF.Prelude
import LF.DB
import Flat.Rpc

data Season = Season
  { uuid      :: UUID5 Season
  , version   :: Id Transaction
  , thumbnail :: Text
  , number    :: Int }
  deriving stock (Show, Eq, Generic)
  deriving anyclass Flat

deriveDbUUID ["number"]''Season

data Episode = Episode
  { uuid        :: UUID5 Episode
  , version     :: Id Transaction
  , seasonId    :: UUID5 Season
  , code        :: Text
  , name        :: Text
  , href        :: Text
  , shortDesc   :: Text
  , thumbnail   :: Text
  , description :: Text
  , links       :: [Text] }
  deriving stock (Show, Eq, Generic)
  deriving anyclass Flat

deriveDbUUID ["seasonId", "code"] ''Episode
