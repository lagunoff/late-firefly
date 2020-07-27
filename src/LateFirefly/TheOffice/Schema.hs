module LateFirefly.TheOffice.Schema where

import Flat
import LateFirefly.DB
import LateFirefly.Prelude

data Season = Season
  { uuid      :: ~(UUID5 Season)
  , version   :: Id Transaction
  , deleted   :: Bool
  , thumbnail :: Text
  , number    :: Int }
  deriving stock (Show, Eq, Generic)
  deriving anyclass Flat

-- deriveDbUUID ["number"] ''Season

data Episode = Episode
  { uuid        :: ~(UUID5 Episode)
  , version     :: Id Transaction
  , deleted     :: Bool
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

-- deriveDbUUID ["seasonId", "code"] ''Episode
