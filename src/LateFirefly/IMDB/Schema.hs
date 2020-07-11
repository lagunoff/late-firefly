module LateFirefly.IMDB.Schema where

import Flat
import LateFirefly.DB
import LateFirefly.Prelude

data SearchItem = SearchItem
  { uuid        :: ~(UUID5 SearchItem)
  , version     :: Id Transaction
  , deleted     :: Bool
  , href        :: Text
  , index       :: Int
  , header      :: Text
  , certificate :: Maybe Text
  , runtime     :: Maybe Text
  , genre       :: [Text]
  , rating      :: Maybe Text
  , text        :: Text }
  deriving stock (Show, Eq, Generic)
  deriving anyclass Flat

deriveDbUUID ["href"] ''SearchItem
