module LateFirefly.IMDB.Schema where

import Flat
import Data.Map as M
import LateFirefly.DB
import LateFirefly.Prelude
import LateFirefly.Aeson

type Genre = Text

data Imdb

data ImdbSearch = ImdbSearch
  { rowid       :: Tid Imdb
  , version     :: Id Transaction
  , deleted     :: Bool
  , year        :: Maybe Text
  , popularity  :: Map Genre Int
  , header      :: Text
  , certificate :: Maybe Text
  , runtime     :: Maybe Text
  , genre       :: [Text]
  , rating      :: Maybe Text
  , text        :: Text
  , stars       :: [(Text, Text)]
  , thumbnail67x98 :: Maybe Text }
  deriving stock (Show, Eq, Generic)
  deriving anyclass Flat

deriveDb ''ImdbSearch

data ImdbDetail = ImdbDetail
  { rowid        :: Tid Imdb
  , version      :: Id Transaction
  , deleted      :: Bool
  , title        :: Text
  , posters      :: [Text]
  , summary      :: Text
  , storyline    :: Maybe Text
  , rating_value :: Int
  , rating_count :: Int }
  deriving stock (Show, Eq, Generic)
  deriving anyclass Flat

deriveDb ''ImdbDetail

data ImdbEpisode = ImdbEpisode
  { rowid   :: Tid Imdb
  , version :: Id Transaction
  , deleted :: Bool
  , parent  :: Tid Imdb
  , season  :: Text
  , episode :: Text }
  deriving stock (Show, Eq, Generic)
  deriving anyclass Flat

deriveDb ''ImdbEpisode
