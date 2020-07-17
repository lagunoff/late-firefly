module LateFirefly.IMDB.Schema where

import Flat
import Data.Map as M
import LateFirefly.DB
import LateFirefly.Prelude

type Genre = Text
type ImdbId = Text

data SearchItem = SearchItem
  { uuid        :: ~(UUID5 SearchItem)
  , version     :: Id Transaction
  , deleted     :: Bool
  , imdbId      :: ImdbId
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

deriveDbUUID ["imdbId"] ''SearchItem

data ImdbTitle = ImdbTitle
  { uuid        :: ~(UUID5 ImdbTitle)
  , version     :: Id Transaction
  , deleted     :: Bool
  , imdbId      :: ImdbId
  , poster      :: Maybe Text
  , summary     :: Text
  , storyline   :: Maybe Text
  , ratingValue :: Int
  , ratingCount :: Int
  , seasons     :: Map Text ImdbId }
  deriving stock (Show, Eq, Generic)
  deriving anyclass Flat

deriveDbUUID ["imdbId"] ''ImdbTitle

data ImdbEpisode = ImdbEpisode
  { uuid        :: ~(UUID5 ImdbTitle)
  , version     :: Id Transaction
  , deleted     :: Bool
  , imdbId      :: ImdbId
  , parent      :: ImdbId
  , season      :: Text
  , episode     :: Text }
  deriving stock (Show, Eq, Generic)
  deriving anyclass Flat

deriveDbUUID ["imdbId"] ''ImdbEpisode
