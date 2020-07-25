module LateFirefly.IMDB.Schema where

import Flat
import Data.Map as M
import Data.List as L
import LateFirefly.DB
import LateFirefly.Prelude
import LateFirefly.Aeson
import LateFirefly.IMDB.GraphQL
import Database.SQLite.Simple

type Genre1 = Text

data Imdb

data ImdbSearch = ImdbSearch
  { rowid       :: Tid Imdb
  , version     :: Id Transaction
  , deleted     :: Bool
  , year        :: Maybe Text
  , popularity  :: Map Genre1 Int
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

data ImdbPlot = ImdbPlot {
  id         :: Tid Plot,
  plot_text  :: Text,
  plot_type  :: PlotType,
  language   :: DisplayableLanguage,
  is_spoiler :: Bool,
  author     :: Maybe Text }
  deriving stock (Show, Eq, Generic)

deriveRow' ''ImdbPlot def {fields = [
  ("language", CstgRow)
  ]}

data ImdbTitle = ImdbTitle {
  rowid :: Tid ImdbTitle,
  version :: Id Transaction,
  deleted :: Bool,
  plot :: Maybe ImdbPlot
  }
  deriving stock (Show, Eq, Generic)

-- deriveDb' ''ImdbTitle def {fields = [("plot", CstgRow)]}
