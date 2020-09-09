module VidSrc.Scrape where

import Data.Aeson as AE
import Data.Typeable
import GHC.Stack
import Control.Exception
import Network.HTTP.Client

import "this" Intro
import "this" Scrape
import "this" DB
import "this" IMDB.Types

data ScrapeError
  = HttpException HttpException
  | CallStackException CallStack
  | SQLError SQLError
  deriving (Exception, Show, Generic)

data VidsrcResponce r = VidsrcResponce
  { pages  :: Int
  , result :: [r] }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data VidsrcEpisode = VidsrcEpisode
  { show_imdb_id :: ImdbId "tt"
  , show_title   :: Text
  , season       :: Text
  , episode      :: Text }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

deriveDb ''VidsrcEpisode def {pkeys = ["show_imdb_id", "season", "episode"]}

data VidsrcMovie = VidsrcMovie
  { imdb_id   :: ImdbId "tt"
  , quality   :: Maybe Text
  , title     :: Text }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

deriveDb ''VidsrcMovie def {pkeys = ["imdb_id"]}

scrapeVidsrc :: forall r. (FromJSON r, DbTable r, Typeable r) =>  IO ()
scrapeVidsrc = unEio $ withConnectionSetup $collectTables $ go 1 where
  go :: (?conn::Connection) => _
  go p = do
    VidsrcResponce{..} <- getJSON @(VidsrcResponce r) @ScrapeError $ url p
    for_ result upsert
    when (p < pages) do go (p + 1)
  url p
    | Just _ <- eqT @r @VidsrcMovie =
      "https://vidsrc.me/movies/latest/page-" <> show p <> ".json"
    | Just _ <- eqT @r @VidsrcEpisode =
      "https://vidsrc.me/episodes/latest/page-" <> show p <> ".json"
    | otherwise =
      error "`r` can be either VidsrcMovie or VidsrcEpisode"
