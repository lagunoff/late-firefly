{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds, BangPatterns, QuasiQuotes #-}
module IWatchTheOffice.Parser where

import qualified Text.HTML.Scalpel as Scalpel
import Text.HTML.Scalpel (Selector, Scraper, URL, scrape, TagName(..), AttributeName(..), (@:), (@=), tagSelector, hasClass, (//))
import Control.Monad.Except (MonadError, throwError)
import Network.HTTP.Client hiding (withConnection)
import Network.HTTP.Client.TLS
import Control.Monad.IO.Class(MonadIO, liftIO)
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Lazy as BS
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (MonadReader, runReaderT, local, asks)
import Data.Maybe (fromMaybe)
-- import Debug.Trace (traceShowM)
import Control.Exception (SomeException)
import IWatchTheOffice.Db (Season(..), Episode(..))
import Options.Applicative
import Data.Semigroup ((<>))
import Control.DeepSeq (deepseq, force)
import Text.HTML.TagSoup (Tag, parseTags)
import Control.Monad (void, forM, forM_, (>=>))
import Control.Exception (evaluate)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.QQ
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getCurrentTime)

schema :: [Query]
schema = fmap Query $ filter ((/= 0) . T.length) $ map T.strip $ T.splitOn ";" . fromQuery $ [sql|
  create table if not exists updates
  ( started_at integer not null
  , finished_at integer default null
  );
  
  create table if not exists iwatchtheoffice_episodes
  ( update_id integer not null
  , season_id integer not null
  , code text not null
  , name text not null
  , href text not null
  , short_description text not null
  , thumbnail text not null
  , description text not null
  , links text not null
  , foreign key(season_id) references iwatchtheoffice_season(rowid)
  , foreign key(update_id) references updates(rowid)
  );

  create table if not exists iwatchtheoffice_seasons
  ( update_id integer not null
  , code text not null
  , thumbnail text not null
  , href text not null
  , foreign key(update_id) references updates(rowid)
  );
|]


data Cli = Cli
  { cliAction :: Action
  }

data Action
  = Update { dbpath :: String }
  | InitSchema { dbpath :: String }
  | Demo
  | Goodbye

opts :: ParserInfo Cli
opts = info (cli)
  ( progDesc "Print a greeting for TARGET"
  <> header "hello - a test for optparse-applicative"
  )
  where
    cli = Cli <$> actionParser
    actionParser =
      subparser
      ( command "update" (info update (progDesc "Update website data"))
        <> command "init-schema" (info initSchema (progDesc "Setup schema for a new database"))
        <> command "goodbye" (info (pure Goodbye) (progDesc "Say goodbye"))
        <> command "demo" (info (pure Demo) (progDesc "Run demo"))
      )
    update = Update <$> strOption (long "dbpath" <> short 'd' <> help "SQLite3 database")
    initSchema = InitSchema <$> strOption (long "dbpath" <> short 'd' <> help "SQLite3 database")

type Scraper' a = Scraper String a
type Tag' = Tag String
data Ctx = Ctx { manager :: Manager, url :: Maybe URL }
type App m = (MonadIO m, MonadError Err m, MonadReader Ctx m)

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  ethr <- runExceptT $ flip runReaderT (Ctx manager Nothing) $ process
  either (putStrLn . ("Finished with error: " <>) . show) (const $ putStrLn "Ok") ethr
  pure ()

process :: App m => m ()
process = do
  options <- liftIO $ execParser opts
  case cliAction options of
    Update { dbpath } -> do
      started_at <- liftIO getCurrentTime
      seasons <- scrapeSeasons
      liftIO $ withConnection dbpath $ \conn -> do
        forM_ schema $ execute_ conn
        update_id <- execute conn "insert into updates (started_at) values (?)" (Only started_at) *> lastInsertRowId conn
        forM_ seasons $ \season -> do
          season_id <- execute conn "insert into iwatchtheoffice_seasons (update_id, code, thumbnail, href) values (?,?,?,?)" (Only update_id :. season) *> lastInsertRowId conn
          forM_ (season_episodes season) $ \episode -> do
            execute conn "insert into iwatchtheoffice_episodes (update_id, season_id, code, name, href, short_description, thumbnail, description, links) values (?,?,?,?,?,?,?,?,?)" (Only update_id :. Only season_id :. episode)
        finished_at <- getCurrentTime
        execute conn "update updates set finished_at=(?) where id=(?)" (finished_at, update_id)
      pure ()
    InitSchema { dbpath } ->
      liftIO $ withConnection dbpath $ forM_ schema . execute_
    Goodbye ->
      liftIO $ putStrLn "Goodbye..."
    Demo -> do
      liftIO $ withConnection "test.db" $ \conn -> do
        forM_ schema $ execute_ conn
      liftIO $ putStrLn "There is no demo here, Bye!"

  where
    showDb db = unlines
      [ "module IWatchTheOffice.Db.Data (db) where"
      , "import IWatchTheOffice.Db"
      , ""
      , "db :: Db"
      , "db = " <> show db
      ]

scrapeSeasons :: App m => m [Season]
scrapeSeasons = local (\ctx -> ctx { url = Just url }) $ do
  html <- parseTags <$> httpGet url
  seasonsRaw <- scrapeHTMLs html (TagString "div" @: [AttributeString "id" @= "outer"])
  forM (parseTags <$> seasonsRaw) $ scrapeSeason >=> liftIO . evaluate . force
  where
    url = "https://iwatchtheoffice.com/season-list/"

scrapeSeason :: App m => [Tag'] -> m Season
scrapeSeason content = do
  season_href <- attr1 content (tagSelector "a") "href"
  season_thumbnail <- attr1 content (tagSelector "img") "src"
  inner_remaining_l <- scrapeHTML content (TagString "div" @: [AttributeString "id" @= "inner_remaining_l"])
  season_code <- scrapeText (parseTags inner_remaining_l) Scalpel.anySelector
  season_episodes <- scrapeEpisodes $ "https://iwatchtheoffice.com" <> season_href
  pure $ Season { season_href, season_thumbnail, season_code, season_episodes }

scrapeEpisodes :: App m => URL -> m [Episode]
scrapeEpisodes url = local (\ctx -> ctx { url = Just url }) $ do
  html <- parseTags <$> httpGet url
  episodesRaw <- scrapeHTMLs html (TagString "div" @: [AttributeString "id" @= "outer"])
  forM (parseTags <$> episodesRaw) $ scrapeEpisode >=> liftIO . evaluate . force

scrapeEpisode :: App m => [Tag'] -> m Episode
scrapeEpisode content = do
  episode_href <- attr1 content (tagSelector "a") "href"
  episode_thumbnail <- attr1 content (tagSelector "img") "src"
  episode_code <- scrapeText content (TagString "div" @: [AttributeString "id" @= "inner_remaining_l"])
  episode_name <- scrapeText content (TagString "div" @: [AttributeString "id" @= "inner_remaining_r"])
  episode_short_description <- scrapeText content (TagString "div" @: [AttributeString "id" @= "inner_remaining_r2"])
  let episodeUrl = "https://iwatchtheoffice.com" <> episode_href
  local (\ctx -> ctx { url = Just $ episodeUrl }) $ do
    html <- parseTags <$> httpGet episodeUrl
    episode_links <- attrs1 html (TagString "div" @: [hasClass "linkz_box"] // tagSelector "a") "href"
    episode_description <- scrapeText html (TagString "div" @: [hasClass "description_box"])
    pure $ Episode { episode_href, episode_thumbnail, episode_code, episode_name, episode_short_description, episode_links, episode_description }

scrapeHTML :: (MonadError Err m, MonadReader Ctx m) => [Tag'] -> Selector -> m String
scrapeHTML content selector = do
  url <- asks (fromMaybe "" . url)
  let maybeA = scrape (Scalpel.html selector) content
  maybe (throwError $ SelectorFailed url selector) pure maybeA

scrapeHTMLs :: (MonadError Err m, MonadReader Ctx m) => [Tag'] -> Selector -> m [String]
scrapeHTMLs content selector = do
  url <- asks (fromMaybe "" . url)
  let maybeA = scrape (Scalpel.htmls selector) content
  maybe (throwError $ SelectorFailed url selector) pure maybeA

scrapeText :: (MonadError Err m, MonadReader Ctx m) => [Tag'] -> Selector -> m String
scrapeText content selector = do
  url <- asks (fromMaybe "" . url)
  let maybeA = scrape (Scalpel.text selector) content
  maybe (throwError $ SelectorFailed url selector) pure maybeA

attr1 :: (MonadError Err m, MonadReader Ctx m) => [Tag'] -> Selector -> String -> m String
attr1 content selector name  = do
  url <- asks (fromMaybe "" . url)
  let maybeA = scrape (Scalpel.attr name selector) content
  maybe (throwError $ AttrFailed url selector name) pure maybeA

attrs1 :: (MonadError Err m, MonadReader Ctx m) => [Tag'] -> Selector -> String -> m [String]
attrs1 content selector name = do
  url <- asks (fromMaybe "" . url)
  let maybeA = scrape (Scalpel.attrs name selector) content
  maybe (throwError $ AttrFailed url selector name) pure maybeA

data Err
  = SelectorFailed URL Selector
  | AttrFailed URL Selector String
  | ScraperFailed URL
  | HTTPError SomeException
  deriving (Show)

instance Show Selector where
  show _ = "(Selector ..)"


httpGet :: App m => String -> m String
httpGet url = do
  liftIO $ putStrLn $ "Processing " <> url <> "..."
  man <- asks manager
  url' <- either (throwError . HTTPError) pure (parseRequest url)
  resp <- liftIO $ httpLbs url' man
  pure $ UTF8.toString $ BS.toStrict $ responseBody resp
