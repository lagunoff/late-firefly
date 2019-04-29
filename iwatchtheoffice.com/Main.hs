{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
module Main where

import qualified Text.HTML.Scalpel as Scalpel
import Text.HTML.Scalpel (Selector, Scraper, URL, scrape, TagName(..), AttributeName(..), (@:), (@=), tagSelector, hasClass, (//))
import Control.Monad.Except (MonadError, throwError)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Control.Monad.IO.Class(MonadIO, liftIO)
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Lazy as BS
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (MonadReader, runReaderT, local, asks)
import Data.Maybe (fromMaybe)
-- import Debug.Trace (traceShowM)
import Control.Exception (SomeException)
import IWatchTheOffice.Db (Season(..), Episode(..), Db(..))
import Options.Applicative
import Data.Semigroup ((<>))
import Control.DeepSeq (deepseq, force)
import Text.HTML.TagSoup (Tag, parseTags)


data Cli = Cli
  { cliAction :: Action
  }

data Action
  = Update { output :: String }
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
      ( command "update" (info update (progDesc "Update website info"))
        <> command "goodbye" (info (pure Goodbye) (progDesc "Say goodbye"))
      )
    update = Update <$> strOption (long "output" <> short 'o' <> help "Write output to FILE")

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
  let options = Cli { cliAction = Update { output = "/tmp/Data.hs" } } -- liftIO $ execParser opts
  case cliAction options of
    Update { output } -> do
      db_seasons <- scrapeSeasons
      let db = Db { db_seasons }
      liftIO $ writeFile output (showDb db)
    Goodbye ->
      liftIO $ putStrLn "Goodbye..."
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
  mapM scrapeSeason (parseTags <$> seasonsRaw)
  where
    url = "https://iwatchtheoffice.com/season-list/"

scrapeSeason :: App m => [Tag'] -> m Season
scrapeSeason content = do
  season_href <- attr1 content (tagSelector "a") "href"
  season_thumbnail <- attr1 content (tagSelector "img") "data-cfsrc"
  inner_remaining_l <- scrapeHTML content (TagString "div" @: [AttributeString "id" @= "inner_remaining_l"])
  season_code <- scrapeText (parseTags inner_remaining_l) Scalpel.anySelector
  season_episodes <- scrapeEpisodes $ "https://iwatchtheoffice.com" <> season_href
  let season = Season { season_href, season_thumbnail, season_code, season_episodes }
  pure $ season `deepseq` season
  
scrapeEpisodes :: App m => URL -> m [Episode]
scrapeEpisodes url = local (\ctx -> ctx { url = Just url }) $ do
  html <- parseTags <$> httpGet url
  episodesRaw <- scrapeHTMLs html (TagString "div" @: [AttributeString "id" @= "outer"])
  mapM scrapeEpisode (parseTags <$> episodesRaw)
  
scrapeEpisode :: App m => [Tag'] -> m Episode
scrapeEpisode content = do
  episode_href <- attr1 content (tagSelector "a") "href"
  episode_thumbnail <- attr1 content (tagSelector "img") "data-cfsrc"
  episode_code <- scrapeText content (TagString "div" @: [AttributeString "id" @= "inner_remaining_l"])
  episode_name <- scrapeText content (TagString "div" @: [AttributeString "id" @= "inner_remaining_r"])
  episode_short_description <- scrapeText content (TagString "div" @: [AttributeString "id" @= "inner_remaining_r2"])
  let episodeUrl = "https://iwatchtheoffice.com" <> episode_href
  local (\ctx -> ctx { url = Just $ episodeUrl }) $ do
    html <- parseTags <$> httpGet episodeUrl
    episode_links <- attrs1 html (TagString "div" @: [hasClass "linkz_box"] // tagSelector "a") "href"
    episode_description <- scrapeText html (TagString "div" @: [hasClass "description_box"])
    pure $ force $ Episode { episode_href, episode_thumbnail, episode_code, episode_name, episode_short_description, episode_links, episode_description }

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
