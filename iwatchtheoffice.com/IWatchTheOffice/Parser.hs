{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
module Main where

import Control.DeepSeq           (deepseq, force)
import Control.Exception         (SomeException)
import Control.Exception         (evaluate)
import Control.Monad             (void, (>=>))
import Control.Monad.Except      (MonadError, throwError)
import Control.Monad.Except      (runExceptT)
import Control.Monad.Fail        (MonadFail)
import Control.Monad.IO.Class    (MonadIO, liftIO)
import Control.Monad.Reader      (MonadReader, asks, local, runReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Foldable             (for_)
import Data.Maybe                (fromMaybe)
import Data.Semigroup            ((<>))
import Data.Time.Clock.POSIX     (getCurrentTime)
import Data.Traversable          (for)
import Database.MongoDB          (AccessMode, Action, Database, Host, ObjectId,
                                  Selection (..),
                                  Value (Doc, Null, ObjId, ObjId), access,
                                  close, connect, createCollection, host,
                                  insert, master, modify, (=:))
import IWatchTheOffice.Db        (Episode (..), Season (..))
import Network.HTTP.Client       (Manager, httpLbs, newManager, parseRequest,
                                  responseBody)
import Network.HTTP.Client.TLS   (tlsManagerSettings)
import Options.Applicative
import Text.HTML.Scalpel         (AttributeName (..), Scraper, Selector,
                                  TagName (..), URL, hasClass, scrape,
                                  tagSelector, (//), (@:), (@=))
import Text.HTML.TagSoup         (Tag, parseTags)

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Text            as T
import qualified Text.HTML.Scalpel    as Scalpel

-- | Cli options
data Cli = Cli
  { cliAction :: CliAction
  }

-- | Cli actions
data CliAction
  = Update
  | Demo
  | Goodbye

-- | Command line parser
opts :: ParserInfo Cli
opts = info (cli)
  ( progDesc "Print a greeting for TARGET"
  <> header "hello - a test for optparse-applicative"
  ) where
    cli = Cli <$> actionParser
    actionParser =
      subparser
      ( command "update" (info (pure Update) (progDesc "Update website data"))
        <> command "goodbye" (info (pure Goodbye) (progDesc "Say goodbye"))
        <> command "demo" (info (pure Demo) (progDesc "Run demo"))
      )

type Scraper' a = Scraper String a
type Tag' = Tag T.Text
data Ctx = Ctx { manager :: Manager, url :: Maybe URL }
type App m = (MonadIO m, MonadError Err m, MonadReader Ctx m)

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  ethr <- runExceptT $ flip runReaderT (Ctx manager Nothing) $ process
  either (putStrLn . ("Finished with error: " <>) . show) (const $ putStrLn "Ok") ethr

process :: (MonadFail m, App m) => m ()
process = do
  options <- liftIO $ execParser opts
  case cliAction options of
    Update -> do
      startedAt <- liftIO getCurrentTime
      seasons <- scrapeSeasons
      withPipe (host "127.0.0.1") "telikov" master $ do
        let collections = ["iwatchtheoffice-transactions", "iwatchtheoffice-seasons", "iwatchtheoffice-episodes"]
        for_ collections $ createCollection []
        transactionId <- insert "iwatchtheoffice-transactions" ["startedAt" =: startedAt, "finishedAt" =: Null]
        liftIO . putStrLn $ "show trId: " <> show transactionId
        for_ seasons $ \season -> do
          let Season { _seasonCode, _seasonThumbnail, _seasonHref } = season
              seasonBson = ["transactionId" =: transactionId, "_seasonCode" =: _seasonCode, "_seasonThumbnail" =: _seasonThumbnail, "_seasonHref" =: _seasonHref]
          seasonId <- insert "iwatchtheoffice-seasons" seasonBson
          for_ (_seasonEpisodes season) $ \episode -> do
            let Episode { _episodeCode, _episodeName, _episodeHref, _episodeShortDescription, _episodeThumbnail, _episodeDescription, _episodeLinks } = episode
                episodeBson = ["transactionId" =: transactionId, "seasonId" =: seasonId, "_episodeCode" =: _episodeCode,"_episodeName" =: _episodeName,"_episodeHref" =: _episodeHref,"_episodeShortDescription" =: _episodeShortDescription,"_episodeThumbnail" =: _episodeThumbnail,"_episodeDescription" =: _episodeDescription, "_episodeLinks" =: _episodeLinks]
            insert "iwatchtheoffice-episodes" episodeBson
        finishedAt <- liftIO getCurrentTime
        modify (Select ["_id" =: transactionId] "iwatchtheoffice-transactions") ["$set" =: Doc ["finishedAt" =: finishedAt]]
    Goodbye ->
      liftIO $ putStrLn "Goodbye..."
    Demo -> do
      liftIO $ putStrLn "Not implemented"
      withPipe (host "127.0.0.1") "telikov" master $ do
        let transactionId = read "5d7379421356600408000000" :: ObjectId
        finishedAt <- liftIO getCurrentTime
        modify (Select ["_id" =: ObjId transactionId] "iwatchtheoffice-transactions") ["$set" =: Doc ["finishedAt" =: finishedAt]]

scrapeSeasons :: App m => m [Season]
scrapeSeasons = local (\ctx -> ctx { url = Just url }) $ do
  html <- parseTags <$> httpGet url
  seasonsRaw <- scrapeHTMLs html (TagString "div" @: [AttributeString "id" @= "outer"])
  for (parseTags <$> seasonsRaw) $ scrapeSeason >=> liftIO . evaluate
  where
    url = "https://iwatchtheoffice.com/season-list/" :: URL

scrapeSeason :: App m => [Tag'] -> m Season
scrapeSeason content = do
  _seasonHref <- attr1 content (tagSelector "a") "href"
  _seasonThumbnail <- attr1 content (tagSelector "img") "src"
  inner_remaining_l <- scrapeHTML content (TagString "div" @: [AttributeString "id" @= "inner_remaining_l"])
  _seasonCode <- scrapeText (parseTags inner_remaining_l) Scalpel.anySelector
  _seasonEpisodes <- scrapeEpisodes $ "https://iwatchtheoffice.com" <> T.unpack _seasonHref
  pure $ Season { _seasonHref, _seasonThumbnail, _seasonCode, _seasonEpisodes }

scrapeEpisodes :: App m => URL -> m [Episode]
scrapeEpisodes url = local (\ctx -> ctx { url = Just url }) $ do
  html <- parseTags <$> httpGet url
  episodesRaw <- scrapeHTMLs html (TagString "div" @: [AttributeString "id" @= "outer"])
  for (parseTags <$> episodesRaw) $ scrapeEpisode >=> liftIO . evaluate

scrapeEpisode :: App m => [Tag'] -> m Episode
scrapeEpisode content = do
  _episodeHref <- attr1 content (tagSelector "a") "href"
  _episodeThumbnail <- attr1 content (tagSelector "img") "src"
  _episodeCode <- scrapeText content (TagString "div" @: [AttributeString "id" @= "inner_remaining_l"])
  _episodeName <- scrapeText content (TagString "div" @: [AttributeString "id" @= "inner_remaining_r"])
  _episodeShortDescription <- scrapeText content (TagString "div" @: [AttributeString "id" @= "inner_remaining_r2"])
  let episodeUrl = "https://iwatchtheoffice.com" <> T.unpack _episodeHref
  local (\ctx -> ctx { url = Just $ episodeUrl }) $ do
    html <- parseTags <$> httpGet episodeUrl
    _episodeLinks <- attrs1 html (TagString "div" @: [hasClass "linkz_box"] // tagSelector "a") "href"
    _episodeDescription <- scrapeText html (TagString "div" @: [hasClass "description_box"])
    pure $ Episode { _episodeHref, _episodeThumbnail, _episodeCode, _episodeName, _episodeShortDescription, _episodeLinks, _episodeDescription }

scrapeHTML :: (MonadError Err m, MonadReader Ctx m) => [Tag'] -> Selector -> m T.Text
scrapeHTML content selector = do
  url <- asks (fromMaybe "" . url)
  let maybeA = scrape (Scalpel.html selector) content
  maybe (throwError $ SelectorFailed url selector) pure maybeA

scrapeHTMLs :: (MonadError Err m, MonadReader Ctx m) => [Tag'] -> Selector -> m [T.Text]
scrapeHTMLs content selector = do
  url <- asks (fromMaybe "" . url)
  let maybeA = scrape (Scalpel.htmls selector) content
  maybe (throwError $ SelectorFailed url selector) pure maybeA

scrapeText :: (MonadError Err m, MonadReader Ctx m) => [Tag'] -> Selector -> m T.Text
scrapeText content selector = do
  url <- asks (fromMaybe "" . url)
  let maybeA = scrape (Scalpel.text selector) content
  maybe (throwError $ SelectorFailed url selector) pure maybeA

attr1 :: (MonadError Err m, MonadReader Ctx m) => [Tag'] -> Selector -> String -> m T.Text
attr1 content selector name  = do
  url <- asks (fromMaybe "" . url)
  let maybeA = scrape (Scalpel.attr name selector) content
  maybe (throwError $ AttrFailed url selector name) pure maybeA

attrs1 :: (MonadError Err m, MonadReader Ctx m) => [Tag'] -> Selector -> String -> m [T.Text]
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

httpGet :: App m => URL -> m T.Text
httpGet url = do
  liftIO $ putStrLn $ "Processing " <> url <> "..."
  man <- asks manager
  url' <- either (throwError . HTTPError) pure (parseRequest url)
  resp <- liftIO $ httpLbs url' man
  pure . T.pack . UTF8.toString . BS.toStrict $ responseBody resp

withPipe :: MonadIO m => Host -> Database -> AccessMode -> Action m a -> m a
withPipe host db mode action = do
  pipe <- liftIO $ connect host
  a <- access pipe mode db action
  liftIO $ close pipe
  pure a
