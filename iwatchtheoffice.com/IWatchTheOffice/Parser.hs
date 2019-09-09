{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
module Main where

import Control.Exception              (SomeException)
import Control.Exception              (evaluate)
import Control.Lens                   hiding (children, element, elements)
import Control.Monad                  (void, (>=>))
import Control.Monad.Except           (ExceptT, MonadError, throwError)
import Control.Monad.Except           (runExceptT)
import Control.Monad.Fail             (MonadFail)
import Control.Monad.IO.Class         (MonadIO, liftIO)
import Control.Monad.Reader           (MonadReader, ReaderT, asks, local,
                                       runReaderT)
import Control.Monad.Trans.Class      (lift)
import Data.Foldable                  (for_)
import Data.Int                       (Int64)
import Data.Maybe                     (fromMaybe)
import Data.Semigroup                 ((<>))
import Data.Time.Clock.POSIX          (getCurrentTime)
import Data.Traversable               (for)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.QQ
import IWatchTheOffice.Db             (Episode (..), HasDatabase (..),
                                       Season (..), initSchema)
import Network.Wreq                   (get, responseBody, Response)
import Options.Applicative
import Text.HTML.TagSoup.Lens

import qualified Data.Text.Lazy          as Lazy
import qualified Data.Text.Lazy.Encoding as Lazy
import qualified Data.Text          as T
import qualified Data.ByteString.Lazy as L


type URL = String

-- | Cli options
data Cli = Cli
  { cliAction :: CliAction
  }

-- | Cli actions
data CliAction
  = Update { dbpath :: String }
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
      ( command "update" (info update (progDesc "Update website data"))
        <> command "goodbye" (info (pure Goodbye) (progDesc "Say goodbye"))
        <> command "demo" (info (pure Demo) (progDesc "Run demo"))
      )
    update = Update <$> strOption (long "dbpath" <> short 'd' <> help "SQLite3 database")

data Env = Env
  { envConnection :: Connection
  , envUrl        :: Maybe URL -- ^ Address of the current page
  }

type App = ReaderT Env (ExceptT Err IO)
type MonadApp m = (Monad m, MonadIO m, HasDatabase m, MonadError Err m)

instance HasDatabase App where
  getConnection = asks envConnection

class Monad m => HasHttp m where
  httpGet :: String -> m (Response L.ByteString)

instance HasHttp App where
  httpGet url = liftIO $ putStrLn ("fetching " <> url) *> get url
  
  
main :: IO ()
main = do
  options <- execParser opts
  case cliAction options of
    Update { dbpath } -> do
      startedAt <- liftIO getCurrentTime
      withConnection dbpath $ \conn -> do
        let env = Env conn Nothing
        runExceptT $ flip runReaderT env (initSchema :: App ())
        tid <- execute conn "insert into transactions (started_at) values (?)" (Only startedAt) *> lastInsertRowId conn
        void $ runExceptT $ flip runReaderT env $ do
          seasons <- scrapeSeasons tid :: App [(Season,Int64)]
          for seasons $ \(season, seasonId) -> do
            scrapeEpisodes tid seasonId season
          finishedAt <- liftIO getCurrentTime
          liftIO $ execute conn "update transactions set finished_at=(?) where rowid=(?)" (finishedAt, tid)
        
    Goodbye ->
      liftIO $ putStrLn "Goodbye..."
    Demo -> do
      liftIO $ putStrLn "Not implemented"

scrapeSeasons :: (HasDatabase m, HasHttp m) => Int64 -> m [(Season, Int64)]
scrapeSeasons seasonTid = do
  conn   <- getConnection
  markup <- Lazy.decodeUtf8 . (^.responseBody) <$> httpGet "https://iwatchtheoffice.com/season-list/"
  let seasonOuters = markup^.._DOM.traverse.allAttributed(ix "id" . traverse . only "outer")
  for seasonOuters $ \el -> do
    let seasonHref      = Lazy.toStrict $ el^.allElements.named(only "a").attrOne "href"
    let seasonThumbnail = Lazy.toStrict $ el^.allElements.named(only "img").attrOne "src"
    let season          = Season {..}
    seasonId <- liftIO $ execute conn "insert into seasons (tid, thumbnail, href) values (?,?,?)" season *> lastInsertRowId conn
    pure (season, seasonId)

scrapeEpisodes :: (HasDatabase m, HasHttp m) => Int64 -> Int64 -> Season -> m [(Episode, Int64)]
scrapeEpisodes episodeTid episodeSeasonId season = do
  conn <- getConnection
  markup <- Lazy.decodeUtf8 . (^.responseBody) <$> httpGet ("https://iwatchtheoffice.com" <> T.unpack (seasonHref season))
  let episodeOuters = markup^.._DOM.traverse.allAttributed(ix "id" . traverse . only "outer")
  for episodeOuters $ \el -> do
    let episodeHref             = Lazy.toStrict $ el^.allElements.named(only "a").attrOne "href"
    let episodeThumbnail        = Lazy.toStrict $ el^.allElements.named(only "img").attrOne "src"
    let episodeCode             = Lazy.toStrict $ el^.allElements.named(only "div").attributed(ix "id" . traverse . only "inner_remaining_l").contents
    let episodeName             = Lazy.toStrict $ el^.allElements.named(only "div").attributed(ix "id" . traverse . only "inner_remaining_r").contents
    let episodeShortDescription = Lazy.toStrict $ el^.allElements.named(only "div").attributed(ix "id" . traverse . only "inner_remaining_r2").contents
    markup2 <- Lazy.decodeUtf8 . (^.responseBody) <$> httpGet ("https://iwatchtheoffice.com" <> T.unpack episodeHref)
    let episodeLinks       = fmap Lazy.toStrict $ markup2^.._DOM.traverse.allAttributed(ix "class" . traverse . only "linkz_box").children.traverse.allNamed(only "a").attrOne "href"
    let episodeDescription = Lazy.toStrict $ markup2^._DOM.traverse.allAttributed(ix "class" . traverse . only "description_box").contents
    let episode            = Episode {..}
    episodeId <- liftIO $ execute conn "insert into episodes (tid, season_id, code, name, href, short_description, thumbnail, description, links) values (?, ?, ?, ?, ?, ?, ?, ?, ?)" episode *> lastInsertRowId conn
    pure (episode, episodeId)

data Err
  = HTTPError SomeException
  deriving (Show)
