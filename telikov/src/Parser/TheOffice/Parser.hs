{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Main where

import Control.Exception (SomeException)
import Control.Lens (ix, only, traverse, (&), (^.), (^..))
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import Polysemy.Reader (Reader)
import Data.Semigroup ((<>))
import Data.Traversable (for)
import Database.SQLite.Simple (Connection, withConnection)
import GHC.Generics (Generic)
import Options.Generic (ParseRecord(..), getRecord)
import Parser.TheOffice.Db (Episode (..), Season (..), initSchema)
import Telikov.Effects (withNewTransaction, uuid5, Transaction(..), insert, RowID, Embed, Members, CurrentTime, Http, SQL, Sem, http2IO, httpGet, responseBody, runM, sql2IO, time2IO)
import Text.HTML.TagSoup.Lens (allAttributed, allElements, allNamed, attrOne,
                               attributed, children, contents, named, _DOM)
import Data.Generics.Product (field)

import qualified Data.Text as T
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.Encoding as Lazy

type URL = String
type Scrape r = forall eff. Members '[Reader (RowID Transaction), SQL, Http, Embed IO] eff => Sem eff r

-- | Cli arguments
data Args
  = Update { dbpath :: String }
  | InitSchema { dbpath :: String }
  | GoodBye
  deriving (Generic, Show, ParseRecord)

data Ctx = Ctx
  { connection :: Connection
  } deriving (Generic)

main :: IO ()
main = do
  options <- getRecord "Parser for https://iwatchtheoffice.com/"
  case options of
    Update { dbpath } -> do
      let program :: Members '[SQL, Http, CurrentTime, Embed IO] r => Sem r ()
          program = do
            initSchema
            withNewTransaction do
              seasons <- scrapeSeasons
              for_ seasons scrapeEpisodes
      withConnection dbpath \conn -> program
        & sql2IO conn
        & time2IO
        & http2IO
        & runM
    InitSchema { dbpath } -> do
      withConnection dbpath \conn -> initSchema & sql2IO conn & runM
    GoodBye ->
      liftIO $ putStrLn "Goodbye..."

scrapeSeasons :: Scrape [Season]
scrapeSeasons = do
  markup <- Lazy.decodeUtf8 . (^.responseBody) <$> httpGet "https://iwatchtheoffice.com/season-list/"
  let seasonOuters = markup^.._DOM.traverse.allAttributed(ix "id" . traverse . only "outer")
  for seasonOuters \el -> do
    let href      = Lazy.toStrict $ el^.allElements.named(only "a").attrOne "href"
    let thumbnail = Lazy.toStrict $ el^.allElements.named(only "img").attrOne "src"
    let season    = Season {..}
    insert season *> pure season

scrapeEpisodes :: Season -> Scrape [Episode]
scrapeEpisodes season = do
  markup <- Lazy.decodeUtf8 . (^.responseBody) <$> httpGet ("https://iwatchtheoffice.com" <> T.unpack (season ^. field @"href"))
  let episodeOuters = markup^.._DOM.traverse.allAttributed(ix "id" . traverse . only "outer")
  for episodeOuters \el -> do
    let season_id         = uuid5 season
    let href              = Lazy.toStrict $ el^.allElements.named(only "a").attrOne "href"
    let thumbnail         = Lazy.toStrict $ el^.allElements.named(only "img").attrOne "src"
    let code              = Lazy.toStrict $ el^.allElements.named(only "div").attributed(ix "id" . traverse . only "inner_remaining_l").contents
    let name              = Lazy.toStrict $ el^.allElements.named(only "div").attributed(ix "id" . traverse . only "inner_remaining_r").contents
    let short_description = Lazy.toStrict $ el^.allElements.named(only "div").attributed(ix "id" . traverse . only "inner_remaining_r2").contents
    markup2 <- Lazy.decodeUtf8 . (^.responseBody) <$> httpGet ("https://iwatchtheoffice.com" <> T.unpack href)
    let links       = fmap Lazy.toStrict $ markup2^.._DOM.traverse.allAttributed(ix "class" . traverse . only "linkz_box").children.traverse.allNamed(only "a").attrOne "href"
    let description = Lazy.toStrict $ markup2^._DOM.traverse.allAttributed(ix "class" . traverse . only "description_box").contents
    let episode     = Episode {..}
    insert episode *> pure episode

data Err
  = HTTPError SomeException
  deriving (Show)
