{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Main where

import Control.Exception (SomeException)
import Control.Lens (ix, only, traverse, (&), (^.), (^..))
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import Data.Int (Int64)
import Data.Semigroup ((<>))
import Data.Traversable (for)
import Database.SQLite.Simple (Connection, Only (..), withConnection)
import GHC.Generics (Generic)
import Options.Generic (ParseRecord(..), getRecord)
import Parser.TheOffice.Db (Episode (..), Season (..), initSchema)
import Telikov.Effects (Members, CurrentTime, Http, SQL, Sem, currentTime,
                        execute, http2IO, httpGet, lastInsertRowId, responseBody,
                        runM, sql2IO, time2IO)
import Text.HTML.TagSoup.Lens (allAttributed, allElements, allNamed, attrOne,
                               attributed, children, contents, named, _DOM)
import Data.Generics.Product

import qualified Data.Text as T
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.Encoding as Lazy

type URL = String

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
      let program :: Members '[SQL, Http, CurrentTime] r => Sem r ()
          program = do
            initSchema
            startedAt <- currentTime
            tid <- execute "insert into transactions (started_at) values (?)" (Only startedAt) *> lastInsertRowId
            seasons <- scrapeSeasons tid
            for_ seasons $ \(season, seasonId) -> do
              scrapeEpisodes tid seasonId season
            finishedAt <- currentTime
            execute "update transactions set finished_at=(?) where rowid=(?)" (finishedAt, tid)
      withConnection dbpath $ \conn -> program
        & sql2IO conn
        & time2IO
        & http2IO
        & runM

    InitSchema { dbpath } -> do
      withConnection dbpath $ \conn -> initSchema & sql2IO conn & runM
      
    GoodBye ->
      liftIO $ putStrLn "Goodbye..."

scrapeSeasons :: Members '[SQL, Http] r => Int64 -> Sem r [(Season, Int64)]
scrapeSeasons tid = do
  markup <- Lazy.decodeUtf8 . (^.responseBody) <$> httpGet "https://iwatchtheoffice.com/season-list/"
  let seasonOuters = markup^.._DOM.traverse.allAttributed(ix "id" . traverse . only "outer")
  for seasonOuters $ \el -> do
    let href      = Lazy.toStrict $ el^.allElements.named(only "a").attrOne "href"
    let thumbnail = Lazy.toStrict $ el^.allElements.named(only "img").attrOne "src"
    let season          = Season {..}
    seasonId <- execute "insert into seasons (tid, thumbnail, href) values (?,?,?)" season *> lastInsertRowId
    pure (season, seasonId)

scrapeEpisodes :: Members '[SQL, Http] r => Int64 -> Int64 -> Season -> Sem r [(Episode, Int64)]
scrapeEpisodes tid season_id season = do
  markup <- Lazy.decodeUtf8 . (^.responseBody) <$> httpGet ("https://iwatchtheoffice.com" <> T.unpack (season ^. field @"href"))
  let episodeOuters = markup^.._DOM.traverse.allAttributed(ix "id" . traverse . only "outer")
  for episodeOuters $ \el -> do
    let href              = Lazy.toStrict $ el^.allElements.named(only "a").attrOne "href"
    let thumbnail         = Lazy.toStrict $ el^.allElements.named(only "img").attrOne "src"
    let code              = Lazy.toStrict $ el^.allElements.named(only "div").attributed(ix "id" . traverse . only "inner_remaining_l").contents
    let name              = Lazy.toStrict $ el^.allElements.named(only "div").attributed(ix "id" . traverse . only "inner_remaining_r").contents
    let short_description = Lazy.toStrict $ el^.allElements.named(only "div").attributed(ix "id" . traverse . only "inner_remaining_r2").contents
    markup2 <- Lazy.decodeUtf8 . (^.responseBody) <$> httpGet ("https://iwatchtheoffice.com" <> T.unpack (season ^. field @"href"))
    let links       = fmap Lazy.toStrict $ markup2^.._DOM.traverse.allAttributed(ix "class" . traverse . only "linkz_box").children.traverse.allNamed(only "a").attrOne "href"
    let description = Lazy.toStrict $ markup2^._DOM.traverse.allAttributed(ix "class" . traverse . only "description_box").contents
    let episode            = Episode {..}
    episodeId <- execute "insert into episodes (tid, season_id, code, name, href, short_description, thumbnail, description, links) values (?, ?, ?, ?, ?, ?, ?, ?, ?)" episode *> lastInsertRowId
    pure (episode, episodeId)

data Err
  = HTTPError SomeException
  deriving (Show)
