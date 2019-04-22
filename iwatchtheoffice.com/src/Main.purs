module Main where

import Prelude

import Affjax (get)
import Affjax.ResponseFormat as Format
import Cheerio (Cheerio, attr, find, text, toArray)
import Cheerio.Static (loadRoot)
import Control.Monad.Error.Class (class MonadThrow, withResource)
import Control.Monad.Except (runExceptT, throwError)
import Data.Array (foldM, length, (!!))
import Data.Either (Either(..), either)
import Data.Maybe (maybe)
import Data.Traversable (traverse, traverse_)
import Debug.Trace (traceM)
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Console (log, logShow)
import Err (Err(..))
import Prim.Boolean (False, True)
import QueryDsl (Column, Table, asc, from, insertInto, limit, makeTable, orderBy, select, where_)
import QueryDsl.Expressions ((:==))
import QueryDsl.SQLite3 (runQuery, runSelectManyQuery)
import SQLite3 (DBConnection, closeDB, newDB)


type Series =
  { code :: String
  , name :: String
  , href :: String
  , short_description :: String
  , thumbnail :: String
  , description :: String
  }
    
type Season =
  { code :: String
  , thumbnail :: String
  , href ∷ String
  , series :: Array Series
  }

series = makeTable "iwatchtheoffice_series" ∷ Table
  ( id ∷ Column Int False
  , code :: Column String True
  , name :: Column String True
  , href :: Column String True
  , short_description :: Column String True
  , thumbnail :: Column String True
  , description :: Column String True
  )

seasons = makeTable "iwatchtheoffice_seasons" ∷ Table
  ( id :: Column Int False
  , code :: Column String True
  , href :: Column String True
  , thumbnail :: Column String True
  )


process ∷ Aff (Either Err Unit)
process = runExceptT $ liftAff (newDB "../db.sqlite") `withResource` (liftAff <<< closeDB) $ \db → do
  let listUrl = "https://iwatchtheoffice.com/season-list/"
  resp ← liftAff $ get Format.string listUrl
  body ← either (throwError <<< ResponseFormatError) pure resp.body
  let root = loadRoot body
  entries ← findNonEmpty listUrl root ".entry-content > div"
  traceM $ entries # map text
  seasons ← traverse (parseSeason listUrl) entries
  traverse_ (liftAff <<< insertSeason db) seasons
  series ← parseSeries "/season-2/"
  traceM seasons
  traceM series
  pure unit

main01 :: Effect Unit
main01 = process # runAff_ case _ of
  Left err → log "Async Error occured" *> traceM err
  Right (Left err) → log "Error occured" *> traceM err
  Right (Right _) → log "All ended up fine"

main = main01


parseSeason ∷ ∀ m. MonadThrow Err m ⇒ String → Cheerio → m Season
parseSeason url root = do
  a ← find1 url root "a"
  href ← attr0 url a "href"
  img ← find1 url root "img"
  thumbnail ← attr0 url img "src"
  inner_remaining_l ← find1 url root "#inner_remaining_l"
  let code = text inner_remaining_l
  pure { code, thumbnail, href, series: [] }
  
parseSeriesEntry ∷ ∀ m. MonadThrow Err m ⇒ String → Cheerio → m Series
parseSeriesEntry url root = do
  a ← find1 url root "a"
  href ← attr0 url a "href"
  img ← find1 url root "img"
  thumbnail ← attr0 url img "src"
  inner_remaining_l ← find1 url root "#inner_remaining_l"
  let code = text inner_remaining_l
  inner_remaining_r ← find1 url root "#inner_remaining_r"
  let name = text inner_remaining_r
  inner_remaining_r2 ← find1 url root "#inner_remaining_r2"
  let short_description = text inner_remaining_r2
  pure { code, thumbnail, href, short_description, name, description: "" }

parseSeries
  ∷ ∀ m
  . MonadThrow Err m
  ⇒ MonadAff m
  ⇒ String
  → m (Array Series)
parseSeries href = do
  let url = "https://iwatchtheoffice.com" <> href
  resp ← liftAff $ get Format.string url
  body ← either (throwError <<< ResponseFormatError) pure resp.body
  let root = loadRoot body
  entries ← findNonEmpty url root ".entry-content > div#outer"
  traverse (parseSeriesEntry url) entries
  

findNonEmpty ∷ ∀ m. MonadThrow Err m ⇒ String → Cheerio → String → m (Array Cheerio)
findNonEmpty url root selector = do
  let entries = find selector root # toArray
  if length entries == 0 then throwError (EmptySelectorResults { url, selector }) else pure unit
  pure entries

find1 ∷ ∀ m. MonadThrow Err m ⇒ String → Cheerio → String → m Cheerio
find1 url root selector = do
  let entries = find selector root # toArray
      first = entries !! 0
  maybe (throwError (EmptySelectorResults { url, selector })) pure first
  
attr0 ∷ ∀ m. MonadThrow Err m ⇒ String → Cheerio → String → m String
attr0 url root attribute = do
  maybe (throwError $ EmptyAttribute { url, attribute }) pure (attr attribute root)
  

-- selectP db =
--   runSelectManyQuery db do
--     c <- from seasons
--     pure $ select { lastName: "" }
--       `where_` (c.id :== 1)
--       `orderBy` [asc c.id]
--       `limit` 10

-- process02 :: Aff Unit
-- process02 = do
--   newDB "../db.sqlite" `withResource` closeDB $ \db → do
--     insertQ db

-- main02 :: Effect Unit
-- main02 = process02 # runAff_ case _ of
--   Left err → log "Async Error occured" *> traceM err
--   Right _ → log "Aff completed without errors"
  
insertSeason :: DBConnection -> Season → Aff Unit
insertSeason db {code, thumbnail, href} =
  runQuery db $ insertInto seasons {code, thumbnail, href}
