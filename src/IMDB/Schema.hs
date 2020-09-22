{-# OPTIONS_GHC -Wno-orphans #-}
module IMDB.Schema where

import Control.Lens
import Data.Generics.Product
import Data.Map as M
import Data.Monoid.Generic
import Web.Slug

import "this" DB
import "this" IMDB.GraphQL
import "this" IMDB.Types
import "this" Intro

type Genre1 = Text

data ImdbSearch = ImdbSearch
  { rowid       :: Id ImdbSearch
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

data TitleBasicsTsv = TitleBasicsTsv
  { rowid          :: Id TitleBasicsTsv
  , titleType      :: Text
  , primaryTitle   :: Text
  , originalTitle  :: Text
  , isAdult        :: Bool
  , startYear      :: Maybe Int
  , endYear        :: Maybe Int
  , runtimeMinutes :: Maybe Int }
  deriving stock (Show, Eq, Generic)

data TitleEpisodeTsv = TitleEpisodeTsv
  { rowid         :: Id TitleEpisodeTsv
  , parentId      :: Id TitleBasicsTsv
  , seasonNumber  :: Maybe Int
  , episodeNumber :: Maybe Int }
  deriving stock (Show, Eq, Generic)

data TitleRatingsTsv = TitleRatingsTsv
  { rowid         :: Id TitleRatingsTsv
  , averageRating :: Double
  , numVotes      :: Int }
  deriving stock (Show, Eq, Generic)

data ImdbImage = ImdbImage {
  rowid :: Id ImdbImage,
  url :: Maybe Text,
  height :: Maybe Int,
  width :: Maybe Int }
  deriving stock (Show, Eq, Generic)

data ImdbPlot = ImdbPlot {
  rowid :: Tid ImdbPlot,
  title_id :: Id ImdbTitle,
  plot_text :: Text,
  plot_type :: PlotType,
  isSpoiler :: Bool,
  author :: Maybe Text }
  deriving stock (Show, Eq, Generic)

data ImdbGenre = ImdbGenre {
  rowid :: Tid ImdbGenre,
  text :: Text }
  deriving stock (Show, Eq, Generic)

data ImdbTitleToGenre = ImdbTitleToGenre {
  imdb_title_id :: Id ImdbTitle,
  imdb_genre_id :: Tid ImdbGenre }
  deriving stock (Show, Eq, Generic)

data ImdbKeyword = ImdbKeyword {
  rowid :: Id ImdbKeyword,
  text :: Text }
  deriving stock (Show, Eq, Generic)

data ImdbTitleToKeyword = ImdbTitleToKeyword {
  imdb_title_id :: Id ImdbTitle,
  imdb_keyword_id :: Id ImdbKeyword }
  deriving stock (Show, Eq, Generic)

data ImdbTitle = ImdbTitle
  { rowid               :: Id ImdbTitle
  , primary_image_id    :: Maybe (Id ImdbImage)
  , plot_id             :: Maybe (Tid ImdbPlot)
  , original_title_text :: Maybe Text
  , url_slug            :: Maybe Text }
  deriving stock (Show, Eq, Generic)

data TitleChunk = TitleChunk
  { title :: [ImdbTitle]
  , images :: [ImdbImage]
  , plots :: [ImdbPlot]
  , genres :: [ImdbGenre]
  , titleToGenre :: [ImdbTitleToGenre]
  , keywords :: [ImdbKeyword]
  , titleToKeyword :: [ImdbTitleToKeyword] }
  deriving stock (Show, Eq, Generic)
  deriving Semigroup via GenericSemigroup TitleChunk
  deriving Monoid via GenericMonoid TitleChunk

class ToDb a b | a -> b where
  toDb :: a -> b

urlSlug :: Id ImdbTitle -> Text -> Maybe Text
urlSlug (Id ii) = fmap (pre . unSlug) . mkSlug where
  pre x = showt ii <> "-" <> x

instance ToDb Title TitleChunk where
  toDb Title{..} = TitleChunk
    { title = pure ImdbTitle
      { rowid = coerce id
      , primary_image_id = fmap (Id . unImdbId . getField @"id") primaryImage
      , plot_id = fmap (Tid . getField @"id") plot
      , original_title_text = original_title_text
      , url_slug = urlSlug tid =<< original_title_text }
    , images = catMaybes [fmap toDb primaryImage]
    , plots = plots ^.. _Just . field @"edges" . traverse . field @"node" . to (flip toDb  tid)
    , genres = genres'
    , titleToGenre = ImdbTitleToGenre tid . getField @"rowid" <$> genres'
    , keywords = keywords'
    , titleToKeyword = ImdbTitleToKeyword tid . getField @"rowid" <$> keywords' }
    where
      tid = Id (unImdbId id)
      original_title_text = originalTitleText ^? _Just . field @"text"
      genres' = genres ^.. _Just . field @"genres" . traverse . to toDb
      keywords' = keywords ^.. _Just . field @"edges" . traverse . field @"node" . to toDb

instance ToDb Image ImdbImage where
  toDb Image{..} = ImdbImage {rowid = coerce id, ..}

instance ToDb Plot (Id ImdbTitle -> ImdbPlot) where
  toDb Plot{..} tid = ImdbPlot
    { rowid = Tid id
    , title_id = coerce tid
    , plot_text = plotText ^. field @"markdown"
    , plot_type = plotType, .. }

instance ToDb Genre ImdbGenre where
  toDb Genre{..} = ImdbGenre{rowid = coerce id, ..}

instance ToDb Keyword ImdbKeyword where
  toDb Keyword{..} = ImdbKeyword{rowid = coerce id, ..}

deriveDb ''TitleEpisodeTsv def {renameField=underscore}
deriveDb ''TitleRatingsTsv def {renameField=underscore}
deriveDb ''TitleBasicsTsv def {renameField=underscore}

deriveDb ''ImdbSearch def
deriveDb ''ImdbImage def
deriveDb ''ImdbPlot def
deriveDb ''ImdbGenre def
deriveDb ''ImdbKeyword def
deriveDb ''ImdbTitleToGenre def {pkeys = ["imdb_genre_id", "imdb_title_id"]}
deriveDb ''ImdbTitleToKeyword def {pkeys = ["imdb_keyword_id", "imdb_title_id"]}
deriveDb ''ImdbTitle def
