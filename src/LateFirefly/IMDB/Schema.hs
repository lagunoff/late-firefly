{-# OPTIONS_GHC -Wno-orphans #-}
module LateFirefly.IMDB.Schema where

import Flat hiding (to)
import Control.Lens
import Data.Generics.Product
import Data.Map as M
import Data.List as L
import LateFirefly.DB
import LateFirefly.Prelude
import LateFirefly.Aeson
import LateFirefly.IMDB.GraphQL
import Data.Monoid.Generic
--import Database.SQLite.Simple

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
  deriving anyclass Flat

data ImdbImage = ImdbImage {
  rowid :: Id ImdbImage,
  url :: Maybe Text,
  height :: Maybe Int,
  width :: Maybe Int }
  deriving stock (Show, Eq, Generic)

data ImdbPlot = ImdbPlot {
  rowid :: Tid ImdbPlot,
  title_id :: Id ImdbTitle,
  plot_text :: Markdown,
  plot_type :: PlotType,
  language :: DisplayableLanguage,
  isSpoiler :: Bool,
  author :: Maybe Text }
  deriving stock (Show, Eq, Generic)

data ImdbSeries = ImdbSeries
  { title_id :: Id ImdbTitle
  , episode_number :: Maybe Int
  , season_number :: Maybe Int
  , next_episode :: Maybe (Id ImdbTitle)
  , previous_episode :: Maybe (Id ImdbTitle) }
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

data ImdbName = ImdbName {
  rowid :: Id ImdbName,
  primaryImage :: Maybe (Id ImdbImage),
  nameText :: Text }
  deriving stock (Show, Eq, Generic)

data ImdbCredit = ImdbCredit {
  name_id :: Id ImdbName,
  title_id :: Id ImdbTitle,
  category :: CreditCategory }
  deriving stock (Show, Eq, Generic)

data ImdbTitle = ImdbTitle
  { rowid :: Id ImdbTitle
  , primaryImageId :: Maybe (Id ImdbImage)
  , plotId :: Maybe (Tid ImdbPlot)
  , countriesOfOrigin :: Maybe CountriesOfOrigin
  , releaseYear :: Maybe YearRange
  , titleType :: Maybe TitleType
  , originalTitleText :: Maybe TitleText
  , releaseDate :: Maybe ReleaseDate
  , runtime :: Maybe Runtime
  , certificate :: Maybe Certificate
  , series :: Maybe ImdbSeries
  , userRating :: Maybe Rating
  }
  deriving stock (Show, Eq, Generic)

data TitleChunk = TitleChunk
  { title :: [ImdbTitle]
  , images :: [ImdbImage]
  , plots :: [ImdbPlot]
  , genres :: [ImdbGenre]
  , titleToGenre :: [ImdbTitleToGenre]
  , keywords :: [ImdbKeyword]
  , titleToKeyword :: [ImdbTitleToKeyword]
  , credits :: [ImdbCredit]
  , names :: [ImdbName] }
  deriving stock (Show, Eq, Generic)
  deriving Semigroup via GenericSemigroup TitleChunk
  deriving Monoid via GenericMonoid TitleChunk

fromTitle :: Title -> TitleChunk
fromTitle Title
  { images=imgs, plots=pls, genres=gnrs, keywords=kws, credits=crs, ..}
  = TitleChunk [title] images plots genres titleToGenre keywords titleToKeyword
    credits names
  where
    title = ImdbTitle
      { rowid = coerce id
      , primaryImageId = fmap (Id . unImdbId . getField @"id") primaryImage
      , plotId = fmap (Tid . getField @"id") plot
      , series = fmap toSeries series
      , .. }
    images = pim <> nim where
      pim = primaryImage ^.. _Just . to toImage
      nim = crs ^.. _Just . field @"edges" . traverse . field @"node" . field @"name" . field @"primaryImage" . _Just . to toImage
    plots = pls ^.. _Just . field @"edges" . traverse . field @"node" . to (toPlot id)
    genres = gnrs ^.. _Just . field @"genres" . traverse . to toGenre
    titleToGenre = genres <&> (ImdbTitleToGenre (Id (unImdbId id)) . getField @"rowid")
    keywords = kws ^.. _Just . field @"edges" . traverse . field @"node" . to toKeyword
    titleToKeyword = keywords <&> (ImdbTitleToKeyword (Id (unImdbId id)) . getField @"rowid")
    credits = crs ^.. _Just . field @"edges" . traverse . field @"node" . to (toCredit id)
    names = crs ^.. _Just . field @"edges" . traverse . field @"node" . field @"name" . to toName

    toImage Image{..} = ImdbImage
      {rowid = coerce id, ..}

    toPlot tid Plot{..} = ImdbPlot
      { rowid = Tid id
      , title_id = coerce tid
      , plot_text = plotText
      , plot_type = plotType, .. }

    toCredit tid Credit{..} = ImdbCredit
      { name_id = name ^. field @"id" . coerced
      , title_id = coerce tid, .. }

    toName Name{..} = ImdbName
      { rowid = coerce id
      , nameText = nameText ^. field @"text"
      , primaryImage = primaryImage ^? _Just . field @"id" . coerced }

    toSeries Series{..} = ImdbSeries
      { title_id = series ^. field @"id" . to (Id . unImdbId)
      , episode_number = episodeNumber ^? _Just . field @"episodeNumber"
      , season_number = episodeNumber ^? _Just . field @"seasonNumber"
      , next_episode = nextEpisode ^? _Just . field @"id" . to (Id . unImdbId)
      , previous_episode = previousEpisode ^? _Just . field @"id" . to (Id . unImdbId) }

    toGenre Genre{..} = ImdbGenre{rowid = coerce id, ..}

    toKeyword Keyword{..} = ImdbKeyword{rowid=coerce id, ..}

deriveRow ''ImdbSeries def
deriveRow ''DisplayableLanguage def
deriveRow ''InterestScore def {renameField = underscore}
deriveRow ''CountriesOfOrigin def {renameField = underscore}
deriveRow ''YearRange def {renameField = underscore}
deriveDb ''ImdbSearch def
deriveDb ''ImdbImage def {renameField = stripPrefix1 "_"}
deriveDb ''ImdbPlot def
deriveDb ''ImdbGenre def {renameField = underscore}
deriveDb ''ImdbName def {renameField = underscore}
deriveDb ''ImdbKeyword def {renameField = underscore}
deriveDb ''ImdbTitleToGenre def {pkeys = ["imdb_genre_id", "imdb_title_id"]}
deriveDb ''ImdbTitleToKeyword def {pkeys = ["imdb_keyword_id", "imdb_title_id"]}
deriveDb ''ImdbCredit def {pkeys = ["name_id", "title_id"]}

deriveDb ''ImdbTitle def {renameField = underscore, fields = [
  ("series", CstgRow),
  ("countriesOfOrigin", CstgRow),
  ("releaseYear", CstgRow) ]}

instance FromField Markdown where fromField f = Markdown <$> fromField @Text f
instance ToField Markdown where toField (Markdown t) = toField t
instance DbField Markdown where columnInfo _ = columnInfo (Proxy::Proxy Text)

deriving via JsonField DisplayableLanguage instance FromField DisplayableLanguage
deriving via JsonField DisplayableLanguage instance ToField DisplayableLanguage
deriving via JsonField DisplayableLanguage instance DbField DisplayableLanguage

deriving via JsonField TitleType instance FromField TitleType
deriving via JsonField TitleType instance ToField TitleType
deriving via JsonField TitleType instance DbField TitleType

deriving via JsonField TitleText instance FromField TitleText
deriving via JsonField TitleText instance ToField TitleText
deriving via JsonField TitleText instance DbField TitleText

deriving via JsonField ReleaseDate instance FromField ReleaseDate
deriving via JsonField ReleaseDate instance ToField ReleaseDate
deriving via JsonField ReleaseDate instance DbField ReleaseDate

deriving via JsonField Runtime instance FromField Runtime
deriving via JsonField Runtime instance ToField Runtime
deriving via JsonField Runtime instance DbField Runtime

deriving via JsonField Certificate instance FromField Certificate
deriving via JsonField Certificate instance ToField Certificate
deriving via JsonField Certificate instance DbField Certificate

deriving via JsonField Rating instance FromField Rating
deriving via JsonField Rating instance ToField Rating
deriving via JsonField Rating instance DbField Rating

deriving via JsonField InterestScore instance FromField InterestScore
deriving via JsonField InterestScore instance ToField InterestScore
deriving via JsonField InterestScore instance DbField InterestScore

deriving via JsonField CreditCategory instance FromField CreditCategory
deriving via JsonField CreditCategory instance ToField CreditCategory
deriving via JsonField CreditCategory instance DbField CreditCategory

deriving via JsonField AnyId instance FromField AnyId
deriving via JsonField AnyId instance ToField AnyId
deriving via JsonField AnyId instance DbField AnyId
