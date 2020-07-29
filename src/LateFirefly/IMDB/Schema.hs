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
  { rowid       :: Tid ImdbSearch
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
  width :: Maybe Int,
  caption :: Maybe Markdown,
  copyright :: Maybe Text,
  created_by :: Maybe Text,
  source :: Maybe Source,
  _type :: Maybe Text,
  names :: Maybe [NameId],
  countries :: Maybe [DisplayableCountry],
  languages :: Maybe [DisplayableLanguage] }
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
  , episode_number :: Int
  , season_number :: Int
  , next_episode :: Maybe (Id ImdbTitle)
  , previous_episode :: Maybe (Id ImdbTitle) }
  deriving stock (Show, Eq, Generic)

data ImdbTitleQuote = ImdbTitleQuote {
  rowid :: Id ImdbTitleQuote,
  titleId :: Id ImdbTitle,
  isSpoiler :: Bool,
  lines :: [TitleQuoteLine],
  interestScore :: InterestScore,
  language :: DisplayableLanguage }
  deriving stock (Show, Eq, Generic)

data ImdbTitleTrivia = ImdbTitleTrivia {
  rowid :: Id ImdbTitleTrivia,
  titleId :: Id ImdbTitle,
  text :: Markdown,
  isSpoiler :: Bool,
  triviaType :: Maybe Text,
  interestScore :: InterestScore,
  trademark :: Maybe Text,
  relatedNames :: Maybe [NameId] }
  deriving stock (Show, Eq, Generic)

data ImdbNews = ImdbNews {
  rowid :: Id ImdbNews,
  titleId :: Id ImdbTitle,
  articleTitle :: Markdown,
  externalUrl :: Text,
  source :: NewsSource,
  date :: Date,
  text :: Markdown,
  imageId :: Maybe AnyId,
  byline :: Maybe Text,
  language :: Maybe DisplayableLanguage }
  deriving stock (Show, Eq, Generic)

-- Details of a single goof
data ImdbGoof = ImdbGoof {
  rowid :: Id ImdbGoof,
  titleId :: Id ImdbTitle,
  text :: Markdown,
  isSpoiler :: Bool,
  interestScore :: InterestScore,
  category :: GoofCategory }
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

data ImdbReview = ImdbReview {
  rowid :: Id ImdbReview,
  titleId :: Maybe (Id ImdbTitle),
  author :: Maybe UserProfile,
  authorRating :: Maybe Int,
  helpfulness :: Maybe ReviewHelpfulness,
  language :: Maybe Language,
  spoiler :: Maybe Bool,
  submissionDate :: Maybe Date,
  summary :: Maybe ReviewSummary,
  text :: Maybe ReviewText }
  deriving stock (Show, Eq, Generic)

data ImdbTitle = ImdbTitle
  { rowid :: Id ImdbTitle
  , primaryImageId :: Maybe (Id ImdbImage)
  , plotId :: Maybe (Tid ImdbPlot)
  , countriesOfOrigin :: Maybe CountriesOfOrigin
  , releaseYear :: Maybe YearRange
  , externalLinks :: [ExternalLink]
  , taglines :: [Tagline]
  , alternateVersions :: [AlternateVersion]
  , awardNominations :: [AwardNomination]
  , faqs :: [Faq]
  , titleType :: Maybe TitleType
  , titleText :: Maybe TitleText
  , originalTitleText :: Maybe TitleText
  , releaseDate :: Maybe ReleaseDate
  , runtime :: Maybe Runtime
  , certificate :: Maybe Certificate
  , series :: Maybe ImdbSeries
  , userRating :: Maybe Rating
  }
  deriving stock (Show, Eq, Generic)

data ImdbTitleToImage = ImdbTitleToImage {
  imdb_image_id :: Id ImdbImage,
  imdb_title_id :: Id ImdbTitle }
  deriving stock (Show, Eq, Generic)

data TitleChunk = TitleChunk
  { title :: [ImdbTitle]
  , images :: [ImdbImage]
  , titleToImage :: [ImdbTitleToImage]
  , plots :: [ImdbPlot]
  , quotes :: [ImdbTitleQuote]
  , trivia :: [ImdbTitleTrivia]
  , news :: [ImdbNews]
  , goofs :: [ImdbGoof]
  , genres :: [ImdbGenre]
  , titleToGenre :: [ImdbTitleToGenre]
  , keywords :: [ImdbKeyword]
  , titleToKeyword :: [ImdbTitleToKeyword]
  , reviews :: [ImdbReview]
  }
  deriving stock (Show, Eq, Generic)
  deriving Semigroup via GenericSemigroup TitleChunk
  deriving Monoid via GenericMonoid TitleChunk

fromTitle :: Title -> TitleChunk
fromTitle Title
  { images=imgs, plots=pls, quotes=quos, trivia=triv, news=ns
  , goofs=gfs, genres=gnrs, keywords=kws, reviews=rvs, ..}
  = TitleChunk [title] images titleToImage plots quotes trivia news goofs
    genres titleToGenre keywords titleToKeyword reviews
  where
    title = ImdbTitle
      { rowid = coerce id
      , primaryImageId = fmap (Id . unImdbId . getField @"id") primaryImage
      , plotId = fmap (Tid . getField @"id") plot
      , series = fmap toSeries series
      , externalLinks = externalLinks ^.. _Just . field @"edges" . traverse . field @"node"
      , taglines = taglines ^.. _Just . field @"edges" . traverse . field @"node"
      , alternateVersions = alternateVersions ^.. _Just . field @"edges" . traverse . field @"node"
      , awardNominations = awardNominations ^.. _Just . field @"edges" . traverse . field @"node"
      , faqs = faqs ^.. _Just . field @"edges" . traverse . field @"node"
      , ..
      }
    images = pims <> tims where
      pims = primaryImage ^.. _Just . to toImage
      tims = imgs ^.. _Just . field @"edges" . traverse . field @"node" . to toImage
    plots = pls ^.. _Just . field @"edges" . traverse . field @"node" . to (toPlot id)
    quotes = quos ^.. _Just . field @"edges" . traverse . field @"node" . to (toQuote id)
    trivia = triv ^.. _Just . field @"edges" . traverse . field @"node" . to (toTitleTrivia id)
    news = ns ^.. _Just . field @"edges" . traverse . field @"node" . to (toNews id)
    goofs = gfs ^.. _Just . field @"edges" . traverse . field @"node" . to (toGoof id)
    genres = gnrs ^.. _Just . field @"genres" . traverse . to toGenre
    titleToGenre = genres <&> (ImdbTitleToGenre (Id (unImdbId id)) . getField @"rowid")
    keywords = kws ^.. _Just . field @"edges" . traverse . field @"node" . to toKeyword
    titleToKeyword = keywords <&> (ImdbTitleToKeyword (Id (unImdbId id)) . getField @"rowid")
    reviews = rvs ^.. _Just . field @"edges" . traverse . field @"node" . to (toReview id)

    titleToImage = let
      ims = imgs ^.. _Just . field @"edges" . traverse . field @"node"
      in join $ ims <&> \Image{..} -> titles ^.. _Just . traverse . field @"id" . to (ImdbTitleToImage (Id (unImdbId id)) . Id . unImdbId)

    toImage Image{..} = ImdbImage
      {rowid = coerce id, created_by = createdBy, ..}

    toPlot tid Plot{..} = ImdbPlot
      { rowid = Tid id
      , title_id = coerce tid
      , plot_text = plotText
      , plot_type = plotType, .. }

    toSeries Series{..} = ImdbSeries
      { title_id = series ^. field @"id" . to (Id . unImdbId)
      , episode_number = episodeNumber ^. field @"episodeNumber"
      , season_number = episodeNumber ^. field @"seasonNumber"
      , next_episode = nextEpisode ^? _Just . field @"id" . to (Id . unImdbId)
      , previous_episode = previousEpisode ^? _Just . field @"id" . to (Id . unImdbId) }

    toQuote tid TitleQuote{..} = ImdbTitleQuote
      { rowid = coerce id, titleId = coerce tid, .. }

    toTitleTrivia tid TitleTrivia{..} = ImdbTitleTrivia
      { rowid = coerce id, titleId = coerce tid, .. }

    toGoof tid Goof{..} = ImdbGoof{rowid = coerce id, titleId = coerce tid, ..}

    toGenre Genre{..} = ImdbGenre{rowid = coerce id, ..}

    toKeyword Keyword{..} = ImdbKeyword{rowid=coerce id, ..}

    toReview tid Review{..} = ImdbReview{rowid = coerce id, titleId = Just (coerce tid), ..}

toNews :: _
toNews tid News{..} = ImdbNews
  { rowid = coerce id
  , titleId = coerce tid
  , imageId = image
  , .. }

deriveRow ''ImdbSeries def
deriveRow ''DisplayableLanguage def
deriveRow ''InterestScore def {renameField = underscore}
deriveRow ''CountriesOfOrigin def {renameField = underscore}
deriveRow ''YearRange def {renameField = underscore}
deriveDb ''ImdbSearch def
deriveDb ''ImdbImage def {renameField = stripPrefix1 "_"}
deriveDb ''ImdbPlot def
deriveDb ''ImdbTitleTrivia def {renameField = underscore, fields = [("interestScore", CstgRow)]}
deriveDb ''ImdbNews def {renameField = underscore}
deriveDb ''ImdbGoof def {renameField = underscore}
deriveDb ''ImdbGenre def {renameField = underscore}
deriveDb ''ImdbKeyword def {renameField = underscore}
deriveDb ''ImdbReview def {renameField = underscore}
deriveDb ''ImdbTitleToImage def {pkeys = ["imdb_image_id", "imdb_title_id"]}
deriveDb ''ImdbTitleToGenre def {pkeys = ["imdb_genre_id", "imdb_title_id"]}
deriveDb ''ImdbTitleToKeyword def {pkeys = ["imdb_keyword_id", "imdb_title_id"]}
deriveDb ''ImdbTitleQuote def {renameField = underscore, fields = [("interestScore", CstgRow)]}

deriveDb ''ImdbTitle def {renameField = underscore, fields = [
  ("series", CstgRow),
  ("countriesOfOrigin", CstgRow),
  ("releaseYear", CstgRow) ]}

deriving via JsonField Markdown instance FromField Markdown
deriving via JsonField Markdown instance ToField Markdown
deriving via JsonField Markdown instance DbField Markdown

deriving via JsonField Source instance FromField Source
deriving via JsonField Source instance ToField Source
deriving via JsonField Source instance DbField Source

deriving via JsonField DisplayableLanguage instance FromField DisplayableLanguage
deriving via JsonField DisplayableLanguage instance ToField DisplayableLanguage
deriving via JsonField DisplayableLanguage instance DbField DisplayableLanguage

deriving via JsonField ExternalLink instance FromField ExternalLink
deriving via JsonField ExternalLink instance ToField ExternalLink
deriving via JsonField ExternalLink instance DbField ExternalLink

deriving via JsonField NewsSource instance FromField NewsSource
deriving via JsonField NewsSource instance ToField NewsSource
deriving via JsonField NewsSource instance DbField NewsSource

deriving via JsonField AlternateVersion instance FromField AlternateVersion
deriving via JsonField AlternateVersion instance ToField AlternateVersion
deriving via JsonField AlternateVersion instance DbField AlternateVersion

deriving via JsonField AwardNomination instance FromField AwardNomination
deriving via JsonField AwardNomination instance ToField AwardNomination
deriving via JsonField AwardNomination instance DbField AwardNomination

deriving via JsonField Faq instance FromField Faq
deriving via JsonField Faq instance ToField Faq
deriving via JsonField Faq instance DbField Faq

deriving via JsonField TitleType instance FromField TitleType
deriving via JsonField TitleType instance ToField TitleType
deriving via JsonField TitleType instance DbField TitleType

deriving via JsonField TitleText instance FromField TitleText
deriving via JsonField TitleText instance ToField TitleText
deriving via JsonField TitleText instance DbField TitleText

deriving via JsonField GoofCategory instance FromField GoofCategory
deriving via JsonField GoofCategory instance ToField GoofCategory
deriving via JsonField GoofCategory instance DbField GoofCategory

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

deriving via JsonField UserProfile instance FromField UserProfile
deriving via JsonField UserProfile instance ToField UserProfile
deriving via JsonField UserProfile instance DbField UserProfile

deriving via JsonField ReviewSummary instance FromField ReviewSummary
deriving via JsonField ReviewSummary instance ToField ReviewSummary
deriving via JsonField ReviewSummary instance DbField ReviewSummary

deriving via JsonField ReviewHelpfulness instance FromField ReviewHelpfulness
deriving via JsonField ReviewHelpfulness instance ToField ReviewHelpfulness
deriving via JsonField ReviewHelpfulness instance DbField ReviewHelpfulness

deriving via JsonField ReviewText instance FromField ReviewText
deriving via JsonField ReviewText instance ToField ReviewText
deriving via JsonField ReviewText instance DbField ReviewText

deriving via JsonField AnyId instance FromField AnyId
deriving via JsonField AnyId instance ToField AnyId
deriving via JsonField AnyId instance DbField AnyId
