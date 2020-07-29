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

data Imdb

data ImdbSearch = ImdbSearch
  { rowid       :: Tid Imdb
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
  rowid :: Tid ImdbImage,
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
  title_id :: Tid ImdbTitle,
  plot_text :: Markdown,
  plot_type :: PlotType,
  language :: DisplayableLanguage,
  isSpoiler :: Bool,
  author :: Maybe Text }
  deriving stock (Show, Eq, Generic)

data ImdbSeries = ImdbSeries
  { title_id :: Tid ImdbTitle
  , episode_number :: Int
  , season_number :: Int
  , next_episode :: Maybe (Tid ImdbTitle)
  , previous_episode :: Maybe (Tid ImdbTitle) }
  deriving stock (Show, Eq, Generic)

data ImdbTitleQuote = ImdbTitleQuote {
  rowid :: Tid ImdbTitleQuote,
  titleId :: Tid ImdbTitle,
  isSpoiler :: Bool,
  lines :: [TitleQuoteLine],
  interestScore :: InterestScore,
  language :: DisplayableLanguage }
  deriving stock (Show, Eq, Generic)

data ImdbTitleTrivia = ImdbTitleTrivia {
  rowid :: Tid ImdbTitleTrivia,
  titleId :: Tid ImdbTitle,
  text :: Markdown,
  isSpoiler :: Bool,
  triviaType :: Maybe Text,
  interestScore :: InterestScore,
  trademark :: Maybe Text,
  relatedNames :: Maybe [NameId] }
  deriving stock (Show, Eq, Generic)

data ImdbNews = ImdbNews {
  rowid :: Tid ImdbNews,
  titleId :: Tid ImdbTitle,
  articleTitle :: Markdown,
  externalUrl :: Text,
  source :: NewsSource,
  date :: Date,
  text :: Markdown,
  imageId :: Maybe (Tid ImdbImage),
  byline :: Maybe Text,
  language :: Maybe DisplayableLanguage }
  deriving stock (Show, Eq, Generic)

data ImdbTitle = ImdbTitle
  { rowid :: Tid ImdbTitle
  , primaryImageId :: Maybe (Tid ImdbImage)
  , plotId :: Maybe (Tid ImdbPlot)
  , series :: Maybe ImdbSeries
  , countriesOfOrigin :: Maybe CountriesOfOrigin
  , releaseYear :: Maybe YearRange
  , externalLinks :: [ExternalLink]
  , taglines :: [Tagline]
  , alternateVersions :: [AlternateVersion]
  , awardNominations :: [AwardNomination]
  , faqs :: [Faq]
  , titleType :: Maybe TitleType
  , titleText :: Maybe TitleText
  , originalTitleText :: Maybe TitleText }
  deriving stock (Show, Eq, Generic)

data ImdbTitleToImage = ImdbTitleToImage {
  imdb_image_id :: Tid ImdbImage,
  imdb_title_id :: Tid ImdbTitle }
  deriving stock (Show, Eq, Generic)

data TitleChunk = TitleChunk
  { title :: [ImdbTitle]
  , images :: [ImdbImage]
  , titleToImage :: [ImdbTitleToImage]
  , plots :: [ImdbPlot]
  , quotes :: [ImdbTitleQuote]
  , trivia :: [ImdbTitleTrivia]
  , news :: [ImdbNews] }
  deriving stock (Show, Eq, Generic)
  deriving Semigroup via GenericSemigroup TitleChunk
  deriving Monoid via GenericMonoid TitleChunk

fromTitle :: Title -> TitleChunk
fromTitle Title{images=imgs, plots=pls, quotes=quos, trivia=triv, news=ns,..} = TitleChunk [ImdbTitle
  { rowid = Tid id
  , primaryImageId = fmap (Tid . getField @"id") primaryImage
  , plotId = fmap (Tid . getField @"id") plot
  , series = fmap toSeries series
  , externalLinks = externalLinks ^.. _Just . field @"edges" . traverse . field @"node"
  , taglines = taglines ^.. _Just . field @"edges" . traverse . field @"node"
  , alternateVersions = alternateVersions ^.. _Just . field @"edges" . traverse . field @"node"
  , awardNominations = awardNominations ^.. _Just . field @"edges" . traverse . field @"node"
  , faqs = faqs ^.. _Just . field @"edges" . traverse . field @"node"
  , ..
  }] images titleToImage plots quotes trivia news
  where
    images = tims <> nims where
      tims = imgs ^.. _Just . field @"edges" . traverse . field @"node" . to toImage
      nims = ns ^.. _Just . field @"edges" . traverse . field @"node" . field @"image" . _Just . to toImage
    plots = pls ^.. _Just . field @"edges" . traverse . field @"node" . to (toPlot id)
    quotes = quos ^.. _Just . field @"edges" . traverse . field @"node" . to (toQuote id)
    trivia = triv ^.. _Just . field @"edges" . traverse . field @"node" . to (toTitleTrivia id)
    news = ns ^.. _Just . field @"edges" . traverse . field @"node" . to (toNews id)

    titleToImage = let
      ims = imgs ^.. _Just . field @"edges" . traverse . field @"node"
      in join $ ims <&> \Image{..} -> titles ^.. _Just . traverse . field @"id" . to (ImdbTitleToImage (Tid id) . Tid)

    toImage Image{..} = ImdbImage
      {rowid = Tid id, created_by = createdBy, ..}

    toPlot tid Plot{..} = ImdbPlot
      { rowid = Tid id
      , title_id = Tid tid
      , plot_text = plotText
      , plot_type = plotType, .. }

    toSeries Series{..} = ImdbSeries
      { title_id = series ^. field @"id" . to Tid
      , episode_number = episodeNumber ^. field @"episodeNumber"
      , season_number = episodeNumber ^. field @"seasonNumber"
      , next_episode = nextEpisode ^? _Just . field @"id" . to Tid
      , previous_episode = previousEpisode ^? _Just . field @"id" . to Tid }

    toQuote tid TitleQuote{..} = ImdbTitleQuote
      { rowid = Tid id, titleId = Tid tid, .. }

    toTitleTrivia tid TitleTrivia{..} = ImdbTitleTrivia
      { rowid = Tid id, titleId = Tid tid, .. }

toNews :: _
toNews tid News{..} = ImdbNews
  { rowid = Tid id
  , titleId = Tid tid
  , imageId = fmap (Tid . getField @"id") image
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
deriveDb ''ImdbTitleToImage def {pkeys = ["imdb_image_id", "imdb_title_id"]}
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
