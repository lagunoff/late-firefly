module LateFirefly.IMDB.Schema where

import Flat
import Data.Map as M
import Data.List as L
import LateFirefly.DB
import LateFirefly.Prelude
import LateFirefly.Aeson

type Genre = Text

data Imdb

data ImdbSearch = ImdbSearch
  { rowid       :: Tid Imdb
  , version     :: Id Transaction
  , deleted     :: Bool
  , year        :: Maybe Text
  , popularity  :: Map Genre Int
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

deriveDb ''ImdbSearch

data ImdbDetail = ImdbDetail
  { rowid        :: Tid Imdb
  , version      :: Id Transaction
  , deleted      :: Bool
  , title        :: Text
  , posters      :: [Text]
  , summary      :: Text
  , storyline    :: Maybe Text
  , rating_value :: Int
  , rating_count :: Int }
  deriving stock (Show, Eq, Generic)
  deriving anyclass Flat

deriveDb ''ImdbDetail

data ImdbEpisode = ImdbEpisode
  { rowid   :: Tid Imdb
  , version :: Id Transaction
  , deleted :: Bool
  , parent  :: Tid Imdb
  , season  :: Text
  , episode :: Text }
  deriving stock (Show, Eq, Generic)
  deriving anyclass Flat

deriveDb ''ImdbEpisode

data PageInfo = PageInfo
  { hasNextPage :: Bool }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data GQLConnection a = GQLConnection
  { edges    :: [Edge a]
  , pageInfo :: PageInfo }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

type Many0 a = Maybe (GQLConnection a)
type Many a = GQLConnection a

data Edge a = Edge
  { node :: a }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data DisplayableCountry = DisplayableCountry {
  -- The country code - either an ISO 3166 code or an internally defined code if no ISO code exists for that country.
  -- Cache TTL in seconds: 900
  id :: Tid DisplayableCountry,
  -- Display text for the country (e.g. 'United States').
  -- Cache TTL in seconds: 900
  text :: Text }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data DisplayableLanguage = DisplayableLanguage {
  -- The language code - either an ISO 639 code or an internally defined code if no ISO code exists for the language.
  -- Cache TTL in seconds: 900
  id :: Tid DisplayableLanguage,
  -- Display text for the language (e.g. 'American English').
  -- Cache TTL in seconds: 900
  text :: Text }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Markdown = Markdown {
  -- IMDb markdown format
  --
  -- Cache TTL in seconds: 900
  markdown :: Text }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- Image source details, including attribution requirements.
data Source = Source {
  -- The image source ID
  -- Cache TTL in seconds: 900
  id :: Text,
  -- Displayable text for the image source, if required by the license
  -- Cache TTL in seconds: 900
  text :: Maybe Text,
  -- Link provided by the image source, if required by the license
  -- Cache TTL in seconds: 900
  attributionUrl :: Maybe Text,
  -- Attribution banner image, if required by the license
  -- Cache TTL in seconds: 900
  banner :: Maybe Banner }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data TitleId = TitleId
  { id :: Tid Imdb }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data NameId = NameId {
  -- Cache TTL in seconds: 900
  id :: Tid NameId }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data NameMeta = NameMeta {
  -- Cache TTL in seconds: 900
  publicationStatus :: PublicationStatus,
  -- Cache TTL in seconds: 900
  redirectEntity :: Maybe (Tid NameId) }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data NameText = NameText {
  -- Cache TTL in seconds: 900
  text :: Text }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data KnownForJobCategories = KnownForJobCategories {
  -- Cache TTL in seconds: 900
  jobCategories :: [JobCategory] }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Banner = Banner {
  -- URL for the image resource
  -- Cache TTL in seconds: 900
  url :: Text,
  -- Original height of the banner image resource, in pixels
  -- Cache TTL in seconds: 900
  height :: Int,
  -- Original width of the banner image resource, in pixels
  -- Cache TTL in seconds: 900
  width :: Int,
  -- Link provided by the image source, if required by the license
  -- Cache TTL in seconds: 900
  attributionUrl :: Text }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data JobCategory = JobCategory {
  -- Job category token.
  -- Cache TTL in seconds: 900
  id :: Tid JobCategory,
  -- Job category display text, e.g. 'Animation Department'. This will eventually be localized.
  -- Cache TTL in seconds: 900
  text :: Text }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data PublicationStatus = NOT_PUBLISHED | PUBLISHED | REDIRECTED
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Image = Image {
  -- Cache TTL in seconds: 900
  id :: Text,
  -- URL for the image resource
  -- Cache TTL in seconds: 900
  url :: Maybe Text,
  -- Original height of the image resource, in pixels
  -- Cache TTL in seconds: 900
  height :: Maybe Int,
  -- Original width of the image resource, in pixels
  -- Cache TTL in seconds: 900
  width :: Maybe Int,
  -- Image caption text
  -- Cache TTL in seconds: 900
  caption :: Maybe Markdown,
  -- Image copyright text
  -- Cache TTL in seconds: 900
  copyright :: Maybe Text,
  -- The creator of the image
  -- Cache TTL in seconds: 900
  createdBy :: Maybe Text,
  -- Image source and attribution data
  -- Cache TTL in seconds: 900
  source :: Maybe Source,
  -- Image type token
  -- Cache TTL in seconds: 900
  _type :: Maybe Text,
  -- Name IDs for people in the image
  -- Cache TTL in seconds: 900
  names :: Maybe [NameId],
  -- Title IDs for titles related to the image
  -- Cache TTL in seconds: 900
  titles :: Maybe [TitleId],
  -- Countries associated with the image
  -- Cache TTL in seconds: 900
  countries :: Maybe [DisplayableCountry],
  -- Languages of text in the image
  -- Cache TTL in seconds: 900
  languages :: Maybe [DisplayableLanguage] }
  deriving stock (Show, Eq, Generic)

deriveJSON defaultOptions
  {fieldLabelModifier = stripPrefix1 "_"}
  ''Image

data Title = Title {
  -- Cache TTL in seconds: 900
  id :: Tid Title,
  -- Cache TTL in seconds: 900
  images :: Many0 Image,
  -- For titles which are a series, provides information about the episodes of that series
  -- Cache TTL in seconds: 900
  episodes :: Maybe Episodes,
  -- Cache TTL in seconds: 900
  meta :: Maybe TitleMeta,
  -- The primary image for the title.
  -- Cache TTL in seconds: 900
  primaryImage :: Maybe Image,
  -- Quotes in this Title
  -- Cache TTL in seconds: 900
  quotes :: Many0 TitleQuote,
  -- The plot for the title.
  -- Cache TTL in seconds: 900
  plot :: Maybe Plot,
  -- A list of the countries of origin for the title.
  -- Cache TTL in seconds: 900
  countriesOfOrigin :: Maybe CountriesOfOrigin,
  -- Other title texts by which this Title is known
  -- Cache TTL in seconds: 900
  -- akas :: AkaConnection

  -- The year of the title. The existing IMDb concept of a year that is independent of the release
  -- date.
  -- Cache TTL in seconds: 900
  releaseYear :: Maybe YearRange,
  -- Trivia for a title.
  -- Cache TTL in seconds: 900
  trivia :: Many0 Trivia,
  -- External links for a title.
  -- Cache TTL in seconds: 900
  externalLinks :: Many0 ExternalLink,
  -- Taglines for a title.
  -- Cache TTL in seconds: 900
  taglines :: Many0 Tagline,
  -- News articles about a title
  -- Cache TTL in seconds: 900
  news :: Many0 News,
  -- Alternate versions for a title.
  -- Cache TTL in seconds: 900
  alternateVersions :: Many0 AlternateVersion,
  -- The awards that the title has won or been nominated for
  -- Cache TTL in seconds: 900
  awardNominations :: Many0 AwardNomination,
  -- Frequently asked questions (FAQs) for a title.
  -- Cache TTL in seconds: 900
  faqs :: Many0 Faq,
  -- The type of a given title
  -- Cache TTL in seconds: 900
  titleType :: Maybe TitleType,
  -- The localized text for the title.
  -- Cache TTL in seconds: 900
  titleText :: Maybe TitleText,
  -- The original text for the title.
  -- Cache TTL in seconds: 900
  originalTitleText :: Maybe TitleText,
  -- Goofs for a title.
  -- Cache TTL in seconds: 900
  goofs :: Many0 Goof,
  -- Cache TTL in seconds: 900
  goofCategories :: Maybe [GoofCategoryWithGoofs],
  -- The earliest localized release date of the title.
  -- Cache TTL in seconds: 900
  releaseDate :: Maybe ReleaseDate,
  -- A list of the languages spoken in the title.
  -- Cache TTL in seconds: 900
  spokenLanguages :: Maybe SpokenLanguages,
  -- Technical specifications for a title.
  -- Cache TTL in seconds: 900
  technicalSpecifications :: Maybe TechnicalSpecifications,
  -- Sound track for a title.
  -- Cache TTL in seconds: 900
  soundtrack :: Many0 Soundtrack,
  -- The runtime for the title, in seconds
  -- Cache TTL in seconds: 900
  runtime :: Maybe Runtime,
  -- The localized certificate for the title.
  -- Cache TTL in seconds: 900
  certificate :: Maybe Certificate,
  -- The genres for the title.
  -- Cache TTL in seconds: 900
  genres :: Maybe Genres,
  -- Cache TTL in seconds: 900
  companyCredits :: Many0 CompanyCredit,
  -- Cache TTL in seconds: 900
  parentsGuide :: Maybe ParentsGuide,
  -- Production status of the title, optional comment and the date at which it moved to that status.
  -- Cache TTL in seconds: 900
  productionStatus :: Maybe ProductionStatusDetails,
  -- Title keywords.
  -- Cache TTL in seconds: 900
  keywords :: Many0 Keyword,
  -- Title filming locations
  -- Cache TTL in seconds: 900
  filmingLocations :: Many0 FilmingLocation,
  -- Crazy credits for a title.
  -- Cache TTL in seconds: 900
  crazyCredits :: Many0 CrazyCredit,
  -- The connections for the title.
  -- Cache TTL in seconds: 900
  connections :: Many0 TitleConnection,
  -- All releases of the title.
  -- Cache TTL in seconds: 900
  releases :: Many0 TitleRelease,
  -- The entire list of cast & crew members
  -- Cache TTL in seconds: 900
  credits :: Many0 Credit,
  -- Array of most important credit categories and credits in that category
  -- Cache TTL in seconds: 900
  principalCredits :: Maybe [PrincipalCreditsForCategory],
  -- A boolean flag that tells you whether this title can have episodes.
  -- It does not guarantee that it actually has episodes, only that it can have episodes.
  -- Cache TTL in seconds: 900
  canHaveEpisodes :: Maybe Bool,
  -- The plots for the title.
  -- Cache TTL in seconds: 900
  plots :: Many0 Plot,
  -- For titles which are part of a series, provides information about its position within the series
  -- Cache TTL in seconds: 900
  series :: Maybe Series,
  -- The latest trailer for a title
  -- Cache TTL in seconds: 300
  latestTrailer :: Maybe Video,
  -- An ordered list of the primary videos related to a title.
  -- If the first argument exceeds the maximum, the results will be capped to 100.
  -- Cache TTL in seconds: 300
  primaryVideos :: Many0 Video,
  -- An ordered list of prominent videos related to a title
  -- Cache TTL in seconds: 300
  videoStrip :: Many0 Video,
  -- Primary watch option determined by ordering the providers based on an order defined by business.
  --
  -- If the location is included, returns any location-specific watch options (will be required in the future)
  -- Cache TTL in seconds: 600
  -- primaryWatchOption(
  --   location: WatchOptionsLocation
  --   promotedProvider: String
  --   queryFilter: WatchOptionQueryFilter
  -- ): PrimaryWatchOption

  -- Watch options for a given titleId.
  -- The options are ordered in an order defined by business and grouped by the watch option category.
  --
  -- If the location is included, returns any location-specific watch options (will be required in the future)
  --
  -- Returns a list with the maximum number of watch option categories based on the filters. NO PAGINATION
  -- Cache TTL in seconds: 600
  -- watchOptionsByCategory(
  --   limit: Int
  --   location: WatchOptionsLocation
  --   promotedProvider: String
  --   queryFilter: WatchOptionQueryFilter
  -- ): CategorizedWatchOptionsList

  -- Cache TTL in seconds: 0
  -- canRate :: Maybe CanRate,

  -- Featured reviews for the title.
  -- Capped at 1 review. No pagination
  -- Cache TTL in seconds: 0
  featuredReviews :: Many0 Review,
  -- Cache TTL in seconds: 0
  ratingsSummary :: Maybe RatingsSummary,
  -- All Reviews for the title.
  -- Cache TTL in seconds: 0
  reviews :: Many0 Review,
  -- Cache TTL in seconds: 0
  userRating :: Maybe Rating,
  -- This schema extends Title with a new field, Metacritic.
  -- Cache TTL in seconds: 900
  metacritic :: Maybe Metacritic,
  -- Featured polls for the title.
  -- Capped at <LIMIT> polls. No pagination
  -- Cache TTL in seconds: 300
  featuredPolls :: Many0 Poll }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Episodes = Episodes
  { episodes :: Many0 TitleId
  , years    :: [EpisodesYear]
  , seasons  :: [EpisodesSeason] }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data EpisodesSeason = EpisodesSeason
  { number :: Int }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Series = Series
  { series :: TitleId
  , episodeNumber :: EpisodeNumber
  , nextEpisode :: Maybe TitleId
  , previousEpisode :: Maybe TitleId }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data EpisodeNumber = EpisodeNumber
  { episodeNumber :: Int
  , seasonNumber  :: Int }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data EpisodesYear = EpisodesYear
  { year :: Int }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Rating = Rating {
  -- Cache TTL in seconds: 0
  date :: DateTime,
  -- Cache TTL in seconds: 0
  value :: Int }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | A date-time with an offset from UTC/Greenwich in the ISO-8601 calendar system, such as 2007-12-03T10:15:30+01:00.
type DateTime = Int

data AlternateVersion = AlternateVersion {
  -- The alternate version text
  -- Cache TTL in seconds: 900
  text :: Markdown }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data TitleText = TitleText {
  -- Cache TTL in seconds: 900
  text :: Text,
  -- Cache TTL in seconds: 900
  isOriginalTitle :: Bool,
  -- Cache TTL in seconds: 900
  country :: DisplayableCountry,
  -- Cache TTL in seconds: 900
  language :: Maybe DisplayableLanguage }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Plot = Plot {
  -- Unique persistent ID for this plot
  -- Cache TTL in seconds: 900
  id :: Tid Plot,
  -- The plot text
  -- Cache TTL in seconds: 900
  plotText :: Markdown,
  -- The type of the plot
  -- Cache TTL in seconds: 900
  plotType :: PlotType,
  -- The language of the plot text
  -- Cache TTL in seconds: 900
  language :: DisplayableLanguage,
  -- Whether this plot contains spoilers
  -- Cache TTL in seconds: 900
  isSpoiler :: Bool,
  -- The name of the plot's author. This field is not required as it can be null for an anonymous author.
  -- Cache TTL in seconds: 900
  author :: Maybe Text }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data PlotType = OUTLINE | SUMMARY | SYNOPSIS
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- Quote details
data TitleQuote = TitleQuote {
  -- Title Quote ID
  -- Cache TTL in seconds: 900
  id :: Tid TitleQuote,
  -- Is this Title Quote a spoiler
  -- Cache TTL in seconds: 900
  isSpoiler :: Bool,
  -- The verbal and/or non-verbal lines that make up this Title Quote
  -- Cache TTL in seconds: 900
  lines :: [TitleQuoteLine],
  -- Votes from users about whether this Quote is interesting
  -- Cache TTL in seconds: 900
  interestScore :: InterestScore,
  -- The language of this Title Quote
  -- Cache TTL in seconds: 900
  language :: DisplayableLanguage }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- A specific line in the Title Quote. Can be a verbal line with characters speaking or stage directions
data TitleQuoteLine = TitleQuoteLine {
  -- The characters who speak this line, e.g.  'Rick'. Not required: a line may be non-verbal
  -- Cache TTL in seconds: 900
  characters :: Maybe [TitleQuoteCharacter],
  -- The body of the quotation line, e.g 'Here's looking at you kid. '.  Not
  -- required: you may have stage directions with no dialogue.
  -- Cache TTL in seconds: 900
  text :: Maybe Text,
  -- Stage direction, e.g. 'Rick gently places his hand under her chin and raises it so their eyes meet'. Not required.
  -- Cache TTL in seconds: 900
  stageDirection :: Maybe Text }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- Votes from users about whether an item is interesting.
data InterestScore = InterestScore {
  -- The number of users who found this interesting.
  -- Cache TTL in seconds: 900
  usersInterested :: Int,
  -- The total number of users who have voted on this item being interesting or not.
  -- Cache TTL in seconds: 900
  usersVoted :: Int }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- A character speaking a line in a Title Quote
data TitleQuoteCharacter = TitleQuoteCharacter {
  -- The name of the character in the Title
  -- Cache TTL in seconds: 900
  character :: Text,
  -- The Name behind that character
  -- Cache TTL in seconds: 900
  name :: Maybe NameId }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Name = Name {
  -- Cache TTL in seconds: 900
  id :: Tid Name,
  -- Cache TTL in seconds: 900
  images :: Many Image,
  -- Cache TTL in seconds: 900
  meta :: NameMeta,
  -- The primary image for the name.
  -- Cache TTL in seconds: 900
  primaryImage :: Image,
  -- The person's name as it appears on IMDb.
  -- Cache TTL in seconds: 900
  nameText :: NameText,
  -- The person's knownForJobCategories as they appear on IMDb.
  -- Cache TTL in seconds: 900
  knownForJobCategories :: KnownForJobCategories }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

type AwardNomination = ()
type Certificate = ()
type CompanyCredit = ()
type CountriesOfOrigin = ()
type CrazyCredit = ()
type Credit = ()
type ExternalLink = ()
type Faq = ()
type FilmingLocation = ()
type Genres = ()
type Goof = ()
type GoofCategoryWithGoofs = ()
type Keyword = ()
type Metacritic = ()
type News = ()
type ParentsGuide = ()
type Poll = ()
type PrincipalCreditsForCategory = ()
type ProductionStatusDetails = ()
type RatingsSummary = ()
type ReleaseDate = ()
type Review = ()
type Runtime = ()
type Soundtrack = ()
type SpokenLanguages = ()
type Tagline = ()
type TechnicalSpecifications = ()
type TitleConnection = ()
type TitleMeta = ()
type TitleRelease = ()
type TitleType = ()
type Trivia = ()
type Video = ()
type YearRange = ()
