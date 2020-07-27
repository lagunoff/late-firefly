module LateFirefly.IMDB.GraphQL where

import LateFirefly.DB
import LateFirefly.Prelude
import LateFirefly.Aeson

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

-- Scalar defining a date without a time info according to the ISO 8601 format, such as 2018-01-11
type Date = Text
-- A BCP-47 String with the primary language in ISO 639-1 followed by a dash and
-- ISO 3166-1 or UN M.49 code (e.g en-US es-419 fr-FR)
type Language = Text
-- A date-time with an offset from UTC/Greenwich in the ISO-8601 calendar system, such as 2007-12-03T10:15:30+01:00.
type DateTime = Text
type ID = Text

data Edge a = Edge
  { node :: a }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data DisplayableCountry = DisplayableCountry {
  -- The country code - either an ISO 3166 code or an internally defined code if no ISO code exists for that country.
  -- Cache TTL in seconds: 900
  id :: ID,
  -- Display text for the country (e.g. 'United States').
  -- Cache TTL in seconds: 900
  text :: Text }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data DisplayableLanguage = DisplayableLanguage {
  -- The language code - either an ISO 639 code or an internally defined code if no ISO code exists for the language.
  -- Cache TTL in seconds: 900
  id :: ID,
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
  id :: ID,
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
  { id :: ID }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data NameId = NameId {
  -- Cache TTL in seconds: 900
  id :: ID }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data NameMeta = NameMeta {
  -- Cache TTL in seconds: 900
  publicationStatus :: PublicationStatus,
  -- Cache TTL in seconds: 900
  redirectEntity :: Maybe Text }
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
  id :: ID,
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
  id :: ID,
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
  {fieldLabelModifier = replaces [("_type", "type")]}
  ''Image

data ImageId = ImageId {
  -- Cache TTL in seconds: 900
  id :: ID }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Title = Title {
  -- Cache TTL in seconds: 900
  id :: ID,
  -- Cache TTL in seconds: 900
  images :: Many0 Image,
  -- For titles which are a series, provides information about the episodes of that series
  -- Cache TTL in seconds: 900
  episodes :: Maybe Episodes,
  -- Cache TTL in seconds: 900
  meta :: Maybe TitleMeta,
  -- The primary image for the title.
  -- Cache TTL in seconds: 900
  primaryImage :: Maybe ImageId,
  -- Quotes in this Title
  -- Cache TTL in seconds: 900
  quotes :: Many0 TitleQuote,
  -- The plot for the title.
  -- Cache TTL in seconds: 900
  plot :: Maybe PlotId,
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
  trivia :: Many0 TitleTrivia,
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
  -- The earliest localized release date of the title.
  -- Cache TTL in seconds: 900
  releaseDate :: Maybe ReleaseDate,
  -- A list of the languages spoken in the title.
  -- Cache TTL in seconds: 900
  spokenLanguages :: Maybe SpokenLanguages,
  -- Technical specifications for a title.
  -- Cache TTL in seconds: 900
  -- technicalSpecifications :: Maybe TechnicalSpecifications,
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
  id :: ID,
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

data PlotId = PlotId {
  id :: ID }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data PlotType = OUTLINE | SUMMARY | SYNOPSIS
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)
  deriving FromField via JsonField PlotType
  deriving ToField via JsonField PlotType
  deriving DbField via JsonField PlotType

-- Quote details
data TitleQuote = TitleQuote {
  -- Title Quote ID
  -- Cache TTL in seconds: 900
  id :: ID,
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
  id :: ID,
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

data AwardNomination = AwardNomination {
  -- Award Nomination ID
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  id :: ID,
  -- Did the title win this award
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  isWinner :: Bool,
  -- Details about the award, such as the name and year
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  award :: AwardDetails }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data AwardDetails = AwardDetails {
  -- Award Id
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  id :: ID,
  -- The event at which the award is presented
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  event :: AwardsEvent,
  -- Award name
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  text :: Text,
  -- Award year
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  year :: Int }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data AwardsEvent = AwardsEvent {
  -- Event ID
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  id :: ID,
  -- Event name
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  text :: Text }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Certificate = Certificate {
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  rating :: Text,
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  country :: DisplayableCountry,
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  ratingsBody :: Maybe Text,
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  ratingReason :: Maybe Text }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- CompanyCredit details. Open question: do we want to add a persistent ID for company credits?
-- Without this we require a companyID, titleID and Category to uniquely identify it.
data CompanyCredit = CompanyCredit {
  -- The company credited
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  company :: Company,
  -- The title on which this credit appears
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  title :: TitleId,
  -- Category (e.g. 'Production').
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  category :: CompanyCreditCategory,
  -- Start and optionally end year(s) that this company was involved with this title
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  yearsInvolved :: Maybe YearRange,
  -- The countries in which this company was involved with this title
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  countries :: Maybe [DisplayableCountry],
  -- Miscellaneous attributes
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  attributes :: Maybe [FreeTextAttribute] }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Company = Company {
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  id :: ID,
  -- The localized text for the company.
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  companyText :: Maybe CompanyText }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data CompanyText = CompanyText {
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  text :: Text }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data CompanyCreditCategory = CompanyCreditCategory {
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  id :: ID,
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  text :: Text }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data YearRange = YearRange {
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  year :: Int,
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  endYear :: Maybe Int }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data FreeTextAttribute = FreeTextAttribute {
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  text :: Text }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data CountriesOfOrigin = CountriesOfOrigin {
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  countries :: [CountryOfOrigin],
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  language :: DisplayableLanguage }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data CountryOfOrigin = CountryOfOrigin {
  -- The country code - either an ISO 3166 code or an internally defined code if no ISO code exists for that country.
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  id :: ID,
  -- Display text for the country (e.g. 'United States').
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  text :: Text }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- Crazy credit details
data CrazyCredit = CrazyCredit {
  -- Crazy credit ID
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  id :: ID,
  -- The crazy credit text
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  text :: Markdown,
  -- Votes from users about whether this crazy credit item is interesting.
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  interestScore :: InterestScore }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- Credit details. Open question: do we want to add a persistent ID for credits?
-- Without this we require a nameID, titleID and Category to uniquely identify it.
data Credit = Credit {
  -- The person credited
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  name :: NameId,
  -- The title on which this credit appears
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  title :: TitleId,
  -- Category (e.g. 'Producer').
  -- Open question: the name of this field comes from TitleCreditsBaseV5. Should we change it?
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  category :: CreditCategory,
  -- Miscellaneous attributes (e.g. 'Archive Footage')
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  attributes :: Maybe [CreditAttribute],
  -- For series parent titles, credits with the same name and category for all the series episodes
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  episodeCredits :: Many0 Credit }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- External link details
data ExternalLink = ExternalLink {
  -- The type of link
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  externalLinkCategory :: ExternalLinkCategory,
  -- The URL of the site
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  url :: Text,
  -- The languages supported by the site
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  externalLinkLanguages :: Maybe [DisplayableLanguage],
  -- The user-supplied region of the site.
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  externalLinkRegion :: Maybe DisplayableCountry,
  -- The link's label
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  label :: Maybe Text,
  -- The language of the link's label
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  labelLanguage :: Maybe DisplayableLanguage }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- Faq details
data Faq = Faq {
  -- FAQ ID
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  id :: ID,
  -- The question text
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  question :: Markdown,
  -- The answer text. Note that [not all questions have answers](https://www.imdb.com/title/tt10547784/quotes/qt4903101).
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  answer :: Maybe Markdown,
  -- The language in which the FAQ was written. Note that each question-answer pair has the same language.
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  language :: DisplayableLanguage,
  -- Is this faq item a spoiler
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  isSpoiler :: Bool }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- Filming location details
data FilmingLocation = FilmingLocation {
  -- Filming location ID
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  id :: ID,
  -- The filming location text
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  text :: Text,
  -- Optional. A list of filming location attributes, can contain:
  -- - Free text attributes e.g. { id: 'freeText', text: 'Butch in his car encounters Marsellus Wallace' }

  -- - Location properties e.g. { id: 'exterior', text: 'Exterior' }

  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  attributes :: Maybe [LocationAttribute],
  -- Votes from users about whether this filming location is interesting.
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  interestScore :: InterestScore }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- Filming location attribute
data LocationAttribute = LocationAttribute {
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  id :: ID,
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  text :: Text }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Genres = Genres {
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  genres :: [Genre],
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  language :: DisplayableLanguage }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- Details of a single goof
data Goof = Goof {
  -- Goof ID
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  id :: ID,
  -- The goof text
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  text :: Markdown,
  -- Is this goof a spoiler
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  isSpoiler :: Bool,
  -- Votes from users about whether this goof item is interesting.
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  interestScore :: InterestScore,
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  category :: GoofCategory }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- A category of goofs describing the nature of a subset of goofs, e.g. 'Continuity'
data GoofCategory = GoofCategory {
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  id :: ID,
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  text :: Text }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- Keyword details
data Keyword = Keyword {
  -- Keyword ID
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  id :: ID,
  -- The keyword text
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  text :: Text,
  -- Votes from users about whether this keyword item is interesting.
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  interestScore :: InterestScore }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Metacritic = Metacritic {
  -- Simple placeholder value to get basic data vending working.
  -- The full schema will be added next.
  --
  -- Affected by headers: x-imdb-detected-country, x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  metascore :: Metascore }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Metascore = Metascore {
  -- The number of reviews factoring into the overall score.
  --
  -- Affected by headers: x-imdb-detected-country, x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  reviewCount :: Int,
  -- The average review score.  Between 0 and 100, inclusive.
  --
  -- Affected by headers: x-imdb-detected-country, x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  score :: Int }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- News details
data News = News {
  -- Unique persistent ID for this news article
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  id :: ID,
  -- The title text of this news article
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  articleTitle :: Markdown,
  -- A direct link to the article on an external news site
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  externalUrl :: Text,
  -- Name and link to homepage for the source website of this news article
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  source :: NewsSource,
  -- The date of this news article, in ISO-8601 format
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  date :: Date,
  -- The lead paragraph of this news article
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  text :: Markdown,
  -- An image associated with this news article
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  image :: Maybe Image,
  -- The byline of this news article
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  byline :: Maybe Text,
  -- The language of this news article
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  language :: Maybe DisplayableLanguage }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- Parents guides for a given title
data ParentsGuide = ParentsGuide {
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  guideItems :: Many0 ParentsGuide,
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  categories :: Maybe [ParentsGuideCategorySummary] }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- Parents guide severity summary
data ParentsGuideCategorySummary = ParentsGuideCategorySummary {
  -- Category for the guide, e.g. 'Violence & Gore'
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  category :: ParentsGuideCategory,
  -- Displayable severity of this category
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  severity :: Maybe SeverityLevel,
  -- Total number of users who voted on the severity levels in this category
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  totalSeverityVotes :: Int,
  -- A breakdown of the severity levels for this category
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  severityBreakdown :: Maybe [SeverityLevel] }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- The severity level of a particular title, together with the number of users
-- who voted for this level of severity.
data SeverityLevel = SeverityLevel {
  -- Severity ID
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  id :: ID,
  -- Severity text, e.g. 'Mild', 'Moderate', 'Severe'
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  text :: Text,
  -- The number of users who voted for this level of severity
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  votedFor :: Int }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- Parents guide category details
data ParentsGuideCategory = ParentsGuideCategory {
  -- Parents guide category ID
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  id :: ID,
  -- The displayable text type of the parents guide category, e.g. 'Violence & Gore'
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  text :: Text }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Poll = Poll {
  -- Answers for polls, non null response
  --
  -- Affected by headers: x-amzn-transitive-authentication-token,
  -- x-imdb-customer-id, x-imdb-user-id, x-imdb-detected-country,
  -- x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 300
  answers :: Many0 PollAnswer,
  -- Affected by headers: x-amzn-transitive-authentication-token,
  -- x-imdb-customer-id, x-imdb-user-id, x-imdb-detected-country,
  -- x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 300
  id :: ID,
  -- Primary image for poll, nullable response
  --
  -- Affected by headers: x-amzn-transitive-authentication-token,
  -- x-imdb-customer-id, x-imdb-user-id, x-imdb-detected-country,
  -- x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 300
  primaryImage :: Maybe PollPrimaryImage,
  -- Poll question, non null response
  --
  -- Affected by headers: x-amzn-transitive-authentication-token,
  -- x-imdb-customer-id, x-imdb-user-id, x-imdb-detected-country,
  -- x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 300
  question :: Maybe PollQuestion }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

--  Answer type to nest items and descriptions
data PollAnswer = PollAnswer {
  -- Affected by headers: x-amzn-transitive-authentication-token,
  -- x-imdb-customer-id, x-imdb-user-id, x-imdb-detected-country,
  -- x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 300
  item :: AnswerItem }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data AnswerItem = AnswerItem -- Image | Name | Title
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data PollPrimaryImage = PollPrimaryImage {
  -- Affected by headers: x-amzn-transitive-authentication-token,
  -- x-imdb-customer-id, x-imdb-user-id, x-imdb-detected-country,
  -- x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 300
  image :: Image }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data PollQuestion = PollQuestion {
  -- Affected by headers: x-amzn-transitive-authentication-token,
  -- x-imdb-customer-id, x-imdb-user-id, x-imdb-detected-country,
  -- x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 300
  originalText :: Markdown }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data PrincipalCreditsForCategory = PrincipalCreditsForCategory {
  -- Category (e.g. 'Producer' or 'Actor')
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  category :: CreditCategory,
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  credits :: Maybe [Credit] }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data CreditCategory = CreditCategory {
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  id :: ID,
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  text :: Text }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ProductionStatusDetails = ProductionStatusDetails {
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  date :: Date,
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  status :: ProductionStatus,
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  comment :: Maybe Text }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ProductionStatus = ProductionStatus {
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  id :: ID,
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  text :: Text }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data CreditAttribute = CreditAttribute {
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  id :: ID,
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  text :: Text }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- External link type
data ExternalLinkCategory = ExternalLinkCategory {
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  id :: ID,
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  text :: Text }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- An external news site
data NewsSource = NewsSource {
  -- Link to the homepage of the site
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  homepage :: NewsLink }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data NewsLink = NewsLink {
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  url :: Text,
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  label :: Maybe Text }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data RatingsSummary = RatingsSummary {
  -- Affected by headers: x-amzn-transitive-authentication-token,
  -- x-imdb-customer-id, x-imdb-user-id, x-imdb-detected-country,
  -- x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 0
  aggregateRating :: Maybe Float,
  -- Affected by headers: x-amzn-transitive-authentication-token,
  -- x-imdb-customer-id, x-imdb-user-id, x-imdb-detected-country,
  -- x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 0
  voteCount :: Int }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ReleaseDate = ReleaseDate {
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  day :: Maybe Int,
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  month :: Maybe Int,
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  year :: Int,
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  country :: DisplayableCountry }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Review = Review {
  -- Affected by headers: x-amzn-transitive-authentication-token,
  -- x-imdb-customer-id, x-imdb-user-id, x-imdb-detected-country,
  -- x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 0
  author :: Maybe UserProfile,
  -- Affected by headers: x-amzn-transitive-authentication-token,
  -- x-imdb-customer-id, x-imdb-user-id, x-imdb-detected-country,
  -- x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 0
  authorRating :: Maybe Int,
  -- Affected by headers: x-amzn-transitive-authentication-token,
  -- x-imdb-customer-id, x-imdb-user-id, x-imdb-detected-country,
  -- x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 0
  helpfulness :: Maybe ReviewHelpfulness,
  -- Affected by headers: x-amzn-transitive-authentication-token,
  -- x-imdb-customer-id, x-imdb-user-id, x-imdb-detected-country,
  -- x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 0
  id :: ID,
  -- Affected by headers: x-amzn-transitive-authentication-token,
  -- x-imdb-customer-id, x-imdb-user-id, x-imdb-detected-country,
  -- x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 0
  language :: Maybe Language,
  -- Affected by headers: x-amzn-transitive-authentication-token,
  -- x-imdb-customer-id, x-imdb-user-id, x-imdb-detected-country,
  -- x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 0
  spoiler :: Maybe Bool,
  -- Affected by headers: x-amzn-transitive-authentication-token,
  -- x-imdb-customer-id, x-imdb-user-id, x-imdb-detected-country,
  -- x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 0
  submissionDate :: Maybe Date,
  -- Affected by headers: x-amzn-transitive-authentication-token,
  -- x-imdb-customer-id, x-imdb-user-id, x-imdb-detected-country,
  -- x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 0
  summary :: Maybe ReviewSummary,
  -- Affected by headers: x-amzn-transitive-authentication-token,
  -- x-imdb-customer-id, x-imdb-user-id, x-imdb-detected-country,
  -- x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 0
  text :: Maybe ReviewText,
  -- Affected by headers: x-amzn-transitive-authentication-token,
  -- x-imdb-customer-id, x-imdb-user-id, x-imdb-detected-country,
  -- x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 0
  title :: Maybe TitleId }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- User Profile with public user info
-- Apollo Entity that will be used by other graphlets
-- For example, author of List
data UserProfile = UserProfile {
  -- Affected by headers: x-amzn-transitive-authentication-token, x-imdb-customer-id, x-imdb-user-id
  --
  -- Cache TTL in seconds: 0
  nickName :: Maybe Text,
  -- Affected by headers: x-amzn-transitive-authentication-token, x-imdb-customer-id, x-imdb-user-id
  --
  -- Cache TTL in seconds: 0
  userId :: Maybe Text }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ReviewHelpfulness = ReviewHelpfulness {
  -- Affected by headers: x-amzn-transitive-authentication-token,
  -- x-imdb-customer-id, x-imdb-user-id, x-imdb-detected-country,
  -- x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 0
  downVotes :: Int,
  -- Affected by headers: x-amzn-transitive-authentication-token,
  -- x-imdb-customer-id, x-imdb-user-id, x-imdb-detected-country,
  -- x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 0
  score :: Float,
  -- Affected by headers: x-amzn-transitive-authentication-token,
  -- x-imdb-customer-id, x-imdb-user-id, x-imdb-detected-country,
  -- x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 0
  upVotes :: Int }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ReviewSummary = ReviewSummary {
  -- Affected by headers: x-amzn-transitive-authentication-token,
  -- x-imdb-customer-id, x-imdb-user-id, x-imdb-detected-country,
  -- x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 0
  originalText :: Text }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ReviewText = ReviewText {
  -- Affected by headers: x-amzn-transitive-authentication-token,
  -- x-imdb-customer-id, x-imdb-user-id, x-imdb-detected-country,
  -- x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 0
  originalText :: Markdown }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Runtime = Runtime {
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  seconds :: Int,
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  country :: DisplayableCountry }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Soundtrack = Soundtrack {
  -- A token representing the track.
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  id :: ID,
  -- Optional display text for the track (e.g. 'Danger Zone').
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  text :: Maybe String,
  -- Comments for this track
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  comments :: [Markdown] }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data SpokenLanguages = SpokenLanguages {
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  spokenLanguages :: [SpokenLanguage],
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  language :: DisplayableLanguage }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data SpokenLanguage = SpokenLanguage {
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  id :: ID,
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  text :: Text }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- Tagline details
data Tagline = Tagline {
  -- The tagline text
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  text :: Text }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- Title connection details
data TitleConnection = TitleConnection {
  -- The connection text
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  text :: Maybe String,
  -- The other title in this connection
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  associatedTitle :: TitleId,
  -- The category of this connection
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  category :: TitleConnectionCategory }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data TitleConnectionCategory = TitleConnectionCategory {
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  id :: ID,
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  text :: Maybe Text }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data TitleMetadata = TitleMetadata {
  -- All the valid external link categories
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  externalLinkCategories :: [ExternalLinkCategory],
  -- All the valid goof types
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  goofCategories :: [GoofCategory] }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data TitleMeta = TitleMeta {
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  publicationStatus :: PublicationStatus,
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  redirectEntity :: Maybe Title }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- All the information for a title release
data TitleRelease = TitleRelease {
  -- The release date
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  releaseDate :: ReleaseDate,
  -- Additional attributes
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  attributes :: Maybe [FreeTextAttribute] }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data TitleType = TitleType {
  -- Title type ID
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  id :: ID,
  -- The displayable title type, e.g. 'TV Series'
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  text :: Text }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- Trivia details
data TitleTrivia = TitleTrivia {
  -- TRIVIA ID
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  id :: ID,
  -- The trivia text
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  text :: Markdown,
  -- Is this trivia item a spoiler
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  isSpoiler :: Bool,
  -- trivia Type
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  triviaType :: Maybe Text,
  -- trivia Voting summary
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  interestScore :: InterestScore,
  -- trivia Trademark text
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  trademark :: Maybe Text,
  -- trivia Related Name IDs
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  relatedNames :: Maybe [NameId] }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- Entity
data Video = Video {
  -- Affected by headers: x-imdb-detected-country
  --
  -- Cache TTL in seconds: 300
  description :: Maybe LocalizedString,
  -- Affected by headers: x-imdb-detected-country
  --
  -- Cache TTL in seconds: 300
  id :: ID,
  -- Affected by headers: x-imdb-detected-country
  --
  -- Cache TTL in seconds: 300
  name :: LocalizedString,
  -- Affected by headers: x-imdb-detected-country
  --
  -- Cache TTL in seconds: 300
  primaryTitle :: Maybe TitleId,
  -- Runtime is required for vi consts but not for vc consts
  --
  -- Affected by headers: x-imdb-detected-country
  --
  -- Cache TTL in seconds: 300
  runtime :: Maybe VideoRuntime,
  -- Affected by headers: x-imdb-detected-country
  --
  -- Cache TTL in seconds: 300
  thumbnail :: Thumbnail }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- A string that will be shown to customers with the language it is in.
data LocalizedString = LocalizedString {
  -- Affected by headers: x-imdb-detected-country
  --
  -- Cache TTL in seconds: 600
  language :: Language,
  -- Affected by headers: x-imdb-detected-country
  --
  -- Cache TTL in seconds: 600
  value :: Text }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- Types
data VideoRuntime = VideoRuntime {
  -- Affected by headers: x-imdb-detected-country
  --
  -- Cache TTL in seconds: 300
  unit :: TimeUnit,
  -- Affected by headers: x-imdb-detected-country
  --
  -- Cache TTL in seconds: 300
  value :: Int }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- Enums
data TimeUnit = SECONDS
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Thumbnail = Thumbnail {
  -- Affected by headers: x-imdb-detected-country
  --
  -- Cache TTL in seconds: 300
  height :: Int,
  -- Affected by headers: x-imdb-detected-country
  --
  -- Cache TTL in seconds: 300
  url :: Text,
  -- Affected by headers: x-imdb-detected-country
  --
  -- Cache TTL in seconds: 300
  width :: Int }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Genre = Genre {
  -- A token representing the genre.
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  id :: ID,
  -- Display text for the genre (e.g. 'Action').
  --
  -- Affected by headers: x-imdb-user-country, x-imdb-user-language
  --
  -- Cache TTL in seconds: 900
  text :: Text }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)
