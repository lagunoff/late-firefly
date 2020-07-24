module LateFirefly.IMDB.Scrape
  ( scrapeSearch
  , scrapeEpisodes
  ) where

import Control.Lens as L hiding (children, (.=))
import Control.Exception
import Control.Error
import Data.Aeson as AE
import Data.Aeson.TH as AE
import Data.Text as T
import Data.Char
import Data.Coerce
import Data.List as L
import Data.Map as M
import Prelude as P
import Data.ByteString.Lazy as BSL
import Data.Text.Encoding as T
import Text.Read hiding (lift)
import LateFirefly.DB
import LateFirefly.Prelude
import LateFirefly.IMDB.Schema
import Text.HTML.TagSoup.Lens as L hiding (attr)
import Text.HTML.TagSoup as L
import qualified Network.Wreq as Wreq
import qualified Network.Wreq.Types as Wreq hiding (headers)
import Text.Regex.Lens
import Text.Regex.Quote
import Text.Regex.TDFA
import Network.HTTP.Client hiding (withConnection)
import GHC.Stack
import GHC.Records
import Network.HTTP.Types
import Text.Shakespeare.Text as X (st)

data ScrapeError
  = HttpException HttpException
  | CallStackException CallStack
  | GraphQLError String
  deriving (Exception, Show)

genres :: [Genre]
genres = ["action", "adventure", "animation", "biography", "comedy", "crime", "documentary", "drama", "family", "fantasy", "film_noir", "game_show", "history", "horror", "music", "musical", "mystery", "news", "reality_tv", "romance", "sci_fi", "sport", "talk_show", "thriller", "war", "western"]

-- * Scrape ImdbEpisode

scrapeSearch :: (?conn::Connection) => Bool -> Double -> IO ()
scrapeSearch continue percentile = void $ newVersionOrContinue continue $ unEio do
  prog <- liftIO readProgress
  for_ LateFirefly.IMDB.Scrape.genres \g -> do
    (\x -> foldM_ x Nothing (paginate 9951)) \total p -> do
      let p' = fromIntegral p
      let total' = fmap fromIntegral total
      case (fmap ((p' <) . (*percentile)) total') of
        Just False -> pure total
        _          -> do
          let progress = fromMaybe 0 $ M.lookup g prog
          case (progress >= p + 49) of
            True  -> Nothing <$ (traceM $ "Skipping: " <> ("https://www.imdb.com/search/title/?genres=" <> T.unpack g <> "&start=" <> show p))
            False -> Just <$> (scrapeImdbSearch1 g p)

scrapeImdbSearch1
  :: (?conn::Connection, ?version::NewVersion)
  => Genre -> Int -> Eio ScrapeError Int
scrapeImdbSearch1 g start = do
  markup <- T.decodeUtf8 . BSL.toStrict . (^. Wreq.responseBody) <$> httpGet ("https://www.imdb.com/search/title/?genres=" <> T.unpack g <> "&start=" <> show start)
  let episodeOuters = markup^.._DOM.traverse.allElements.hasClass "lister-item"
  total <- mayThrow $ markup^.._DOM.traverse . allElements . attributed(ix "id" . traverse . only "main") . allClass "desc" . allElements . named (only "span") ^? ix 0 . allContents . to T.unpack . regex [r| of ([,.[:digit:]]+)|] . captures . ix 0 . to (readMaybe . L.filter isDigit) . traverse
  total <$ for_ episodeOuters \el -> do
    let deleted      = False
    let certificate  = el ^? allClass "text-muted" . allClass "certificate" . allContents
    let runtime      = el ^? allClass "text-muted" . allClass "runtime" . allContents
    let genre        = el ^.. allClass "text-muted" . allClass "genre" . allContents. to (L.filter (/=mempty) . fmap T.strip . T.splitOn ",") . traverse
    let textMutedEls = el ^.. allClass "lister-item-content" . children . traverse . elt . hasClass "text-muted"
    let rating       = el ^? allClass "ratings-bar". allElements . named(only "strong").allContents
    let starsEls       = el ^.. allElements . named(only "a") . allElements . attributed(ix "href" . traverse . nearly "" (T.isPrefixOf "/name/"))
    let stars = catMaybes $ starsEls <&> \e -> liftA2 (,) (e ^? attrOne "href" . to T.unpack . regex [r|/name/([[:alnum:]\-_]+)|] . captures . ix 0 . to T.pack) (Just (e ^. allContents))
    let thumbnail67x98 = el ^? allClass "lister-item-image". allElements . named(only "img").attrOne "loadlate"
    let year = el ^? allClass "lister-item-year" . allContents . to (T.dropWhile (=='(') . T.dropWhileEnd (==')'))
    headerEl   <- mayThrow $ el ^? allClass "lister-item-header"
    rowid      <- fmap coerce $ mayThrow $ headerEl ^? allElements . named(only "a").attrOne "href" . to (T.unpack . T.strip) . regex [r|/title/([[:alnum:]\-_]+)|] . captures . ix 0 . to T.pack
    header     <- mayThrow $ headerEl ^? allElements . named(only "a").allContents
    popularity <- fmap (M.singleton g) $ mayThrow $ headerEl ^? allClass "lister-item-index".allContents . to (T.unpack . T.strip) . regex [r|^([\s,[:digit:]]+)\.?$|] . captures . ix 0 . to (readMaybe @Int . L.filter isDigit) . traverse
    text       <- mayThrow $ textMutedEls ^? ix 1 . allContents . to T.strip
    (t, changed) <- liftIO $ upsertVersionConflict clush $ ImdbSearch{version=coerce ?version,..}
    traceM $ T.unpack [st|#{unTid rowid} [#{show changed}]|]
  where
    clush ImdbSearch{popularity=p1} ImdbSearch{popularity=p2,..} = ImdbSearch{popularity=p1 <> p2, ..}

-- * Scrape ImdbEpisode

scrapeEpisodes :: (?conn::Connection) => Bool -> [Tid Imdb] -> IO ()
scrapeEpisodes continue imdbIds =
  void $ newVersionOrContinue continue $ unEio do
    for_ imdbIds scrapeEpisodes0

scrapeEpisodes0
  :: (?conn::Connection, ?version::NewVersion)
  => Tid Imdb -> Eio ScrapeError ()
scrapeEpisodes0 imdbId = do
  seasons <- scrapeEpisodes1 imdbId "1"
  for_ (L.drop 1 seasons) (void . scrapeEpisodes1 imdbId)

scrapeEpisodes1
  :: (?conn::Connection, ?version::NewVersion)
  => Tid Imdb -> Text -> Eio ScrapeError [Text]
scrapeEpisodes1 imdbId seasonNum = do
  tags <- parseTags . T.decodeUtf8 . BSL.toStrict .  (^. Wreq.responseBody) <$> httpGet ("https://www.imdb.com/title/" <> T.unpack (unTid imdbId) <> "/episodes?season=" <> T.unpack seasonNum)
  let
    seasons = fmap (attr "value")
      . partitions (~== ("<option>"::String))
      . L.takeWhile (~/= ("</select>"::String))
      . L.dropWhile (~/= ("<select id=\"bySeason\">"::String))
      $ tags
    episodes = fmap f
      . partitions ((~== ("<div class=\"list_item even\">"::String)) `tagOr` (~== ("<div class=\"list_item odd\">"::String)))
      . L.takeWhile (~/= ("<hr>"::String))
      . L.dropWhile (~/= ("<div class=\"list detail eplist\">"::String))
      $ tags
  for_ episodes \x -> do
    (t, changed) <- liftIO $ upsertVersion x
    let rid = unTid $ getField @"rowid" t
    traceM $ T.unpack [st|#{rid} [#{show changed}]|]
  pure seasons

  where
    tagOr :: (a -> Bool) -> (a -> Bool) -> a -> Bool
    tagOr f g a = f a || g a

    f :: [Tag Text] -> ImdbEpisode
    f tt = ImdbEpisode{version=coerce ?version, ..} where
      href = attr "href" . L.dropWhile (~/= ("<a>"::String)) $ tt
      episode = attr "content" . L.dropWhile (~/= ("<meta itemprop=\"episodeNumber\">"::String)) $ tt
      rowid = Tid $ T.pack $ maybeTrace $ href ^? to T.unpack . regex [r|/title/([[:alnum:]\-_]+)|] . captures . ix 0
      deleted = False
      parent = imdbId
      season = seasonNum

-- * Scrape ImdbTitle

scrapeTitle :: (?conn::Connection) => Bool -> Tid Imdb -> IO ()
scrapeTitle continue imdbId = void $ newVersionOrContinue continue $ unEio do
  scrapeTitle0 imdbId

scrapeTitle0 :: (?conn::Connection) => Tid Imdb -> Eio ScrapeError ()
scrapeTitle0 imdbId = do
  tt <- parseTags . T.decodeUtf8 . BSL.toStrict .  (^. Wreq.responseBody) <$> httpGet ("https://www.imdb.com/title/" <> T.unpack (unTid imdbId))
  let ttDrop = L.dropWhile (~/= ("<div class=title_wrapper>"::String)) tt
  let title = ttText . L.takeWhile (~/= ("<h1>"::String)) . L.dropWhile (~/= ("<h1>"::String)) $ ttDrop
  undefined

readProgress :: (?conn::Connection, ?version::NewVersion) => IO (M.Map Genre Int)
readProgress = do
  let TableInfo{..} = tableInfo @ImdbSearch
  let tableName::Text = name <> "_versions"
  let v = ?version
  kvs::[(Text, Int)] <- [query|select p.key, max(p.value) from #{tableName}, json_each(popularity) p where version={v} group by p.key|]
  pure $ M.fromList kvs

httpGet :: String -> Eio ScrapeError _
httpGet u = liftIOWith HttpException $ P.putStrLn (u <> "...") *> Wreq.getWith opts u
  where
    opts = Wreq.defaults & Wreq.headers .~ [(hUserAgent, ua), (hAcceptLanguage, lang)]
    ua = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/81.0.4044.122 Safari/537.36"
    lang = "en-US;q=0.9,en;q=0.8"

httpPost :: Wreq.Postable body => String -> body -> Eio ScrapeError _
httpPost u b = liftIOWith HttpException $ P.putStrLn (u <> "...") *> Wreq.postWith opts u b
  where
    opts = Wreq.defaults & Wreq.headers .~ [(hUserAgent, ua), (hAcceptLanguage, lang), (hCookie, cook), (hContentType, ct)]
    ua = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/81.0.4044.122 Safari/537.36"
    lang = "en-US;q=0.9,en;q=0.8"
    cook = "ubid-main=130-8178991-3785115; session-id=140-1954702-4881012; session-id-time=2082787201l; uu=BCYo3Dldb2sxDTahtCC6PoSs8bIIl1GAE9w90XYwq-sY-fFUWbVkPcPcv4DpzdS9njrpdmwOkvZH%0D%0ABDVaHagXZl3WLo0Kv7EdP3pDy4nN0wYqlaOXXXwkC3MhQ0i7DC0KaDyxXidncOGttOsS-D6TP-R2%0D%0AOg%0D%0A; adblk=adblk_yes; session-token=xqXJZi+OwxzBPKbTCbcEwc4A9OnrIO5Fh47VdZLGvEjPoKHmXaH8+wMaBmuvfC/eAHAsqyHpfgAyZryzmBtG6b0xnV+p+Ous7+U6Apn1MH63BCKQWKwrucBDywsqd2LwDq1r7VLapR8EJaIHtoRVvBYKZClI2hyZsrJd1e7QNjAo4q/GDmw+V4NEtHvLOHTb"
    ct = "application/json"

sendGql :: (FromJSON a, Wreq.Postable body) => String -> body -> Eio ScrapeError a
sendGql u b =
  either error P.id . AE.eitherDecode' .  traceShowId . (^. Wreq.responseBody) <$> httpPost u b

allClass cs = allElements . attributed(ix "class" . traverse . nearly "" cond) where
  cond = isJust . L.find (==cs) . T.splitOn " "

hasClass :: Text -> Traversal' (Element Text) (Element Text)
hasClass cs = attributed(ix "class" . traverse . nearly "" cond) where
  cond = isJust . L.find (==cs) . T.splitOn " "

mayThrow :: HasCallStack => Maybe a -> Eio ScrapeError a
mayThrow = maybe (throwError (CallStackException callStack)) pure

paginate :: Int -> [Int]
paginate total = L.takeWhile ((<=total') . fromIntegral) $ fmap (succ . (* 50)) [0..]
  where
    total'::Double = fromIntegral total

attr :: HasCallStack => Text -> [Tag Text] -> Text
attr k = withFrozenCallStack (nemptyTrace . fromAttrib k . headTrace)

ttText :: HasCallStack => [Tag Text] -> Text
ttText = withFrozenCallStack (nemptyTrace . innerText)

test0 :: IO Title
test0 = do
  let q = [st|
{
  title(id: "tt0664521") {
    id
    titleText {
      text
      isOriginalTitle
      country {
        id
        text
      }
      language { id text }
    }
    plots(first: 999) {
      edges {
        node {
          id
          plotText {
            markdown
          }
          plotType
          language {
            id
            text
          }
          isSpoiler
          author
        }
      }
      pageInfo {
        endCursor
        hasNextPage
      }
    }
    images(first: 999) {
      edges {
        node {
          id
          url
          width
          height
          copyright
          createdBy
          source { id text attributionUrl banner { url height width attributionUrl } }
          type
        }
        cursor
      }

      pageInfo {
        endCursor
        hasNextPage
      }
    }
    quotes(first:999) {
      edges {
        node {
          id
          isSpoiler
          lines { characters { character name { id }} text stageDirection }
          interestScore { usersInterested usersVoted }
          language { id text }
        }
        cursor
      }

      pageInfo {
        endCursor
        hasNextPage
      }
    }
  }
} |]
  let body = object ["operationName" .= Null, "query" .= q, "variables" .= object []]
  traceM (show $ AE.encode body)
  x::GQLResponse Title <- unEio $ sendGql "https://graphql.imdb.com/index.html" (AE.encode body)
  pure (coerce x)

newtype GQLResponse a = GQLResponse
  { _data :: GQLTitleResponse a }
  deriving stock (Show, Eq, Generic)

newtype GQLTitleResponse a = GQLTitleResponse
  { title :: a }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

deriveJSON defaultOptions {fieldLabelModifier = \x -> fromMaybe x $ L.stripPrefix "_" x } ''GQLResponse
