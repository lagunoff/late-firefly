module IMDB.Scrape where
  -- ( scrapeSearch
  -- , scrapeEpisodes
  -- , test0
  -- ) where

import Control.Error
import Control.Exception
import Control.Lens as L hiding (children, (.=))
import Data.Aeson as AE
import Data.ByteString.Lazy as BSL
import Data.Char
import Data.Coerce
import Data.Foldable as F
import Data.Generics.Product
import Data.List as L
import Data.Map as M
import Data.Text as T
import Data.Text.Encoding as T
import GHC.Stack
import GHC.TypeLits
import "this" DB
import "this" IMDB.GraphQL
import "this" IMDB.Schema
import "this" Intro
import Network.HTTP.Client hiding (withConnection, Proxy)
import Network.HTTP.Types
import Prelude as P
import Text.HTML.TagSoup as L
import Text.HTML.TagSoup.Lens as L hiding (attr)
import Text.Read hiding (lift)
import Text.Regex.Lens
import Text.Regex.Quote
import Text.Regex.TDFA
import Text.Shakespeare.Text as X (st)
import qualified Network.Wreq as Wreq
import qualified Network.Wreq.Types as Wreq hiding (headers)

data ScrapeError
  = HttpException HttpException
  | CallStackException CallStack
  | GraphQLError String
  deriving (Exception, Show)

genres :: [Genre1]
genres = ["action", "adventure", "animation", "biography", "comedy", "crime", "documentary", "drama", "family", "fantasy", "film_noir", "game_show", "history", "horror", "music", "musical", "mystery", "news", "reality_tv", "romance", "sci_fi", "sport", "talk_show", "thriller", "war", "western"]

-- * Scrape ImdbEpisode

scrapeSearch :: (?conn::Connection) => Bool -> Double -> ServerIO ()
scrapeSearch continue percentile = void $ newVersionOrContinue continue $ do
  prog <- liftIO readProgress
  for_ IMDB.Scrape.genres \g -> do
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
  => Genre1 -> Int -> Eio ScrapeError Int
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
    rowid      <- fmap coerce $ mayThrow $ headerEl ^? allElements . named(only "a").attrOne "href" . to (T.unpack . T.strip) . regex [r|/title/([[:alnum:]\-_]+)|] . captures . ix 0 . to (readMaybe @Int64 . stripPrefix1 "tt") . traverse
    header     <- mayThrow $ headerEl ^? allElements . named(only "a").allContents
    popularity <- fmap (M.singleton g) $ mayThrow $ headerEl ^? allClass "lister-item-index".allContents . to (T.unpack . T.strip) . regex [r|^([\s,[:digit:]]+)\.?$|] . captures . ix 0 . to (readMaybe @Int . L.filter isDigit) . traverse
    text       <- mayThrow $ textMutedEls ^? ix 1 . allContents . to T.strip
    (t, changed) <- liftIO $ upsertVersionConflict clush $ ImdbSearch{version=coerce ?version,..}
    traceM $ T.unpack [st|#{showt (unId rowid)} [#{show changed}]|]
  where
    clush ImdbSearch{popularity=p1} ImdbSearch{popularity=p2,..} = ImdbSearch{popularity=p1 <> p2, ..}

-- * Scrape ImdbTitle

readProgress :: (?conn::Connection, ?version::NewVersion) => IO (M.Map Genre1 Int)
readProgress = do
  let TableInfo{..} = tableInfo @ImdbSearch
  let tableName::Text = name <> "_versions"
  let v = ?version
  kvs::[(Text, Int)] <- query [sql|select p.key, max(p.value) from #{tableName}, json_each(popularity) p where version={v} group by p.key|]
  pure $ M.fromList kvs

httpGet :: String -> Eio ScrapeError _
httpGet u = liftIOWith HttpException $ P.putStrLn (u <> "...") *> Wreq.getWith opts u
  where
    opts = Wreq.defaults & Wreq.headers .~ [(hUserAgent, ua), (hAcceptLanguage, lang)]
    ua = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/81.0.4044.122 Safari/537.36"
    lang = "en-US;q=0.9,en;q=0.8"

httpPost :: Wreq.Postable body => String -> body -> Eio ScrapeError _
httpPost u b = liftIOWith HttpException $ Wreq.postWith opts u b
  where
    opts = Wreq.defaults & Wreq.headers .~ [(hUserAgent, ua), (hAcceptLanguage, lang), (hCookie, cook), (hContentType, ct)]
    ua = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/81.0.4044.122 Safari/537.36"
    lang = "en-US;q=0.9,en;q=0.8"
    cook = "ubid-main=130-8178991-3785115; session-id=140-1954702-4881012; session-id-time=2082787201l; uu=BCYo3Dldb2sxDTahtCC6PoSs8bIIl1GAE9w90XYwq-sY-fFUWbVkPcPcv4DpzdS9njrpdmwOkvZH%0D%0ABDVaHagXZl3WLo0Kv7EdP3pDy4nN0wYqlaOXXXwkC3MhQ0i7DC0KaDyxXidncOGttOsS-D6TP-R2%0D%0AOg%0D%0A; adblk=adblk_yes; session-token=xqXJZi+OwxzBPKbTCbcEwc4A9OnrIO5Fh47VdZLGvEjPoKHmXaH8+wMaBmuvfC/eAHAsqyHpfgAyZryzmBtG6b0xnV+p+Ous7+U6Apn1MH63BCKQWKwrucBDywsqd2LwDq1r7VLapR8EJaIHtoRVvBYKZClI2hyZsrJd1e7QNjAo4q/GDmw+V4NEtHvLOHTb"
    ct = "application/json"

sendGql
  :: forall f a q.
  (FromJSON a, ToJSON q, KnownSymbol f)
  => String -> q -> Eio ScrapeError a
sendGql u q = do
  let b = AE.encode $ object ["operationName" .= Null, "query" .= q, "variables" .= object []]
  either error imdb_response . AE.eitherDecode' @(ImdbResponse f a) . (^. Wreq.responseBody) <$> httpPost u b

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

test0 :: IO ()
test0 = void $ withConnection "late-firefly.sqlite" do
  for_ $mkDatabaseSetup execute
  let
    g x = "coalesce(json_extract(popularity, '$." <> x <> "'), 99999)"
    po = T.intercalate "," (fmap g IMDB.Scrape.genres)
  tids::[Only (Id ImdbTitle)] <- query [sql|
    with s as
      (select rowid, min(#{po}) as pop from imdb_search group by rowid)
      select rowid from s where rowid not in (select rowid from imdb_title) order by pop
    |]
  progressInc (L.length tids) do
    for_ tids \(Only (Id x)) -> do
      inc ?progress
      let tid = ImdbId x
      prefixProgress (showt tid <> "/") do
        scrapeTitle tid

scrapeTitle :: (?conn::Connection, ?progress::Progress) => ImdbId "tt" -> IO ()
scrapeTitle tid = do
  TitleChunk{..} <- scrapeTitle1 tid
  transaction do
    for_ title upsert
    for_ images upsert
    for_ plots upsert
    for_ genres upsert
    for_ titleToGenre upsert
    for_ keywords upsert
    for_ titleToKeyword upsert
    for_ names upsert
    for_ credits upsert

scrapeTitle1 :: (?progress::Progress) => ImdbId "tt" -> IO TitleChunk
scrapeTitle1 tid = do
  let q = qTitle tid
  display ?progress (showt tid)
  x::Title <- unEio $ sendGql @"title" "https://graphql.imdb.com/index.html" q
  eps <- scrapeEpisodes tid
  let chunk = fromTitle x::TitleChunk
  titls <- mapM scrapeTitle1 eps
  pure $ F.fold (chunk:titls)

scrapeEpisodes :: ImdbId "tt" -> IO [ImdbId "tt"]
scrapeEpisodes tid = go Nothing where
  go after = do
    let q = qEpisodes tid after
    x::Pick Title '["id", "episodes"] <- unEio $ sendGql @"title" "https://graphql.imdb.com/index.html" q
    let episodes = getField @"episodes" x
    let episodes' = episodes ^.. _Just . field @"episodes" . _Just . field @"edges" . traverse . field @"node" . field @"id"
    let cursor = episodes ^? _Just . field @"episodes" . _Just . field @"pageInfo" . field @"endCursor" . _Just
    maybe (pure episodes') (\x -> (episodes' <>) <$> go (Just x)) cursor

qEpisodes :: ImdbId "tt" -> Maybe Text -> Text
qEpisodes tid after =
  let after' = maybe "" ((", after: "<>) . show) after
  in [st|
{
  title(id: "#{showt tid}") {
    id
    episodes {
      episodes(first: 100#{after'}) {
        edges {
          node {
            id
          }
        }
        pageInfo {
          endCursor
          hasNextPage
        }
      }
    }
  }
} |]

qTitle :: ImdbId "tt"-> Text
qTitle tid = [st|
{
  title(id: "#{showt tid}") {
    id
    plot { id }
    plots(first:999) {
      edges {
        node {
          id plotText { markdown } plotType language { id text } isSpoiler author
        }
      }
      pageInfo { hasNextPage }
    }

    primaryImage {
      id
      url
      width
      height
    }

    series {
      series { id }
      episodeNumber { episodeNumber seasonNumber }
      nextEpisode { id }
      previousEpisode { id }
    }

    countriesOfOrigin {
      countries { id text }
      language { id text }
    }

    releaseYear { year endYear }

    titleType {id text}

    originalTitleText {text isOriginalTitle country{id text} language{id text}}

    releaseDate{ day month year country {id text} }

    runtime{ seconds country {id text} }

    certificate{ rating country {id text} ratingsBody ratingReason }

    userRating{ date value }

    genres{
      genres {id text}
      language {id text}
    }

    keywords(first: 999){
      edges {
        node {
          id
          text
          interestScore { usersInterested usersVoted }
        }
      }
      pageInfo {
        endCursor
        hasNextPage
      }
    }

    credits(first: 999){
      edges {
        node {
          name {
            id nameText {text}
            primaryImage {
              id
              url
              width
              height
            }
          }
          title {id}
          category {id text}
        }
      }
      pageInfo {
        endCursor
        hasNextPage
      }
    }
  }
} |]
