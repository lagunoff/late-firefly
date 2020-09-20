{-# LANGUAGE Strict #-}
module IMDB.Scrape where

import Control.Error hiding (bool)
import Control.Monad
import Control.Exception
import Control.Lens hiding (children, (.=))
import Data.Aeson as AE
import Data.ByteString.Lazy as BSL
import Data.ByteString.Lazy.Char8 as BSL8
import Data.Char
import Data.Coerce
import Data.Foldable as F
import Data.Generics.Product
import Data.List as L
import Data.Map as M
import Data.Text as T hiding (chunksOf)
import Data.Text.Encoding as T
import GHC.Stack
import GHC.TypeLits
import Network.HTTP.Client hiding (withConnection, Proxy)
import Network.HTTP.Types
import Prelude as P
import Text.HTML.TagSoup.Lens as L hiding (attr)
import Text.Read hiding (lift)
import Text.Regex.Lens
import Text.Regex.Quote
import Text.Regex.TDFA
import Text.Shakespeare.Text as X (st)
import qualified Network.Wreq as Wreq
import qualified Network.Wreq.Types as Wreq hiding (headers)
import Codec.Compression.Zlib.Internal as Zlib

import "this" DB
import "this" IMDB.Types
import "this" IMDB.GraphQL
import "this" IMDB.Schema
import "this" Intro

data ScrapeError
  = HttpException HttpException
  | CallStackException CallStack
  | GraphQLError String
  | SQLError SQLError
  deriving (Exception, Show, Generic)

type ScrapeIO = Eio ScrapeError

genres :: [Genre1]
genres = ["action", "adventure", "animation", "biography", "comedy", "crime", "documentary", "drama", "family", "fantasy", "film_noir", "game_show", "history", "horror", "music", "musical", "mystery", "news", "reality_tv", "romance", "sci_fi", "sport", "talk_show", "thriller", "war", "western"]

-- * Scrape ImdbEpisode

scrapeSearch :: (?conn::Connection) => Bool -> Double -> ScrapeIO ()
scrapeSearch continue percentile = void $ newVersionOrContinue continue $ do
  prog <- readProgress
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
  => Genre1 -> Int -> ScrapeIO Int
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
    (t, changed) <- upsertVersionConflict clush $ ImdbSearch{version=coerce ?version,..}
    traceM $ T.unpack [st|#{showt (unId rowid)} [#{show changed}]|]
  where
    clush ImdbSearch{popularity=p1} ImdbSearch{popularity=p2,..} = ImdbSearch{popularity=p1 <> p2, ..}

-- * Scrape ImdbTitle

readProgress :: (?conn::Connection, ?version::NewVersion) => ScrapeIO (M.Map Genre1 Int)
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

httpPost :: Wreq.Postable body => String -> body -> ScrapeIO _
httpPost u b = liftIOWith HttpException $ Wreq.postWith opts u b
  where
    opts = Wreq.defaults & Wreq.headers .~ [(hUserAgent, ua), (hAcceptLanguage, lang), (hCookie, cook), (hContentType, ct)]
    ua = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/81.0.4044.122 Safari/537.36"
    lang = "en-US;q=0.9,en;q=0.8"
    cook = "ubid-main=130-8178991-3785115; session-id=140-1954702-4881012; session-id-time=2082787201l; uu=BCYo3Dldb2sxDTahtCC6PoSs8bIIl1GAE9w90XYwq-sY-fFUWbVkPcPcv4DpzdS9njrpdmwOkvZH%0D%0ABDVaHagXZl3WLo0Kv7EdP3pDy4nN0wYqlaOXXXwkC3MhQ0i7DC0KaDyxXidncOGttOsS-D6TP-R2%0D%0AOg%0D%0A; adblk=adblk_yes; session-token=xqXJZi+OwxzBPKbTCbcEwc4A9OnrIO5Fh47VdZLGvEjPoKHmXaH8+wMaBmuvfC/eAHAsqyHpfgAyZryzmBtG6b0xnV+p+Ous7+U6Apn1MH63BCKQWKwrucBDywsqd2LwDq1r7VLapR8EJaIHtoRVvBYKZClI2hyZsrJd1e7QNjAo4q/GDmw+V4NEtHvLOHTb"
    ct = "application/json"

sendGql
  :: forall f a q. (FromJSON a, ToJSON q, KnownSymbol f)
  => String -> q -> ScrapeIO a
sendGql u q = do
  let b = AE.encode $ object ["operationName" .= Null, "query" .= q, "variables" .= object []]
  either error imdb_response . AE.eitherDecode' @(ImdbResponse f a) . (^. Wreq.responseBody) <$> httpPost u b

allClass cs = allElements . attributed(ix "class" . traverse . nearly "" cond) where
  cond = isJust . L.find (==cs) . T.splitOn " "

hasClass :: Text -> Traversal' (Element Text) (Element Text)
hasClass cs = attributed(ix "class" . traverse . nearly "" cond) where
  cond = isJust . L.find (==cs) . T.splitOn " "

mayThrow :: HasCallStack => Maybe a -> ScrapeIO a
mayThrow = maybe (throwError (CallStackException callStack)) pure

paginate :: Int -> [Int]
paginate total = L.takeWhile (<=total) . fmap (succ . (* 50)) $ [0..]

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n l
  | n > 0 = (L.take n l) : (chunksOf n (L.drop n l))
  | otherwise = error "Negative or zero n"

scrapeGql :: ScrapeIO ()
scrapeGql = $withDb do
  tids::[Only (Id ImdbTitle)] <- query [sql|
    with movies as (
      select
        vm.rowid as rowid,
        trt.num_votes as num_votes
        from vidsrc_movie vm
          left join title_ratings_tsv trt on trt.rowid=vm.imdb_id
        where
          vm.rowid not in (select rowid from imdb_title)
    )
    , series as (
      select
        tet.parent_id as rowid,
        trt.num_votes as num_votes
        from title_episode_tsv tet
          left join title_ratings_tsv trt on trt.rowid=tet.parent_id
        where
          tet.parent_id in (select show_imdb_id from vidsrc_episode)
          and tet.parent_id not in (select rowid from imdb_title)
        group by tet.parent_id, trt.num_votes
    )
    , episodes as (
      select
        tet.rowid as rowid,
        trt.num_votes as num_votes
        from title_episode_tsv tet
          left join title_ratings_tsv trt on trt.rowid=tet.parent_id
        where
          tet.parent_id in (select show_imdb_id from vidsrc_episode)
          and tet.rowid not in (select rowid from imdb_title)
    )
    , united as (
      select * from movies
        union all select * from series
        union all select * from episodes
    )
    select rowid from united
      order by num_votes desc
    |]
  let n = 25
  let chunks = chunksOf n $ fmap (\(Only (Id x)) -> ImdbId x) tids
  knownProgress 1 (L.length chunks * n) \Progress{..} -> do
    for_ chunks \chunk -> do
      for_ (L.uncons chunk) \(t, _) -> report (showt t)
      scrapeTitle chunk
      incBy n

scrapeTitle :: (?conn::Connection) => [ImdbId "tt"] -> ScrapeIO ()
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

scrapeTitle1 :: [ImdbId "tt"] -> ScrapeIO TitleChunk
scrapeTitle1 tids = do
  let q = qTitles tids
  xs::[Title] <- sendGql @"titles" "https://graphql.imdb.com/index.html" q
  pure (foldMap toDb xs)

quo x = "\"" <> x <> "\""

qTitles :: [ImdbId "tt"]-> Text
qTitles tids = [st|
{
  titles(ids: [#{T.intercalate "," $ fmap (quo . showt) tids}]) {
    id
    plot { id }
    plots(first:250) {
      edges {
        node {
          id plotText { markdown } plotType isSpoiler author
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

    originalTitleText {text}

    genres{
      genres {id text}
    }

    keywords(first: 250){
      edges {
        node {
          id
          text
        }
      }
      pageInfo {
        endCursor
        hasNextPage
      }
    }
  }
} |]

scrapeTsv :: Text -> SqlIO ()
scrapeTsv file = withConnectionSetup $collectTables do
  let
    process :: (DbTable t, HasField' "rowid" t (Id t)) => (BSL.ByteString -> Either String t) -> SqlIO ()
    process f = do
      bsl <- liftIO $ BSL.readFile $ T.unpack file
      let unbsl = Zlib.decompress gzipFormat defaultDecompressParams bsl
      let lines = BSL.split 10 unbsl
      knownProgress 100 (L.length lines)
        \Progress{..} -> upsertCb
        \upsert -> for_ lines
        \line -> let
          left = ((inc *>) . (traceShowM line *>) . traceShowM)
          right x = upsert x *> inc *> report (T.pack $ show $ getField @"rowid" x)
          in either left right $ f line
    basics line = do
      (c1,c2,c3,c4,c5,c6,c7,c8) <- case BSL.split 9 line of
        (c1:c2:c3:c4:c5:c6:c7:c8:_) -> Right (c1,c2,c3,c4,c5,c6,c7,c8)
        _                           -> Left "Insufficient columns"
      let titleType = ptext c2
      let startYear = hush $ pint c6
      let endYear = hush $ pint c7
      let runtimeMinutes = hush $ pint c8
      rowid <- Id . unImdbId <$> (pimdb c1)
      primaryTitle <- pmtext c3
      originalTitle <- pmtext c4
      isAdult <- pbool c5
      pure TitleBasicsTsv{..}
    episodes line = do
      (c1,c2,c3,c4) <- case BSL.split 9 line of
        (c1:c2:c3:c4:_) -> Right (c1,c2,c3,c4)
        _                   -> Left "Insufficient columns"
      rowid <- Id . unImdbId <$> (pimdb c1)
      parentId <- Id . unImdbId <$> (pimdb c2)
      let seasonNumber = hush $ pint c3
      let episodeNumber = hush $ pint c4
      pure TitleEpisodeTsv{..}
    ratings line = do
      (c1,c2,c3) <- case BSL.split 9 line of
        (c1:c2:c3:_) -> Right (c1,c2,c3)
        _            -> Left "Insufficient columns"
      rowid <- Id . unImdbId <$> (pimdb c1)
      averageRating <- pdouble c2
      numVotes <- pint c3
      pure TitleRatingsTsv{..}
    action
      | T.isSuffixOf "title.basics.tsv.gz" file = process basics
      | T.isSuffixOf "title.episode.tsv.gz" file = process episodes
      | T.isSuffixOf "title.ratings.tsv.gz" file = process ratings
      | otherwise = error "Unknown file format"
  action
  where
    ptext = T.decodeUtf8 . BSL.toStrict
    pne = note "Non empty failed" . mfilter (/="") . mfilter (/="\\N") . Just
    pmtext = fmap ptext . pne
    pimdb = imdbFromText @"tt" . ptext
    pint = note "pint failed" . readMaybe @Int . BSL8.unpack <=< pne
    pdouble = note "pdouble failed" . readMaybe @Double . BSL8.unpack <=< pne
    pbool = fmap (/=0) . pint
