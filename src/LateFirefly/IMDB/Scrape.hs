module LateFirefly.IMDB.Scrape (scrapeSite) where

import Control.Lens as L hiding (children)
import Control.Exception
import Control.Error
import Data.Text as T
import Data.Char
import Data.List as L
import Data.Map as M
import Prelude as P
import Data.ByteString.Lazy as BSL
import Data.Text.Encoding as T
import Text.Read hiding (lift)
import LateFirefly.DB
import LateFirefly.Prelude
import LateFirefly.IMDB.Schema
import Text.HTML.TagSoup.Lens as L
import Text.HTML.TagSoup as L
import qualified Network.Wreq as Wreq
import Text.Regex.Lens
import Text.Regex.Quote
import Text.Regex.TDFA
import Network.HTTP.Client
import GHC.Stack
import Network.HTTP.Types
import Text.Shakespeare.Text as X (st)

data ScrapeError
  = HttpException HttpException
  | CallStackException CallStack
  deriving (Exception, Show)

scrapeSite :: (?conn::Connection) => Bool -> Double -> IO ()
scrapeSite continue percentile = void $ newVersionOrContinue continue $ unEio (scrapeEpisodes "tt0386676" 1)
-- unEio do
--   prog <- liftIO readProgress
--   for_ genres \g -> do
--     (\x -> foldM_ x Nothing (paginate 9951)) \total p -> do
--       let p' = fromIntegral p
--       let total' = fmap fromIntegral total
--       case (fmap ((p' <) . (*percentile)) total') of
--         Just False -> pure total
--         _          -> do
--           let progress = fromMaybe 0 $ M.lookup g prog
--           case (progress >= p + 49) of
--             True  -> Nothing <$ (traceM $ "Skipping: " <> ("https://www.imdb.com/search/title/?genres=" <> T.unpack g <> "&start=" <> show p))
--             False -> Just <$> (scrapeSearchItems g p)

genres :: [Genre]
genres = ["action", "adventure", "animation", "biography", "comedy", "crime", "documentary", "drama", "family", "fantasy", "film_noir", "game_show", "history", "horror", "music", "musical", "mystery", "news", "reality_tv", "romance", "sci_fi", "sport", "talk_show", "thriller", "war", "western"]

scrapeSearchItems
  :: (?conn::Connection, ?version::NewVersion)
  => Genre -> Int -> Eio ScrapeError Int
scrapeSearchItems g start = do
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
    let year =el ^? allClass "lister-item-year" . allContents . to (T.dropWhile (=='(') . T.dropWhileEnd (==')'))
    headerEl <- mayThrow $ el ^? allClass "lister-item-header"
    imdbId   <- mayThrow $ headerEl ^? allElements . named(only "a").attrOne "href" . to (T.unpack . T.strip) . regex [r|/title/([[:alnum:]\-_]+)|] . captures . ix 0 . to T.pack
    header   <- mayThrow $ headerEl ^? allElements . named(only "a").allContents
    popularity <- fmap (M.singleton g) $ mayThrow $ headerEl ^? allClass "lister-item-index".allContents . to (T.unpack . T.strip) . regex [r|^([\s,[:digit:]]+)\.?$|] . captures . ix 0 . to (readMaybe @Int . L.filter isDigit) . traverse
    text     <- mayThrow $ textMutedEls ^? ix 1 . allContents . to T.strip
    (t, changed) <- liftIO $ upsertWith clush (fixUUID \uuid -> SearchItem{version=coerce ?version,..})
    traceM $ T.unpack [st|#{imdbId} [#{show changed}]|]
  where
    clush SearchItem{popularity=p1} SearchItem{popularity=p2,..} = SearchItem{popularity=p1 <> p2, ..}

scrapeEpisodes
  :: (?conn::Connection, ?version::NewVersion)
  => ImdbId -> Int -> Eio ScrapeError ()
scrapeEpisodes imdbId seasonNum = do
  traceM $ show imdbId <> " " <> show seasonNum
  tags <- parseTags . T.decodeUtf8 . BSL.toStrict .  (^. Wreq.responseBody) <$> httpGet ("https://www.imdb.com/title/tt0386676/episodes?season=" <> show seasonNum)
  let
    papers = fmap f
      . partitions ((~== ("<div class=\"list_item even\">"::String)) `tagOr` (~== ("<div class=\"list_item odd\">"::String)))
      . L.takeWhile (~/= ("<hr>"::String))
      . L.dropWhile (~/= ("<div class=\"list detail eplist\">"::String))
      $ tags
  traceM $ show papers
  traceM $ show (L.length papers)
  where
    tagOr :: (a -> Bool) -> (a -> Bool) -> a -> Bool
    tagOr f g a = f a || g a

    f :: [Tag Text] -> Text
    f = fromAttrib "src" . L.head . L.dropWhile (~/= ("<img>"::String))

    hasClass :: Text -> Tag Text -> Bool
    hasClass cl = hasClasses [cl]

    hasClasses :: [Text] -> Tag Text -> Bool
    hasClasses cls = \case
      TagOpen _ attrs -> case L.lookup "class" attrs of
        Just x -> (\xs -> L.all (isJust .  flip L.find xs . (==)) cls) . T.splitOn " " $ x
        Nothing -> False
      _ -> False

readProgress :: (?conn::Connection, ?version::NewVersion) => IO (M.Map Genre Int)
readProgress = do
  let TableInfo{..} = tableInfo @SearchItem
  let tableName::Text = name <> "_versions"
  let q = [sql|select p.key, max(p.value) from #{tableName}, json_each(popularity) p where version=? group by p.key|]
  kvs::[(Text, Int)] <- query q [?version]
  pure $ M.fromList kvs

httpGet :: String -> Eio ScrapeError _
httpGet u = liftIOWith HttpException $ P.putStrLn (u <> "...") *> Wreq.getWith opts u
  where
    opts = Wreq.defaults & Wreq.headers .~ [(hUserAgent, ua), (hAcceptLanguage, lang)]
    ua = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/81.0.4044.122 Safari/537.36"
    lang = "en-US;q=0.9,en;q=0.8"

allClass cs = allElements . attributed(ix "class" . traverse . nearly "" cond) where
  cond = isJust . L.find (==cs) . T.splitOn " "

hasClass :: Text -> Traversal' (Element Text) (Element Text)
hasClass cs = attributed(ix "class" . traverse . nearly "" cond) where
  cond = isJust . L.find (==cs) . T.splitOn " "

hasClasses :: [Text] -> Traversal' (Element Text) (Element Text)
hasClasses css = attributed(ix "class" . traverse . nearly "" cond) where
  cond = (\xs -> L.all (isJust .  flip L.find xs . (==)) css) . T.splitOn " "

mayThrow :: HasCallStack => Maybe a -> Eio ScrapeError a
mayThrow = maybe (throwError (CallStackException callStack)) pure

paginate :: Int -> [Int]
paginate total = L.takeWhile ((<=total') . fromIntegral) $ fmap (succ . (* 50)) [0..]
  where
    total'::Double = fromIntegral total
