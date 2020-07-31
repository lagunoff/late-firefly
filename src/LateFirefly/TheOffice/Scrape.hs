module LateFirefly.TheOffice.Scrape where

import Control.Lens as L hiding (children)
import Data.ByteString.Lazy as BSL
import Data.Foldable
import Data.Generics.Product
import Data.List as L
import Data.List.Split as Split
import Data.Text as T
import Data.Text.Encoding as T
import GHC.Stack
import LateFirefly.DB
import LateFirefly.Prelude
import LateFirefly.TheOffice.Schema
import Network.HTTP.Types
import Prelude as P
import Text.HTML.TagSoup
import Text.Read
import Text.Regex.Lens
import Text.Regex.Quote
import Text.Regex.TDFA
import qualified Data.Map as M
import qualified Network.Wreq as Wreq

test0 :: IO ()
test0 = do
  void $ withConnection "late-firefly.sqlite" do
    for_ $mkDatabaseSetup execute
    scrapeSeasons

episodeNumberPresets = M.fromList
  [("/s07e12/"::Text, 11), ("/s07e26/", 24)]

videoTitlePresets = M.fromList
  [ ("/s07e11/"::Text, "Classy Christmas, Part 1")
  , ("/s07e12/"::Text, "Classy Christmas, Part 2")
  , ("/s07e25/"::Text, "Search Committee, Part 1")
  , ("/s07e26/"::Text, "Search Committee, Part 2") ]

scrapeSeasons
  :: (?conn::Connection) => IO [Int]
scrapeSeasons = do
  tags <- parseTags . T.decodeUtf8 . BSL.toStrict .  (^. Wreq.responseBody) <$> httpGet "https://iwatchtheoffice.com/season-list/"
  let
    theOfficeId = Id 386676
    seasons = fmap takeSeason
      . partitions (~== ("<div id=\"outer\">"::String))
      . L.takeWhile (~/= ("<div class=\"under_post_a\">"::String))
      . L.dropWhile (~/= ("<div class=\"entry-content\">"::String))
      $ tags
  for_ (seasons) \snum -> do
    eps <- scrapeEpisodes snum
    let
      go ix' e = do
        let ix = M.lookup e episodeNumberPresets ?: ix'
        let vtitl = M.lookup e videoTitlePresets
        links <- scrapeLinks e
        Only titleId:_ <- doQuery [sql|select rowid from imdb_title where series_title_id={theOfficeId} and series_episode_number={ix} and series_season_number={snum}|]
        for_ links \l -> do
          upsert $ VideoLink def titleId (Just e) vtitl l (Just "iwatchtheoffice.com")
        pure (ix + 1)
    foldM_ go (1::Int) (eps)
  pure seasons
  where
    takeSeason :: [Tag Text] -> Int
    takeSeason = maybeTrace
      . (^? regex [r|season-([[:digit:]]+)/?$|] . captures . traversed . L.index 0 . L.to (readMaybe @Int) . _Just)
      . T.unpack . attr "href" . L.dropWhile (~/= ("<a>"::String))

scrapeEpisodes :: (?conn::Connection) => Int -> IO [Text]
scrapeEpisodes season = do
  let seasonHref = "season-" <> show season
  tags <- parseTags . T.decodeUtf8 . BSL.toStrict .  (^. Wreq.responseBody) <$> httpGet ("https://iwatchtheoffice.com/" <> seasonHref)
  let
    episodes = fmap takeSeason
      . partitions (~== ("<div id=\"outer\">"::String))
      . L.takeWhile (~/= ("<div class=\"under_post_a\">"::String))
      . L.dropWhile (~/= ("<div class=\"entry-content\">"::String))
      $ tags
  pure episodes
  where
    takeSeason = attr "href" . L.dropWhile (~/= ("<a>"::String))

scrapeLinks :: (?conn::Connection) => Text -> IO [Text]
scrapeLinks episodeHref = do
  tags <- parseTags . T.decodeUtf8 . BSL.toStrict .  (^. Wreq.responseBody) <$> httpGet ("https://iwatchtheoffice.com" <> T.unpack episodeHref)
  let
    links = fmap (attr "href")
      . partitions (~== ("<a>"::String))
      . L.takeWhile (~/= ("<div class=\"playerz_box\">"::String))
      . L.dropWhile (~/= ("<div class=\"linkz_box\">"::String))
      $ tags
  pure links

attr :: HasCallStack => Text -> [Tag Text] -> Text
attr k = withFrozenCallStack (nemptyTrace . fromAttrib k . headTrace)

httpGet :: String -> IO _
httpGet u = P.putStrLn (u <> "...") *> Wreq.getWith opts u
  where
    opts = Wreq.defaults & Wreq.headers .~ [(hUserAgent, ua), (hAcceptLanguage, lang)]
    ua = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/81.0.4044.122 Safari/537.36"
    lang = "en-US;q=0.9,en;q=0.8"
