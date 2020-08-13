module LateFirefly.Scrape.IsubsMovies where

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
import LateFirefly.Schema
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
  void $ withConnectionEnv do
    for_ $mkDatabaseSetup execute
    scrapeSeasons

scrapeSeasons :: (?conn::Connection) => IO ()
scrapeSeasons = do
  tags <- parseTags . T.decodeUtf8 . BSL.toStrict .  (^. Wreq.responseBody) <$> httpGet "https://isubsmovies.com/tvserie/watch-the-office-online-0386676"
  let
    seasons = fmap takeSeason
      . partitions ((~== ("<div class=\"subtitles col-md-4\">"::String)) |||(~== ("<div class=\"subtitles col-md-4 clearLeft colWithoutPaddingLeft\">"::String)) ||| (~== ("<div class=\"subtitles col-md-4 colWithoutPaddingRight\">"::String)))
      . L.takeWhile (~/= ("<div class=\"container m-t-lg m-b-lg\">"::String))
      . L.dropWhile (~/= ("<div class=\"container seasons m-t-lg\">"::String))
      $ tags
  traceShowM $ L.length seasons
  traceShowM seasons
  where
    takeSeason :: [Tag Text] -> Text
    takeSeason = innerText . L.dropWhile (~/= ("<th>"::String))

    (|||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
    (|||) f g a = f a || g a

httpGet :: String -> IO _
httpGet u = P.putStrLn (u <> "...") *> Wreq.getWith opts u
  where
    opts = Wreq.defaults & Wreq.headers .~ [(hUserAgent, ua), (hAcceptLanguage, lang)]
    ua = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/81.0.4044.122 Safari/537.36"
    lang = "en-US;q=0.9,en;q=0.8"
