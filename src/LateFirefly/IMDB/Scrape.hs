module LateFirefly.IMDB.Scrape (scrapeSite) where

import Control.Lens as L hiding (children)
import Control.Error
import Data.Text as T
import Data.List as L
import Data.ByteString.Lazy as BSL
import Data.Text.Encoding as T
import Text.Read hiding (lift)
import LateFirefly.DB
import LateFirefly.Prelude
import LateFirefly.IMDB.Schema
import Text.HTML.TagSoup.Lens as L
import qualified Network.Wreq as Wreq
import Text.Regex.Lens
import Text.Regex.Quote
import Text.Regex.TDFA
import GHC.Stack

scrapeSite :: (?conn :: Connection) => IO ()
scrapeSite = print =<< (newVersion $ throwToEither do
  scrapeSearchItems "drama")

scrapeSearchItems
  :: (?throw::CallStack, ?conn::Connection, ?version::NewVersion)
  => Text -> IO [SearchItem]
scrapeSearchItems g = do
  markup <- T.decodeUtf8 . BSL.toStrict . (^. Wreq.responseBody) <$> httpGet ("https://www.imdb.com/search/title/?genres=" <> T.unpack g)
  let episodeOuters = markup^.._DOM.traverse.allElements.hasClass "lister-item"
  for episodeOuters \el -> do
    let deleted      = False
    let certificate  = el ^? allClass "text-muted" . allClass "certificate" . allContents
    let runtime      = el ^? allClass "text-muted" . allClass "runtime" . allContents
    let genre        = el ^.. allClass "text-muted" . allClass "genre" . allContents. to (L.filter (/=mempty) . fmap T.strip . T.splitOn ",") . traverse
    let textMutedEls = el ^.. allClass "lister-item-content" . children . traverse . elt . hasClass "text-muted"
    let rating       = el ^? allClass "ratings-bar". allElements . named(only "strong").allContents
    let headerEl     = maybeThrowStack $ el ^? allClass "lister-item-header"
    let href         = maybeThrowStack $ headerEl ^? allElements . named(only "a").attrOne "href"
    let header       = maybeThrowStack $ headerEl ^? allElements . named(only "a").allContents
    let index        = maybeThrowStack $ headerEl^? allClass "lister-item-index".allContents . to (T.unpack . T.strip) . regex [r|^([[:digit:]]+)\.?$|] . captures . ix 0 . to readMaybe . traverse
    let text         = maybeThrowStack $ textMutedEls ^? ix 1 . allContents . to T.strip
    pure (fixUUID \uuid -> SearchItem{version=coerce ?version,..})

httpGet :: String -> IO _
httpGet u = Prelude.putStrLn (u <> "...") *> Wreq.get u

hasInfix :: Text -> Prism' Text ()
hasInfix cs = nearly "" (isJust . L.find (==cs) . T.splitOn " ")

allClass cs = allElements . attributed(ix "class" . traverse . hasInfix cs)

hasClass :: Text -> Traversal' (Element Text) (Element Text)
hasClass cs = attributed(ix "class" . traverse . hasInfix cs)
