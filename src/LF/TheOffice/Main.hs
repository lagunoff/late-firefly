module Main where

import Control.Lens as L hiding (children)
import Data.Text as T
import Data.Text.IO as T
import Data.Text.Lazy as TL hiding (Text)
import Data.Text.Lazy.Encoding as TL
import Data.Generics.Product
import LF.DB
import LF.Prelude
import LF.TheOffice.Schema
import Options.Generic
import qualified Network.Wreq as Wreq
import Text.HTML.TagSoup.Lens (allAttributed, allElements, allNamed, attrOne,
                               attributed, children, contents, named, _DOM)
import Text.Regex.Lens
import Text.Regex.Quote
import Text.Regex.TDFA
import Text.Read

-- | Cli arguments
data Args
  = Update {dbpath :: Maybe Text}
  | GoodBye
  deriving (Show, Generic, ParseRecord)

main :: IO ()
main = do
  options <- getRecord "Parser for https://iwatchtheoffice.com/"
  case options of
    Update{dbpath=mayDb,..} -> do
      let dbpath = maybe "./late-firefly.sqlite" T.unpack mayDb
      withConnection dbpath do
        for_ $(mkInitStmts) execute_
        newVersion do
          scrapeSeasons >>= mapM_ (uncurry scrapeEpisodes)
      T.putStrLn "Goodbye..."
    GoodBye ->
      T.putStrLn "Goodbye..."

scrapeSeasons :: (Given Connection, Given Version) => IO [(Season, String)]
scrapeSeasons = do
  markup <- TL.decodeUtf8 . (^. Wreq.responseBody) <$> httpGet "https://iwatchtheoffice.com/season-list/"
  let seasonOuters = markup^.._DOM .traverse.allAttributed(ix "id" . traverse . only "outer")
  for seasonOuters \el -> do
    let
      numberErr = error "Cannot read season number"
      href = TL.unpack $ el^.allElements.named(only "a").attrOne "href"
      thumbnail = TL.toStrict $ el^.allElements.named(only "img").attrOne "src"
      number = fromMaybe numberErr $ join $ href ^? regex [r|season-([[:digit:]]+)/?$|] . captures . traversed . L.index 0 . L.to (readMaybe @Int)
    (,href) <$> upsert (fixUUID \uuid -> Season{version=given, ..})

scrapeEpisodes :: (Given Connection, Given Version) => Season -> String -> IO [Episode]
scrapeEpisodes season seasonHref = do
  markup <- TL.decodeUtf8 . (^. Wreq.responseBody) <$> httpGet ("https://iwatchtheoffice.com" <> seasonHref)
  let episodeOuters = markup^.._DOM.traverse.allAttributed(ix "id" . traverse . only "outer")
  for episodeOuters \el -> do
    let seasonId  = getField @"uuid" season
    let href      = TL.toStrict $ el^.allElements.named(only "a").attrOne "href"
    let thumbnail = TL.toStrict $ el^.allElements.named(only "img").attrOne "src"
    let code      = TL.toStrict $ el^.allElements.named(only "div").attributed(ix "id" . traverse . only "inner_remaining_l").contents
    let name      = TL.toStrict $ el^.allElements.named(only "div").attributed(ix "id" . traverse . only "inner_remaining_r").contents
    let shortDesc = TL.toStrict $ el^.allElements.named(only "div").attributed(ix "id" . traverse . only "inner_remaining_r2").contents
    markup2 <- TL.decodeUtf8 . (^. Wreq.responseBody) <$> httpGet ("https://iwatchtheoffice.com" <> T.unpack href)
    let links       = fmap TL.toStrict $ markup2^.._DOM.traverse.allAttributed(ix "class" . traverse . only "linkz_box").children.traverse.allNamed(only "a").attrOne "href"
    let description = TL.toStrict $ markup2^._DOM.traverse.allAttributed(ix "class" . traverse . only "description_box").contents
    upsert (fixUUID \uuid -> Episode{version=given, ..})

httpGet :: String -> IO _
httpGet u = Prelude.putStrLn (u <> "...") *> Wreq.get u
