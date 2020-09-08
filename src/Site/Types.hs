module Site.Types where

import Control.Applicative
import Data.Char
import Data.Text as T
import Text.Read
import TextShow
import "this" IMDB.GraphQL (ImdbId(..))

parseEpCode :: Text -> Maybe (Int, Int)
parseEpCode t = do
  (t1, t2) <- h2 $ Prelude.filter (/=T.empty) $ T.split isAlpha t
  i1 <- readMaybe (T.unpack t1)
  i2 <- readMaybe (T.unpack t2)
  pure (i1, i2)
  where
    h2 (a:b:_) = Just (a,b)
    h2 _       = Nothing

parseEpNumber :: Text -> Maybe Text
parseEpNumber t = try1 <|> Just try2 where
  try1 = fmap (showt . snd) $ parseEpCode t
  try2 = T.drop 1 . snd $ breakOn "e" t

printEpCode :: (Int, Int) -> Text
printEpCode (s, e) = T.pack $ ('s':lpad (show s)) <> ('e':lpad (show e)) where
  lpad x = Prelude.replicate (2 - Prelude.length x) '0' ++ x

movieLinks :: ImdbId "tt" -> [Text]
movieLinks rowid =
  [ "https://vidsrc.me/embed/" <> showt rowid <> "/"
  , "http://videospider.stream/personal?key=tr196i95M6UMTd1i&video_id=" <> showt rowid ]

episodeLinks :: ImdbId "tt" -> Int -> Int -> [Text]
episodeLinks rowid season episode =
  [ "https://vidsrc.me/embed/" <> showt rowid <> "/" <> showt season <> "-" <> showt episode <> "/"
  , "http://videospider.stream/personal?key=tr196i95M6UMTd1i&video_id=" <> showt rowid <> "&tv=1&s=" <> showt season <> "&e=" <> showt episode]
