module LateFirefly.Series.Types where

import Control.Applicative
import Data.Char
import Data.Text as T
import Text.Read
import TextShow

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
