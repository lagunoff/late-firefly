{-# LANGUAGE OverloadedStrings #-}
module TheOffice.Router where

import Prelude hiding (print)
import Data.Monoid ((<>))
import qualified IWatchTheOffice.Db as Db
import qualified Haste.JSString as JSStr
import Haste.Prim (toJSStr, fromJSStr)


data Route
  = Home
  | Season String
  | Episode String String


parse :: String -> Maybe Route
parse "" = Just Home
parse _ = Nothing

print :: Route -> String
print Home = ""
print (Season season) = "season-" <> season
print (Episode season episode) = "season-" <> season <> "/episode-" <> episode

seasonUrl :: Db.Season -> String
seasonUrl (Db.Season {Db.season_code=code}) = print $ Season $ fromJSStr $ JSStr.replace (toJSStr code) ("^Season\\s" :: JSStr.RegEx) ""

episodeUrl :: Db.Season -> Db.Episode -> String
episodeUrl (Db.Season {Db.season_code=season_code}) (Db.Episode{Db.episode_code=episode_code}) =
  print $ Episode (fromJSStr $ JSStr.replace (toJSStr season_code) ("^Season\\s" :: JSStr.RegEx) "") (fromJSStr $ JSStr.replace (toJSStr episode_code) ("^S\\d\\dE\\w" :: JSStr.RegEx) "")
