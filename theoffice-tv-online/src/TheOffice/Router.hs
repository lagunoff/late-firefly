{-# LANGUAGE OverloadedStrings #-}
module TheOffice.Router where

import Prelude hiding (print)
import Data.Monoid ((<>))
import qualified IWatchTheOffice.Db as Db
import qualified Haste.JSString as JSStr
import Haste.Prim (toJSStr, fromJSStr)
import Haste.JSString (RegEx, match)
import Haste.Prim.Foreign


data Route
  = Home
  | Season String
  | Episode String String
  deriving (Show, Eq)

parse :: String -> Maybe Route
parse "" = Just Home
parse "#" = Just Home
parse ('#':xs) = go
  ( match ("^season-(\\d+)/episode-(\\d+)$" :: RegEx) (toJSStr xs)
  , match ("^season-(\\d+)$" :: RegEx) (toJSStr xs)
  )
  where
    go ([_, season, episode], _) = Just (Episode (fromJSStr season) (fromJSStr episode))
    go (_, [_, season]) = Just (Season (fromJSStr season))
    go _ = Nothing
parse _ = Nothing


print :: Route -> String
print route = "#" <> case route of
  Home -> ""
  (Season season) -> "season-" <> season
  (Episode season episode) -> "season-" <> season <> "/episode-" <> episode

seasonUrl :: Db.Season -> String
seasonUrl (Db.Season {Db.season_code=code}) = print $ Season $ fromJSStr $ JSStr.replace (toJSStr code) ("^Season\\s" :: JSStr.RegEx) ""

episodeUrl :: Db.Season -> Db.Episode -> String
episodeUrl (Db.Season {Db.season_code=season_code}) (Db.Episode{Db.episode_code=episode_code}) =
  print $ Episode (fromJSStr $ JSStr.replace (toJSStr season_code) ("^Season\\s" :: JSStr.RegEx) "") (fromJSStr $ JSStr.replace (toJSStr episode_code) ("^S\\d\\dE\\w" :: JSStr.RegEx) "")

onPopState :: (Route -> IO ()) -> IO (IO ())
onPopState cb = onPopStateImpl $ \str -> do
  putStrLn $ "onPopState: " <> fromJSStr str
  case parse (fromJSStr str) of
    Just route -> cb route
    Nothing -> putStrLn $ "onPopState: route not found: " <> fromJSStr str
  where
    onPopStateImpl :: (JSStr.JSString -> IO ()) -> IO (IO ())
    onPopStateImpl =
      ffi "(function(cb) {\
          \  var prev = window.onpopstate;\
          \  window.onpopstate = function() { cb(location.hash); };\
          \  return function() { window.onpopstate = prev; };\
          \})"
  
