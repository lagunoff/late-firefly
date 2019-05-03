module TheOffice.Router where

import Data.Monoid ((<>))


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
print (Episode season episode) = "season-" <> season <> "/episode" <> episode
