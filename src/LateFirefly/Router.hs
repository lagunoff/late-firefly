module LateFirefly.Router
  ( Route(..)
  , printRoute
  , module X
  ) where

import LateFirefly.Prelude
import LateFirefly.Parser as X

data Route
  = AboutR
  | EpisodeR {season :: Int, episode :: Text}
  | SeasonR {season :: Int}
  | IndexR
  deriving (Show, Eq, Generic, HasParser U)

printRoute :: Route -> Text
printRoute = ("#" <>) . printUrl
