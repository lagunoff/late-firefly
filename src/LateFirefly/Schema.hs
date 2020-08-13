module LateFirefly.Schema where

import Flat
import LateFirefly.DB
import LateFirefly.Prelude
import LateFirefly.IMDB.Schema

data VideoLink = VideoLink
  { rowid      :: Id VideoLink
  , titleId    :: Id ImdbTitle
  , videoId    :: Maybe Text
  , videoTitle :: Maybe Text
  , url        :: Text
  , origin     :: Maybe Text }
  deriving stock (Show, Eq, Generic)
  deriving anyclass Flat

deriveDb ''VideoLink def {renameField=underscore, ukeys=[["url"]]}

data Series = Series
  { rowid    :: Tid Series
  , title_id :: Id ImdbTitle }
  deriving stock (Show, Eq, Generic)

deriveDb ''Series def
