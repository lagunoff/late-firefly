module Schema where

import "this" DB
import "this" Intro
import "this" IMDB.Schema

data VideoLink = VideoLink
  { rowid      :: Id VideoLink
  , titleId    :: Id ImdbTitle
  , videoId    :: Maybe Text
  , videoTitle :: Maybe Text
  , url        :: Text
  , origin     :: Maybe Text }
  deriving stock (Show, Eq, Generic)

deriveDb ''VideoLink def {renameField=underscore, ukeys=[["url"]]}

data Series = Series
  { rowid    :: Tid Series
  , title_id :: Id ImdbTitle }
  deriving stock (Show, Eq, Generic)

deriveDb ''Series def
