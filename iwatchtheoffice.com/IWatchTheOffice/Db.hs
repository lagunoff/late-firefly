{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module IWatchTheOffice.Db where

import Data.Aeson   (FromJSON, ToJSON)
import Data.Text    (Text)
import GHC.Generics (Generic)

data Episode = Episode
  { _episodeCode             :: Text
  , _episodeName             :: Text
  , _episodeHref             :: Text
  , _episodeShortDescription :: Text
  , _episodeThumbnail        :: Text
  , _episodeDescription      :: Text
  , _episodeLinks            :: [Text]
  } deriving (Show, Generic, ToJSON, FromJSON)

data Season = Season
  { _seasonCode      :: Text
  , _seasonThumbnail :: Text
  , _seasonHref      :: Text
  , _seasonEpisodes  :: [Episode]
  } deriving (Show, Generic, ToJSON, FromJSON)
