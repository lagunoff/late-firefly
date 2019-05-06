{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
module IWatchTheOffice.Db where 
  
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

data Episode = Episode
  { episode_code              :: !String
  , episode_name              :: !String
  , episode_href              :: !String
  , episode_short_description :: !String
  , episode_thumbnail         :: !String
  , episode_description       :: !String
  , episode_links             :: ![String]
  } deriving (Show, Generic, NFData)
    
data Season = Season
  { season_code      :: !String
  , season_thumbnail :: !String
  , season_href      :: !String
  , season_episodes  :: ![Episode]
  } deriving (Show, Generic, NFData)
    
data Db = Db
  { db_seasons :: [Season]
  } deriving (Show, Generic, NFData)
