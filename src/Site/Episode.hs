{-# OPTIONS_GHC -Wno-orphans #-}
module Site.Episode where

import Data.List as L
import Language.Javascript.JMacro
import Lucid.Base

import "this" DB
import "this" Router
import "this" Site.Types
import "this" Site.Movie
import "this" Widget
import "this" IMDB.Schema
import "this" IMDB.Types

data EpisodeD = EpisodeD
  { title  :: Maybe Text
  , links  :: [Text]
  , season :: Int
  , plot   :: Maybe Text }
  deriving stock (Show, Eq, Generic)

episodePage = defPage {pInit,pLayout} where
  Page{pLayout=movieLayout} = moviePage
  pLayout :: Route "Episode" -> EpisodeD -> _
  pLayout EpisodeR{..} EpisodeD{..} = movieLayout (TitleR series) MovieD{title=title ?: "",..}

  pInit :: (?conn::Connection) => Route "Episode" -> ServerIO _
  pInit EpisodeR{..} = do
    case parseEpCode code of
      Just (sea, epi) -> do
        ds::[Only (Id ImdbTitle) :. EpisodeD] <- query [sql|
          select
            it2.rowid,
            it.original_title_text,
            '[]',
            {sea} as series_season_number,
            (select plot_text from imdb_plot ip where ip.title_id=it.rowid and ip.plot_type='"SUMMARY"' limit 1)
          from
            imdb_title it
            left join title_episode_tsv tet on tet.rowid=it.rowid
            left join imdb_title it2 on tet.parent_id=it2.rowid
          where
            it2.url_slug={series} and
            tet.season_number={sea} and
            tet.episode_number={epi}
          |]
        case fmap fst (L.uncons ds) of
          Just (Only (Id i) :. e) -> pure e{Site.Episode.links=episodeLinks (ImdbId i) sea epi}
          Nothing                 -> throwE The404Error
      Nothing -> throwE The404Error

instance IsPage "Episode" EpisodeD where page = episodePage

deriveRowDef ''EpisodeD
