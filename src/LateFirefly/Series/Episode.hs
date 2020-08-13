{-# OPTIONS_GHC -fno-warn-orphans #-}
module LateFirefly.Series.Episode where

import Data.Generics.Product
import Data.Constraint
import Data.List as L
import Control.Monad.Catch
import LateFirefly.DB
import LateFirefly.RPC.TH
import LateFirefly.Router
import LateFirefly.Series.Rules
import LateFirefly.Series.Types
import LateFirefly.Widget.Prelude
import LateFirefly.IMDB.Schema
import LateFirefly.IMDB.GraphQL
import Text.Regex.Quote
import Text.Regex.TDFA

episodeWidget :: EpisodeRoute -> Html (Html ())
episodeWidget r@EpisodeRoute{..} = do
  let episodeTxt::Text = coerce episode
  let seriesTxt::Text = coerce series
  ed@EpisodeData{..} <- $(remote 'getEpisode) (seriesTxt, episodeTxt)
  pure do
    let Theme{..} = theme
    (holdUniqDyn -> linkIdx, modifyIdx) <- liftIO (newDyn 0)
    divClass "episode-root" do
      h3_ [ht|Episode #{episodeTxt}|]
      breadcrumbsWidget (crumbs r ed)
      ulClass "tabs" $ for_ (L.zip links [0..]) \(_, idx) -> do
        li_ do
          toggleClass "active" (fmap (==idx) linkIdx)
          a_ do
            "href" =: "javascript:void 0"
            on_ "click" do
              liftIO $ sync $ modifyIdx (const idx)
            [ht|Server #{showt (idx + 1)}|]
      iframe_ do
        "referrerpolicy" `attr` "no-referrer"
        "scrolling" `attr` "no"
        "allowfullscreen" `attr` "true"
        "frameborder" `attr` "0"
        "style" =: [st|width: 900px; height: 600px|]
        "src" ~: ((links L.!!) <$> linkIdx)
      -- p_ (text description)
    [style|
      .episode-root
        max-width: 900px
        margin: 0 auto
        .tabs
          display: flex
          margin: 0
          padding: 0
          border-bottom: solid 2px #{primary}
          margin-bottom: #{unit}
          & > li
            list-style: none
            padding: #{unit} #{unit * 2}
        li.active
          background: #{primary}
          a
            color: white
        li a
          text-decoration: none
          color: #{primaryText}
    |]

crumbs :: EpisodeRoute -> EpisodeData -> [Route]
crumbs EpisodeRoute{..} EpisodeData{..} =
  [SeriesR_ SeriesRoute{..}, SeasonR_ SeasonRoute{season=coerce season, ..}]

data EpisodeData = EpisodeData
  { title  :: Maybe Text
  , links  :: [Text]
  , season :: Int
  , plot   :: Maybe ImdbPlot }
  deriving stock (Show, Eq, Generic)
  deriving anyclass Flat

deriving anyclass instance Flat ImdbPlot
deriving anyclass instance Flat Markdown
deriving anyclass instance Flat PlotType
deriving anyclass instance Flat DisplayableLanguage

getEpisode :: (?conn::Connection) => (Text, Text) -> Eio BackendError EpisodeData
getEpisode (seriesId, epCode) = liftIO do
  ds::[EpisodeData] <- query [sql|
    with vid as
      (select
        min(vl.video_title) as video_title,
        json_group_array(vl.url) as url,
        min(it.plot_id) as plot_id,
        min(it.series_season_number) as series_season_number,
        vl.title_id as title_id
      from video_link vl
      cross join series sr on sr.rowid={seriesId}
      left join imdb_title it on it.rowid=vl.title_id
      where vl.video_id={epCode} and it.series_title_id=sr.title_id
      group by vl.title_id)
    select vid.video_title, vid.url, vid.series_season_number, ip.*
    from vid
    left join imdb_plot ip where ip.rowid=vid.plot_id
  |]
  case fmap fst (L.uncons ds) of
    Just ep -> pure ep
    Nothing -> case parseEpCode epCode of
      Just (sea, epi) -> do
        ds::[EpisodeData] <- query [sql|
          with vid as
            (select
              min(vl.video_title) as video_title,
              json_group_array(vl.url) as url,
              min(it.plot_id) as plot_id,
              vl.title_id as title_id
            from video_link vl
            cross join series sr on sr.rowid={seriesId}
            left join imdb_title it on it.rowid=vl.title_id
            where it.series_title_id=sr.title_id and it.series_season_number={sea} and it.series_episode_number={epi}
            group by vl.title_id)
          select vid.video_title, vid.url, {sea} as series_season_number, ip.*
          from vid
          left join imdb_plot ip where ip.rowid=vid.plot_id
          |]
        maybe (throwM The404Error) pure $ fmap fst (L.uncons ds)
      Nothing -> throwM The404Error

deriveDb ''EpisodeData def {fields=[("plot", CstgRow)]}
