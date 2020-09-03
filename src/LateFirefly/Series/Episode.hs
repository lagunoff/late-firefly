{-# OPTIONS_GHC -Wno-orphans #-}
module LateFirefly.Series.Episode where

import Data.List as L
import Control.Lens
import Control.Monad.Catch
import Massaraksh as H
import LateFirefly.DB
import LateFirefly.Router
import LateFirefly.Series.Types
import LateFirefly.Widget.Prelude
import LateFirefly.IMDB.Schema
import LateFirefly.IMDB.GraphQL

data EpisodeR = EpisodeR {code :: Text}
  deriving stock (Eq, Ord, Generic)
  deriving anyclass Flat

data EpisodeData2 = EpisodeData2
  { title  :: Maybe Text
  , links  :: [Text]
  , season :: Int
  , plot   :: Maybe ImdbPlot }
  deriving stock (Show, Eq, Generic)
  deriving anyclass Flat

episodeWidget :: Dynamic EpisodeData2 -> Html ()
episodeWidget d = do
  ed@EpisodeData2{..} <- liftIO (dnRead d)
  let Theme{..} = theme
  (linkIdx, modifyIdx) <- liftIO (newDyn @Int 0)
  divClass "episode-root" do
--    h3_ [ht|Episode #{episodeTxt}|]
--    breadcrumbsWidget (crumbs r ed)
    ulClass "tabs" $ for_ (L.zip links [0..]) \(_, idx) -> do
      li_ do
        toggleClass "active" (fmap (==idx) linkIdx)
        a_ do
          "href" =: "javascript:void 0"
    --       on_ "click" do
    --         liftIO $ sync $ modifyIdx (const idx)
          [ht|Server #{showt (idx + 1)}|]
    iframe_ do
      "referrerpolicy" `attr` "no-referrer"
      "scrolling" `attr` "no"
      "allowfullscreen" `attr` "true"
      "frameborder" `attr` "0"
      "style" =: [st|width: 900px; height: 600px|]
      "src" ~: ((links L.!!) <$> linkIdx)
    for_ plot \ImdbPlot{plot_text=Markdown{..}} -> do p_ (H.text markdown)
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

-- crumbs :: EpisodeRoute -> EpisodeData2 -> [Route]
-- crumbs EpisodeRoute{..} EpisodeData2{..} =
--   [SeriesR_ SeriesRoute{..}, SeasonR_ SeasonRoute{season=coerce season, ..}]

initEpisode :: (?conn::Connection) => EpisodeR -> BackendIO EpisodeData2
initEpisode EpisodeR{..} = do
  let seriesId = "the-office"::Text
  ds::[EpisodeData2] <- query [sql|
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
      where vl.video_id={code} and it.series_title_id=sr.title_id
      group by vl.title_id)
    select vid.video_title, vid.url, vid.series_season_number, ip.*
    from vid
    left join imdb_plot ip where ip.rowid=vid.plot_id
  |]
  case fmap fst (L.uncons ds) of
    Just ep -> pure ep
    Nothing -> case parseEpCode code of
      Just (sea, epi) -> do
        ds::[EpisodeData2] <- query [sql|
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

instance HasParser U EpisodeR where
  parser = dimap code EpisodeR $ segment "episode" /> pSegment

instance IsPage EpisodeR where
  type PageData EpisodeR = EpisodeData2
  page = Page (RemotePtr (static (SomeBackend (Backend initEpisode))) 'initEpisode) episodeWidget

deriveRow ''EpisodeData2 def {fields=[("plot", CstgRow)]}
