{-# OPTIONS_GHC -Wno-orphans #-}
module Series.Episode where

import Data.List as L
import Control.Monad.Catch
import Lucid.Base

import "this" DB
import "this" Router
import "this" Series.Types
import "this" Widget

data EpisodeD = EpisodeD
  { title  :: Maybe Text
  , links  :: [Text]
  , season :: Int
  , plot   :: Maybe Text }
  deriving stock (Show, Eq, Generic)

instance IsPage "EpisodeR" EpisodeD where
  pageWidget EpisodeR{..} EpisodeD{..} = do
    let Theme{..} = theme
    div_ [class_ "episode-root"] do
      h3_ [ht|Episode #{code}|]
      -- breadcrumbsWidget (crumbs r ed)
      fun_ "hcl" ["el", "link"] [st|
        document.getElementById('video-frame').src=link;
        Array.from(el.parentNode.parentNode.children).forEach(function(x) {
          x.classList.remove('active');
        });
        el.parentNode.classList.add('active');
      |]
      ul_ [class_ "tabs"] $ for_ (L.zip links [0..]) \(_, idx::Int) -> do
        li_ (bool [] [class_ "active"] $ idx == 0) do
          a_ [href_ "javascript:void 0", onclick_ [st|hcl(this, '#{links !! idx}')|]]
            [ht|Server #{showt (idx + 1)}|]
      iframe_
        [ makeAttribute "referrerpolicy" "no-referrer"
        , makeAttribute "scrolling" "no"
        , makeAttribute "allowfullscreen" "true"
        , makeAttribute "frameborder" "0"
        , makeAttribute "style" "width: 900px; height: 600px"
        , makeAttribute "src" $ links !! 0
        , id_ "video-frame" ] ""
      for_ plot \plot_text -> do p_ (toHtml plot_text)
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

  pageInit EpisodeR{..} = do
    let seriesId = "the-office"::Text
    ds::[EpisodeD] <- query [sql|
      with vid as
        (select
          min(vl.video_title)          as video_title,
          json_group_array(vl.url)     as url,
          min(it.plot_id)              as plot_id,
          min(it.series_season_number) as series_season_number,
          vl.title_id                  as title_id
        from video_link vl
          cross join series sr on sr.rowid={seriesId}
          left join imdb_title it on it.rowid=vl.title_id
        where vl.video_id={code}
          and it.series_title_id=sr.title_id
        group by vl.title_id)
      select vid.video_title, vid.url, vid.series_season_number, ip.plot_text
      from vid
      left join imdb_plot ip where ip.rowid=vid.plot_id
    |]
    case fmap fst (L.uncons ds) of
      Just ep -> pure ep
      Nothing -> case parseEpCode code of
        Just (sea, epi) -> do
          ds::[EpisodeD] <- query [sql|
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
            select vid.video_title, vid.url, {sea} as series_season_number, ip.plot_text
            from vid
            left join imdb_plot ip where ip.rowid=vid.plot_id
            |]
          maybe (throwM The404Error) pure $ fmap fst (L.uncons ds)
        Nothing -> throwM The404Error

deriveRowDef ''EpisodeD
