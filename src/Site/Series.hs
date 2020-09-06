{-# OPTIONS_GHC -Wno-orphans #-}
module Site.Series () where

import Data.Text as T
import qualified Data.Map as M

import "this" Intro
import "this" Icons
import "this" Router
import "this" DB hiding ((:=))
import "this" Widget

data SeriesD = SeriesD
  { seasons   :: M.Map Int [Episode]
  , thumbnail :: Maybe Text
  , plot      :: Maybe Text }
  deriving stock (Eq, Show, Generic)

data Episode = Episode
  { title     :: Maybe Text
  , code      :: Maybe Text
  , thumbnail :: Maybe Text
  , season    :: Maybe Int
  , episode   :: Maybe Int }
  deriving stock (Show, Eq, Generic)

instance IsPage "SeriesR" SeriesD where
  pageWidget SeriesR{..} SeriesD{..} = do
    let Theme{..} = theme
    header2Widget
    div_ [class_ "seasons"] do
      seasonSlider SeriesR{..} SeriesD{..}
    [style|
      .header-2
        width: 100%
        box-sizing: border-box
        padding: #{showt $ unit * 3}
        p
          margin-top: 0
        & > *
          max-width: #{showt $ pageWidth}
          margin: 0 auto
        .poster
          object-fit: contain
          height: 350px
          float: left
          padding-right: #{showt $ unit * 3}
      .seasons
        & > *
          max-width: #{showt $ pageWidth}
          margin: 0 auto
    |]
    where
      header2Widget = do
        div_ [class_ "header-2"] do
          div_ do
            for_ thumbnail \src ->
              img_ [class_ "poster", src_ src]
            for_ plot (p_ . toHtml)
            div_ [style_ "clear: both"] do ""

  pageInit (SeriesR s) = do
    eps::[Only Int :. Episode] <- query [sql|
      with c as (
        select title_id as title_id, min(video_id) as video_id
          from video_link group by title_id
      )
      select
        it.series_season_number,
        json_extract(it.original_title_text, '$.text'),
        c.video_id, ii.url,
        it.series_season_number, it.series_episode_number
      from imdb_title it
        left join imdb_image ii on it.primary_image_id=ii.rowid
        left join c on c.title_id=it.rowid
        where it.series_title_id in (
          select s.title_id from series s where s.rowid={s}
        )
    |]
    (plot, thumbnail) <- query1 [sql|
      select
        ip.plot_text,
        ii.url
      from imdb_title it
        left join imdb_plot ip on it.plot_id=ip.rowid
        left join imdb_image ii on it.primary_image_id=ii.rowid
        where it.rowid=(
          select s.title_id from series s where s.rowid={s}
        )
    |]
    let seasons = M.fromListWith (<>) $ fmap (\(Only k :. v) -> (k, [v])) eps
    pure SeriesD{..}

seasonSlider :: Route "SeriesR" -> SeriesD -> Html ()
seasonSlider SeriesR{..} SeriesD{..} = do
  let
    Theme{..} = theme
    thumbnailHeight = thumbnailWidth * 2 / 3
    chevronWidth = unit * 5
    gap = unit
  fun_ "hscrl" ["el", "dir"] [st|
    var ulEl = el.parentNode.children[0];
    var inc = dir > 0 ? + ulEl.clientWidth : -ulEl.clientWidth
    ulEl.scrollLeft += inc;
  |]
  flip M.traverseWithKey seasons \season episodes -> do
    div_ [class_ "season"] do
      h3_ [class_ "season-header"] do
        toHtml [st|Season #{showt season}|]
      div_ [class_ "wrapper"] do
        ul_ [class_ "episodes-list"] do
          for_ episodes \Episode{..} -> do
            li_ do
              linkAttMay (EpisodeR series <$> code) [class_ "link"] do
                for_ thumbnail \src -> img_ [src_ src, style_ [st|width: #{showt thumbnailWidth}; height: #{showt thumbnailHeight}|] ]
                div_ do
                  h4_ [ht|Episode #{fromMaybe "" code} — #{fromMaybe "" title}|]
        button_
          [ class_ "chevron chevron-left"
          , tabindex_ "-1"
          , onclick_ "hscrl(this, -1)"
          ] do
          chevronLeft_
        button_
          [ class_ "chevron chevron-right"
          , tabindex_ "-1"
          , onclick_ "hscrl(this, 1)"
          ] do
          chevronRight_
  [style|
    .season
      .link
        text-decoration: none
        color: #{primaryText}
        text-decoration: none
        h4
          margin: 0
          font-size: 14px
          font-weight: 400
          color: #{primaryText}
          &:hover
            color: #{primary}
      h3
        text-transform: uppercase
        font-weight: 400
        font-size: 16px
        margin-bottom: 8px
        margin-top: #{unit * 3}
      .season-header-link
        text-decoration: none
      .episodes-list::-webkit-scrollbar
        display: none
      .episodes-list
        width: 100%
        overflow-x: scroll
        -ms-overflow-style: none
        scrollbar-width: none
        display: flex
        padding: 0
        scroll-behavior: smooth
        li
          list-style: none
          img
            border-radius: #{thumbnailBorderRadius}
            object-fit: cover
        li + li
          margin-left: #{gap}
      .chevron
        outline: none
        position: absolute
        top: #{(thumbnailHeight / 2) - (chevronWidth / 2)}
        z-index: 10
        cursor: pointer
        border: none
        padding: 0
        display: flex
        align-items: center
        justify-content: center
        width: #{chevronWidth}
        height: #{chevronWidth}
        border-radius: 50%
        background: white
        box-shadow: 0 4px 6px 0 rgba(0,0,0,0.05), 0 1px 0 1px rgba(0,0,0,0.05), 0 0 0 1px rgba(0,0,0,0.05)
        &:hover
          box-shadow: 0 10px 20px rgba(0,0,0,0.19), 0 6px 6px rgba(0,0,0,0.23);
          opacity: 1
        &[disabled]
          opacity: 0.65
        &[disabled]:hover
          box-shadow: 0 4px 6px 0 rgba(0,0,0,0.05), 0 1px 0 1px rgba(0,0,0,0.05), 0 0 0 1px rgba(0,0,0,0.05)
      .chevron svg
        display: block
      .chevron-left
        left: -#{chevronWidth *  0.5}
      .chevron-right
        right: -#{chevronWidth *  0.5}
      .placeholder > *
        width: 300px
        height: 200px
        border-radius: #{thumbnailBorderRadius}
        background: rgba(155, 147, 127, 0.06)
      .wrapper
        position: relative
  |]

deriveRowDef ''Episode
