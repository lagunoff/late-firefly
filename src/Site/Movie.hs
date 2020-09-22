{-# OPTIONS_GHC -Wno-orphans #-}
module Site.Movie where

import Data.List as L
import Lucid.Base
import qualified Data.Map as M

import "this" DB
import "this" Router
import "this" Icons
import "this" Widget
import "this" Site.Types
import "this" IMDB.Types

data MovieD = MovieD
  { title  :: Text
  , links  :: [Text]
  , plot   :: Maybe Text }
  deriving stock (Eq, Show, Generic)

data SeriesD = SeriesD
  { seasons   :: M.Map (Maybe Int) [Episode]
  , thumbnail :: Maybe Text
  , title     :: Text
  , plot      :: Maybe Text }
  deriving stock (Eq, Show, Generic)

data Episode = Episode
  { title     :: Maybe Text
  , code      :: Maybe Text
  , thumbnail :: Maybe Text
  , season    :: Maybe Int
  , episode   :: Maybe Int }
  deriving stock (Eq, Show, Generic)

instance IsPage "MovieR" (Either MovieD SeriesD) where
  pageWidget MovieR{} (Left MovieD{..}) = do
    let Theme{..} = theme
    div_ [class_ "episode-root"] do
      h3_ [ht|#{title}|]
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
      for_ plot (p_ . toHtml)
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
  pageWidget MovieR{..} (Right SeriesD{..}) = do
    let Theme{..} = theme
    header2Widget
    div_ [class_ "seasons"] do
      seasonSlider MovieR{..} SeriesD{..}
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
            h1_ (toHtml title)
            for_ plot (p_ . toHtml)
            div_ [style_ "clear: both"] do ""

  pageInit MovieR{..} = do
    Only (isTv::Bool) <- query1 [sql|
      select tbt.title_type='tvSeries'
        from title_basics_tsv tbt
          left join imdb_title it on it.rowid=tbt.rowid
        where
          it.url_slug={slug}
        |]
    bool (Left <$> movie slug) (Right <$> series slug) isTv
    where
      movie s = do
        Only rowid :. movie <- query1 [sql|
          select
            it.rowid,
            it.original_title_text,
            '[]' as links,
            ip.plot_text
          from imdb_title it
            left join imdb_plot ip on ip.rowid=it.plot_id
          where
            it.url_slug={s} |]
        pure movie{links=movieLinks (ImdbId rowid)}
      series s = do
        eps::[Episode] <- query [sql|
          select
            it.original_title_text,
            null, ii.url,
            tet.season_number,
            tet.episode_number
          from title_episode_tsv tet
            left join imdb_title it on tet.rowid=it.rowid
            left join imdb_image ii on it.primary_image_id=ii.rowid
          where
            tet.parent_id=(select rowid from imdb_title where url_slug={s}) and
            tet.episode_number is not null
        |]
        (plot, title, thumbnail) <- query1 [sql|
          select
            ip.plot_text,
            it.original_title_text,
            ii.url
          from imdb_title it
            left join imdb_plot ip on it.plot_id=ip.rowid
            left join imdb_image ii on it.primary_image_id=ii.rowid
            where it.url_slug={s}
        |]
        let seasons = M.fromListWith (<>) $ fmap (\v@Episode{..} -> (season, [v{Site.Movie.code=liftA2 (curry printEpCode) season episode}])) eps
        pure SeriesD{..}

seasonSlider :: Route "MovieR" -> SeriesD -> Html ()
seasonSlider MovieR{..} SeriesD{..} = do
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
              linkAttMay (EpisodeR slug <$> code) [class_ "link"] do
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

deriveRowDef ''MovieD
deriveRowDef ''Episode
