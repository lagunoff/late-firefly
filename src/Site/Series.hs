module Site.Series where

import Data.List as L
import Data.Text as T
import Lucid.Base
import Language.Javascript.JMacro
import qualified Data.Map as M

import "this" DB
import "this" Router
import "this" Icons
import "this" Widget
import "this" Site.Types

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

initSeries :: (?conn::Connection) => Route "Title" -> ServerIO SeriesD
initSeries TitleR{..} = do
  eps::[Episode] <- query [sql|
    select
      it.original_title_text,
      null,
      replace(ii.url, '._V1_.jpg', '._V1_UX225_AL_.jpg'),
      tet.season_number,
      tet.episode_number
    from title_episode_tsv tet
      left join imdb_title it on tet.rowid=it.rowid
      left join imdb_image ii on it.primary_image_id=ii.rowid
    where
      tet.parent_id=(select rowid from imdb_title where url_slug={slug}) and
      tet.episode_number is not null
  |]
  (plot, title, thumbnail) <- query1 [sql|
    select
      (select plot_text from imdb_plot ip where ip.title_id=it.rowid and ip.plot_type='"SUMMARY"' limit 1),
      it.original_title_text,
      replace(ii.url, '._V1_.jpg', '._V1_UX256_AL_.jpg')
    from imdb_title it
      left join imdb_image ii on it.primary_image_id=ii.rowid
      where it.url_slug={slug}
  |]
  let seasons = fmap (L.sortOn episode) $ M.fromListWith (<>) $ fmap (\v@Episode{..} -> (season, [v{Site.Series.code=liftA2 (curry printEpCode) season episode}])) eps
  pure SeriesD{..}

seriesWidget :: Route "Title" -> SeriesD -> Html ()
seriesWidget r d@SeriesD{..} = do
    let Theme{..} = theme
    header2Widget
    div_ [class_ "seasons"] do
      seasonSlider r d
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

seasonSlider :: Route "Title" -> SeriesD -> Html ()
seasonSlider TitleR{..} SeriesD{..} = do
  let
    Theme{..} = theme
    thumbnailHeight = thumbnailWidth * 2 / 3
    chevronWidth = unit * 5
    gap = unit
  flip M.traverseWithKey seasons \season episodes -> do
    div_ [class_ "season"] do
      h3_ [class_ "season-header"] do
        toHtml [st|Season #{showt season}|]
      div_ [class_ "wrapper"] do
        ul_ [class_ "episodes-list"] do
          for_ episodes \Episode{..} -> do
            li_ do
              linkAttMay (EpisodeR slug <$> code) [class_ "link", draggable_ "false"] do
                for_ thumbnail \src -> img_ [draggable_ "false", src_ src, style_ [st|width: #{showt thumbnailWidth}; height: #{showt thumbnailHeight}|] ]
                div_ do
                  let tit = T.intercalate " " $ catMaybes [(<>" — ") . ("Episode "<>) . showt <$> episode, title]
                  h4_ (toHtml tit)
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
  toHtml [jmacro|
    fun listen el n h {
      fun hh e { h e }
      el.addEventListener n hh;
      return \{ el.removeEventListener n hh };
    }

    fun listen1 el n h {
      fun hh e { h e; el.removeEventListener n hh; }
      el.addEventListener n hh;
    }

    fun forEach arr cb {
      for (var i = 0; i < arr.length; i++) {
        cb arr[i] i;
      }
    }

    fun hscrl el dir {
      var ulEl = el.parentNode.children[0];
      ulEl.scrollLeft += ulEl.clientWidth * dir;
    }

    liEls = document.querySelectorAll '.episodes-list';

    var down0 = null;
    var scroll0 = null;
    var moved = 0;

    forEach liEls \el {
      listen el 'mousedown' \e {
        down0 = e.x;
        scroll0 = el.scrollLeft;
        moved = 0;
        el.style.scrollBehavior = 'unset';
        listen1 window 'mouseup' \e {
          if (moved > 2) {
            e.preventDefault();
            e.stopPropagation();
          }
          down0 = null;
          el.style.scrollBehavior = 'smooth';
        };
      };

      listen el 'mousemove' \e {
        if (down0 != null) {
          el.scrollLeft = scroll0 + (-(e.x - down0));
          moved++;
        }
      };

      listen el 'click' \e {
        if ((e.target.tagName == 'A' || e.target.parentElement.tagName == 'A') && moved > 2) {
          e.preventDefault();
          e.stopPropagation();
          moved = 0;
        }
      };
    };
  |]
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
        position: relative;
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
