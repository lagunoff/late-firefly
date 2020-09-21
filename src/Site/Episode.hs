{-# OPTIONS_GHC -Wno-orphans #-}
module Site.Episode where

import Control.Monad.Catch
import Data.List as L
import Language.Javascript.JMacro
import Lucid.Base

import "this" DB
import "this" Router
import "this" Site.Types
import "this" Widget
import "this" IMDB.Types

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
      toHtml [jmacro|
        fun hcl el link {
          document.getElementById('video-frame').src=link;
          var ch = el.parentNode.parentNode.children;
          for (var i=0; i < ch.length; i++) {
            ch[i].classList.remove('active');
          };
          el.parentNode.classList.add('active');
        }
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
    case imdbFromText @"tt" epSeries of
      Left _ -> throwE The404Error
      Right (ImdbId i) -> fromImdb i
    where
    fromImdb s = do
      case parseEpCode code of
        Just (sea, epi) -> do
          let ll x = x{links=episodeLinks (ImdbId s) sea epi}
          ds::[EpisodeD] <- query [sql|
            select
              it.original_title_text,
              '[]',
              {sea} as series_season_number,
              ip.plot_text
            from
              imdb_title it
              left join imdb_plot ip on ip.rowid=it.plot_id
              left join title_episode_tsv tet on tet.rowid=it.rowid
            where
              tet.parent_id={s} and
              tet.season_number={sea} and
              tet.episode_number={epi}
            |]
          maybe (throwE The404Error) pure $ fmap (ll . fst) (L.uncons ds)
        Nothing -> throwM The404Error

deriveRowDef ''EpisodeD
