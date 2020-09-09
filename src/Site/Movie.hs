{-# OPTIONS_GHC -Wno-orphans #-}
module Site.Movie where

import Data.List as L
import Lucid.Base

import "this" DB
import "this" Router
import "this" Widget
import "this" Site.Types
import "this" IMDB.Types

data MovieD = MovieD
  { title  :: Text
  , links  :: [Text]
  , plot   :: Maybe Text }
  deriving stock (Show, Eq, Generic)

instance IsPage "MovieR" MovieD where
  pageWidget MovieR{} MovieD{..} = do
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

  pageInit MovieR{..} = do
    Only rowid :. movie <- query1 [sql|
      select
        s.rowid,
        s.header,
        '[]' as links,
        s.text
      from
        imdb_search as s
        left join imdb_title t on t.rowid=s.rowid
      where
        s.rowid={mvCode} |]
    pure movie{links=movieLinks (ImdbId rowid)}

deriveRowDef ''MovieD
