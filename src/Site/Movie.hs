{-# OPTIONS_GHC -Wno-orphans #-}
module Site.Movie where

import Data.List as L
import Data.Text as T
import Lucid.Base
import Language.Javascript.JMacro

import "this" DB
import "this" Router
import "this" Widget
import "this" Site.Types
import "this" IMDB.Types

data MovieD = MovieD
  { title  :: Text
  , links  :: [Text]
  , plot   :: Maybe Text }
  deriving stock (Eq, Show, Generic)

movieWidget :: Route "Title" -> MovieD -> Html ()
movieWidget TitleR{} MovieD{..} = do
  let Theme{..} = theme
  div_ [class_ "episode-root"] do
    toHtml [jmacro|
      fun hcl el link {
        document.getElementById('video-frame').src=link;
        Array.from(el.parentNode.parentNode.children).forEach \x {
          x.classList.remove('active');
        };
        el.parentNode.classList.add('active');
      }
    |]
    iframe_
      [ makeAttribute "referrerpolicy" "no-referrer"
      , makeAttribute "scrolling" "no"
      , makeAttribute "allowfullscreen" "true"
      , makeAttribute "frameborder" "0"
      , makeAttribute "style" "width: 900px; height: 600px"
      , makeAttribute "src" $ links !! 0
      , id_ "video-frame" ] ""
    ul_ [class_ "tabs"] $ for_ (L.zip links [0..]) \(_, idx::Int) -> do
      li_ (bool [] [class_ "active"] $ idx == 0) do
        a_ [href_ "javascript:void 0", onclick_ [st|hcl(this, '#{links !! idx}')|]]
          [ht|Server #{showt (idx + 1)}|]
    h3_ [ht|#{title}|]
    for_ plot (p_ . toHtml)
    [style|
      .episode-root
        max-width: 900px
        margin: 0 auto
        margin-top: #{unit * 3}
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

initMovie :: (?conn::Connection) => Route "Title" -> ServerIO MovieD
initMovie TitleR{..} = do
  Only rowid :. movie <- query1 [sql|
    select
      it.rowid,
      it.original_title_text,
      '[]' as links,
      (select plot_text from imdb_plot ip where ip.title_id=it.rowid and ip.plot_type='"SUMMARY"' limit 1)
    from imdb_title it
    where
      it.url_slug={slug} |]
  pure movie{links=movieLinks (ImdbId rowid)}

deriveRowDef ''MovieD
