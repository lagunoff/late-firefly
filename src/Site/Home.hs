{-# OPTIONS_GHC -Wno-orphans #-}
module Site.Home where

import Lucid.Base
import Data.List as L

import "this" DB
import "this" Intro
import "this" Router
import "this" Widget

data HomeD = HomeD
  { topRated :: [Title] }

data Title = Title
  { header    :: Text
  , thumbnail :: Text
  , urlSlug   :: Text
  , titleType :: Text }
  deriving stock (Eq, Show, Generic)

deriveRowDef ''Title

homePage = coercePage defPage2 {
  make = \_ -> do
    topRated <- query [sql|
      with a as (
        select
          it.original_title_text as original_title_text,
          replace(ii.url, '._V1_.jpg', '._V1_UX128_.jpg') as url,
          it.url_slug as url_slug,
          tbt.title_type as title_type
        from
          title_ratings_tsv trt
          left join imdb_title it on it.rowid=trt.rowid
          left join imdb_image ii on ii.rowid=it.primary_image_id
          left join title_basics_tsv tbt on tbt.rowid=trt.rowid
        order by trt.num_votes desc
      ),
      b as (
        select * from a where title_type='movie' limit 6
      ),
      c as (
        select * from a where title_type='tvSeries' limit 6
      )
      select * from b union all select * from c
    |]
    pure HomeD{..},
  html = \HomeR{} HomeD{..} ->
    div_ [class_ "home"] do
      div_ [class_ "home-wrapper"] do
        h1_ do "Telikov.Net"
        form_ [action_ "/", method_ "GET"] do
          input_ [name_ "s", type_ "search", makeAttribute "autofocus" "on"]
      let
        (topMovies, topSeries) = L.span ((=="movie") . titleType) topRated
        topList xs =
          ul_ [class_ "top"] do
          for_ xs \Title{..} -> do
            li_ do
              linkTo (TitleR urlSlug) do
                div_ do img_ [src_ thumbnail]
                div_ do toHtml header
      div_ [class_ "top-wrapper"] do
        h3_ "Most popular TV-series"
        topList topSeries
      div_ [class_ "top-wrapper"] do
        h3_ "Most popular movies"
        topList topMovies
  ,
  styles = \_ _ ->
    let topWidth = 128 * 6 + (unit * 2) * 5 in [css|
    .home
      padding: 0 #{unit * 2} 0 #{unit * 2}
      box-sizing: border-box
      max-width: #{pageWidth}
      margin: 0 auto
    .home-wrapper
      margin: 0 auto
      max-width: #{pageWidth}
      text-align: center
      h1
        font-size: 60px
        font-weight: 400
      input[type=search]
        display: block
        width: 100%
        margin: 0 auto
        max-width: #{unit * 70}
        height: #{unit * 6}
        border-radius: 3px
        outline: none
        border: solid 1px #{borderColor}
        padding: 0px 8px
        font-size: 18px
        &:focus
          border: solid 2px #{primary}
    .top-wrapper
      margin: 0 auto
      width: #{topWidth}
    .top
      margin: 0
      padding: 0
      display: flex
      li
        list-style: none
        margin: 0
        padding: 0
        img { height: 174px; width: 128px; object-fit: cover; }
        > a > div { width: 128px; }
        text-align: center
      li + li { margin-left: #{unit * 2}; }
    |]
  } where Theme{..} = theme

instance IsPage "Home" HomeD where page = homePage
