{-# OPTIONS_GHC -Wno-orphans #-}
module Site.Home where

import Lucid.Base

import "this" DB
import "this" Intro
import "this" Router
import "this" Widget

data HomeD = HomeD
  { topRated :: [Title] }

data Title = Title
  { header    :: Text
  , thumbnail :: Text
  , urlSlug   :: Text }
  deriving stock (Eq, Show, Generic)

instance IsPage "Home" HomeD where
  pageInit _ = do
    topRated <- query [sql|
      select
        it.original_title_text,
        replace(ii.url, '._V1_.jpg', '._V1_UX128_AL_.jpg'),
        it.url_slug
      from
        imdb_title it
        left join imdb_image ii on ii.rowid=it.primary_image_id
        left join title_ratings_tsv trt on it.rowid=trt.rowid
      order by trt.num_votes desc
      limit 6
    |]
    pure HomeD{..}
  pageWidget HomeR{} HomeD{..} = do
    let Theme{..} = theme
    div_ [class_ "home"] do
      div_ [class_ "home-wrapper"] do
        h1_ do "Telikov.Net"
        form_ [action_ "/", method_ "GET"] do
          input_ [name_ "s", type_ "search", makeAttribute "autofocus" "on"]
      ul_ [class_ "top"] do
        for_ topRated \Title{..} -> do
          linkTo (TitleR urlSlug) do img_ [src_ thumbnail]
          linkTo (TitleR urlSlug) do
            li_ do toHtml header
    [style|
      .home
        padding: 0 #{unit * 2} 0 #{unit * 2}
        box-sizing: border-box
      .home-wrapper
        margin: 0 auto
        max-width: #{pageWidth}
        text-align: center
        padding: 160px 0 160px 0
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
    |]

deriveRowDef ''Title
