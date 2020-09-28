{-# OPTIONS_GHC -Wno-orphans #-}
module Site.Search where

import Lucid.Base
import Data.List as L

import "this" Intro
import "this" Router
import "this" DB
import "this" Widget

data SearchD = SearchD
  { results :: [Result]
  , total   :: Int }

data Result = Result
  { rowid     :: Int
  , urlSlug   :: Text
  , header    :: Text
  , thumbnail :: Maybe Text
  , plot      :: Maybe Text
  , year      :: Maybe Text
  , series    :: Bool
  }
  deriving stock (Eq, Show, Generic)

limit::Int = 50

paginate :: Int -> Int -> Int -> [(Maybe Int, Int, Int)]
paginate offset limit total = prevs $ fmap (\x -> (x, p x)) $ catMaybes pages where
  pages =
    [ first, left 2, left 1, Just offset, right 1, right 2, right 3
    , right 4, last ]
  p = succ . (`div` limit) --
  left x = let y = offset - (limit * x) in if y <= 0 then Nothing else Just y
  right x = let y = offset + (limit * x) in if y >= limit * q then Nothing else Just y
  first = if offset == 0 then Nothing else Just 0
  last = if lastOff == offset then Nothing else Just lastOff
  (q,r) = divMod total limit
  lastOff|total == 0 = 0|r==0 = limit * (q - 1)|otherwise = limit * q
  prevs = go Nothing where
    go _ []     = []
    go y ((a, b):xs) = (y, a, b) : go (Just b) xs

instance IsPage "Search" SearchD where
  pageWidget SearchR{..} SearchD{..} = do
    let
      Theme{..} = theme
      pages =
        when (total > limit) $ ul_ [class_ "pagination"] do
          for_ (paginate offset limit total) \(prev, o, p) -> do
            when (fmap (< (p-1)) prev == Just True) do
              li_ [class_ "dots"] "…"
            if offset==o then li_ [class_ "active"] do (toHtml (showt p))
            else li_ do linkTo SearchR{offset=o,..} do (toHtml (showt p))
    div_ [class_ "search"] do
      form_ [action_ "/", method_ "GET"] do
        input_ [name_ "s", type_ "search", makeAttribute "autofocus" "on", value_ search]
        button_ [type_ "submit"] do "Search"
      div_ [class_ "total"] do
        span_ do toHtml (showt total); " results found"
      pages
      ul_ do
        for_ results \Result{..} -> do
          let
            link = linkTo $ TitleR urlSlug
          li_ [class_ "search-item"] do
            link do
              for_ thumbnail \src -> img_ [src_ src, width_ "64"]
            div_ do
              link do
                 h5_ do
                   toHtml header
                   for_ year \y -> do " "; toHtml y
              for_ plot (p_ . toHtml)
      pages

    [style|
      .search
        max-width: #{pageWidth}
        margin: #{unit * 3} auto 0 auto
        & ul
          margin: 0
          padding: 0
          & li
            list-style: none
      .search-item
        display: flex
        align-items: top
        margin-top: #{unit * 2}
        h5
          margin: 0 0 #{unit} 0
          font-size: 16px
          text-transform: uppercase
          font-weight: 400
        p
          margin: 0
        * + *
          margin-left: #{unit * 2}
      .pagination
        margin: 0
        padding: 0
        display: flex
        justify-content: center
        user-select: none
        li
          border: 1px solid rgba(0,0,0,0.1)
          list-style: none
          margin: 0
          padding: 0
          min-width: #{unit * 4}
          height: #{unit * 4}
          text-align: center
          line-height: #{unit * 4}
          background: white
          &.active
            background: #{primary}
            color: rgba(255,255,255,0.87)
            cursor: default
          a
            width: 100%
            height: 100%
            display: block
        li + li
          border-left: none
        li:first-child
          border-top-left-radius: 5px
          border-bottom-left-radius: 5px
        li:last-child
          border-top-right-radius: 5px
          border-bottom-right-radius: 5px
    |]
  pageInit SearchR{..} = do
    results0 <- query [sql|
      with a as (
        select
          s.rowid as rowid,
          it.url_slug as url_slug,
          s.header as header,
          replace(ii.url, '._V1_.jpg', '._V1_UX67_AL_.jpg') as url,
          s.text as text,
          s.`year` as year,
          case
            when (tbt.title_type = "tvSeries")
            then 1
            else 0
          end as isSeries,
          trt.num_votes as num_votes
        from
          imdb_title_fts fts
          cross join imdb_title it on fts.rowid=it.rowid
          left join imdb_image ii on ii.rowid=it.primary_image_id
          left join imdb_search s on fts.rowid=s.rowid
          left join title_basics_tsv tbt on fts.rowid=tbt.rowid
          left join title_ratings_tsv trt on fts.rowid=trt.rowid
        where
          fts.header match {search}
      ),
      b as (
        select rowid,url_slug,header,url,text,year,isSeries from a
          order by num_votes desc
          limit {limit} offset {offset}
      )
      select count(*),"","",null,null,null,0 from a union all select * from b
    |]
    let results = L.drop 1 results0
    let total::Int = coerce (Site.Search.rowid (results0 !! 0))
    pure SearchD{..}

deriveRowDef ''Result
