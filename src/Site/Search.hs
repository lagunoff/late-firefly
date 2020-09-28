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

paginate :: Int -> Int -> Int -> [Int]
paginate offset limit total = catMaybes pages where
  pages =
    [ first, back 2, back 1, Just offset, forward 1, forward 2, forward 3
    , forward 4, last ]
  back x = let y = offset - (limit * x) in if y <= 0 then Nothing else Just y
  forward x = let y = offset + (limit * x) in if y >= limit * q then Nothing else Just y
  first = if offset == 0 then Nothing else Just 0
  last = if lastOff == offset then Nothing else Just lastOff
  (q,r) = divMod total limit
  lastOff|total == 0 = 0|r==0 = limit * (q - 1)|otherwise = limit * q

pageOf :: Int -> Int
pageOf = succ . (`div` limit)

instance IsPage "Search" SearchD where
  pageWidget SearchR{..} SearchD{..} = do
    let
      Theme{..} = theme
      pages =
        when (total > limit) $ ul_ [class_ "pagination"] do
          let ps = paginate offset limit total
          for_ (L.zip ps (Nothing:fmap Just ps)) \(o, prev) -> do
            when (fmap (< (o - limit)) prev == Just True) do
              li_ [class_ "dots"] "…"
            if offset==o then li_ [class_ "active"] do (toHtml (showt (pageOf o)))
            else li_ do linkTo SearchR{offset=o,..} do (toHtml (showt (pageOf o)))
    div_ [class_ "search-results"] do
      case total of
        0 -> do
          div_ [class_ "not-found"] do
            span_ do "Nothing found"
        _ -> do
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
    let next = SearchR{offset=offset+limit,..}
    let prev = SearchR{offset=offset-limit,..}
    toHtml [jmacro|
      window.addEventListener 'keydown' \e {
        if (e.ctrlKey && e.key == 'ArrowLeft' && `(offset > 0)`) {
          window.location.href = `(urlTo prev)`;
        }
        if (e.ctrlKey && e.key == 'ArrowRight' && `(offset + limit <= total)`) {
          window.location.href = `(urlTo next)`;
        }
      }
    |]
    [style|
      .search-results
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
