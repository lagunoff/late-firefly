{-# OPTIONS_GHC -Wno-orphans #-}
module Site.Search where

import Lucid.Base

import "this" Intro
import "this" Router
import "this" DB
import "this" Widget
import "this" IMDB.Types

data SearchD = SearchD
  { results :: [Result] }

data Result = Result
  { rowid     :: Int
  , header    :: Text
  , thumbnail :: Maybe Text
  , plot      :: Maybe Text
  , year      :: Maybe Text
  , series    :: Bool
  }
  deriving stock (Eq, Show, Generic)

instance IsPage "SearchR" SearchD where
  pageWidget SearchR{..} SearchD{..} = do
    let Theme{..} = theme
    div_ [class_ "search"] do
      form_ [action_ "/", method_ "GET"] do
        input_ [name_ "s", type_ "search", makeAttribute "autofocus" "on", value_ search]
        button_ [type_ "submit"] do "Search"
      ul_ do
        for_ results \Result{..} -> do
          let
            link = if series
              then linkTo $ SeriesR (showt $ ImdbId @"tt" $ fromIntegral rowid)
              else linkTo $ MovieR (showt rowid)
          li_ [class_ "search-item"] do
            link do
              for_ thumbnail \src -> img_ [src_ src]
            div_ do
              link do
                 h5_ do
                   toHtml header
                   for_ year \y -> do " "; toHtml y
              for_ plot (p_ . toHtml)
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
    |]
  pageInit SearchR{..} = do
    results <- query [sql|
      select
        s.rowid,
        s.header,
        s.thumbnail67x98,
        s.text,
        s.`year`,
        case
          when (tbt.title_type = "tvSeries")
          then 1
          else 0
        end
      from
        imdb_title_fts fts
        cross join imdb_title it on fts.rowid=it.rowid
        left join imdb_search s on fts.rowid=s.rowid
        left join title_basics_tsv tbt on fts.rowid=tbt.rowid
      where
        fts.header match {search}
    |]
    pure SearchD{..}

deriveRowDef ''Result
