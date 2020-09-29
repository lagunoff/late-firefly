{-# OPTIONS_GHC -Wno-orphans #-}
module Site.Title where

import "this" DB
import "this" Intro
import "this" Router
import "this" Site.Movie
import "this" Site.Series

titlePage = defPage {pInit,pLayout} where
  Page{pInit=initMovie,pLayout=layoutMovie} = moviePage
  Page{pInit=initSeries,pLayout=layoutSeries} = seriesPage
  pInit :: (?conn::Connection) => Route "Title" -> ServerIO _
  pInit r@TitleR{..} = do
    Only (isTv::Bool) <- query1 [sql|
      select tbt.title_type='tvSeries'
        from title_basics_tsv tbt
          left join imdb_title it on it.rowid=tbt.rowid
        where
          it.url_slug={slug}
        |]
    bool (Left <$> initMovie r) (Right <$> initSeries r) isTv
  pLayout r = either (layoutMovie r) (layoutSeries r)

instance IsPage "Title" (Either MovieD SeriesD) where page = titlePage
