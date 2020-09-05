{-# OPTIONS_GHC -Wno-orphans #-}
module Series where

import Data.Text as T

import "this" Intro
import "this" Router
import "this" DB hiding ((:=))
import "this" Widget

data SeriesD = SeriesD {episodes :: [Episode]}
  deriving stock (Eq, Show, Generic)

data Episode = Episode
  { title     :: Maybe Text
  , code      :: Maybe Text
  , thumbnail :: Maybe Text
  , plot      :: Maybe Text
  , season    :: Maybe Int
  , episode   :: Maybe Int }
  deriving stock (Show, Eq, Generic)

instance IsPage "SeriesR" SeriesD where
  pageWidget SeriesR{..} SeriesD{..} = do
    let Theme{..} = theme
    header2Widget
    div_ [class_ "seasons"] do
      button_ "emit error"
      for_ episodes \Episode{..} -> do
        h2_ $ toHtml $ ((title <|> (("Episode " <>) . showt <$> episode)) ?: "")
        img_ [src_ (thumbnail ?: "https://teddytennis.com/usa/wp-content/uploads/sites/88/2017/11/placeholder.png")]
        p_ $ toHtml $ plot ?: ""
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
    |]
    where
      header2Widget = do
        div_ [class_ "header-2"] do
          div_ do
            img_ [class_ "poster", src_ "https://m.media-amazon.com/images/M/MV5BMDNkOTE4NDQtMTNmYi00MWE0LWE4ZTktYTc0NzhhNWIzNzJiXkEyXkFqcGdeQXVyMzQ2MDI5NjU@._V1_SY999_CR0,0,665,999_AL_.jpg"]
            p_ $ "The Office is an American mockumentary sitcom television series that depicts the everyday lives of office employees in the Scranton, Pennsylvania, branch of the fictional Dunder Mifflin Paper Company. It aired on NBC from March 24, 2005, to May 16, 2013, lasting a total of nine seasons.[1] It is an adaptation of the 2001-2003 BBC series of the same name, being adapted for American television by Greg Daniels, a veteran writer for Saturday Night Live, King of the Hill, and The Simpsons. It was co-produced by Daniels's Deedle-Dee Productions, and Reveille Productions (later Shine America), in association with Universal Television. The original executive producers were Daniels, Howard Klein, Ben Silverman, Ricky Gervais, and Stephen Merchant, with numerous others being promoted in later seasons."
            div_ [style_ "clear: both"] do ""

  pageInit (SeriesR s) = do
    xs::[Episode] <- query [sql|
      with c as (
        select title_id as title_id, min(video_id) as video_id
          from video_link group by title_id
      )
      select
        json_extract(it.original_title_text, '$.text'),
        c.video_id, ii.url, ip.plot_text,
        it.series_season_number, it.series_episode_number
      from imdb_title it
        left join imdb_plot ip on it.plot_id=ip.rowid
        left join imdb_image ii on it.primary_image_id=ii.rowid
        left join c on c.title_id=it.rowid
        where it.series_title_id in (
          select s.title_id from series s where s.rowid={s}
        )
        order by it.series_season_number, it.series_episode_number
    |]
    pure $ SeriesD xs

deriveRowDef ''Episode
