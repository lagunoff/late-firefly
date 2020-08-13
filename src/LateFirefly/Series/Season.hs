module LateFirefly.Series.Season where

import Data.List as L
import LateFirefly.DB
import LateFirefly.IMDB.Schema
import LateFirefly.RPC.TH
import LateFirefly.Router
import LateFirefly.Series.Types
import LateFirefly.Widget.Prelude as H

seasonWidget :: SeasonRoute -> Html (Html ())
seasonWidget r@SeasonRoute{..} = do
  episodes <- $(remote 'getEpisodes) (coerce series, coerce season)
  pure do
    let Theme{..} = theme
    let thumbnailHeight = thumbnailWidth * 2 / 3
    divClass "season" do
      breadcrumbsWidget [SeriesR_ SeriesRoute{..}]
      ul_ $ for_ (L.zip [0..] episodes) \(ix::Int, EpisodeData{..}) -> do
        let epiNum = (parseEpNumber =<< videoId) <|> fmap showt episodeNumber ?: showt ix
        let epcode = videoId <|> printEpCode . (coerce season,) <$> episodeNumber
        let epLink = mayLinkTo (epcode <&> \x -> EpisodeR_ EpisodeRoute{episode=coerce x, ..})
        li_ do
          epLink do
            h3_ do [ht|Episode #{epiNum} — #{title}|]
          divClass "row" do
            epLink $ for_ image \src ->
              img_ do
                "src" =: src
                "style" =: [st|width: #{showt thumbnailWidth}; height: #{showt thumbnailHeight}|]
            for_ plot \p ->
              div_ do
                p_ (H.text p)

    [style|
      .season
        max-width: #{pageWidth}
        margin: 0 auto
        img
          border-radius: #{thumbnailBorderRadius}
        h3
          text-transform: uppercase
          font-weight: 400
          font-size: 16px
          margin-bottom: 8px
          margin-top: 37px
        ul li
          list-style: none
        ul
          margin: 0 #{unit * 3}
          padding: 0
        ul .row
          display: flex
        ul .row > * + *
          margin-left: #{unit * 2}
        p
          margin: 0
    |]

data EpisodeData = EpisodeData
  { title :: Text
  , videoId :: Maybe Text
  , episodeNumber :: Maybe Int
  , image :: Maybe Text
  , plot  :: Maybe Text }
  deriving stock (Show, Eq, Generic)
  deriving anyclass Flat

getEpisodes :: (?conn::Connection) => (Text, Int) -> Eio BackendError [EpisodeData]
getEpisodes (series, season) = liftIO do
  query [sql|
    with
      vi as (
        select min(v.video_id) as video_id, v.title_id as title_id
        from video_link v
        group by v.title_id
      ),
      im as (
        select min(i.url) as url, tim.imdb_title_id as title_id
        from imdb_image i
        left join imdb_title_to_image tim on tim.imdb_image_id=i.rowid
        where json_extract(i.source, '$.id')='amazon' and i.width > i.height
        group by tim.imdb_title_id
      )
    select
      json_extract(ti.title_text, '$.text'),
      vi.video_id, ti.series_episode_number,
      coalesce(im.url, pim.url),
      json_extract(pl.plot_text, '$.markdown')
    from series se
      left join imdb_title ti on ti.series_title_id=se.title_id
      left join imdb_plot  pl on pl.rowid=ti.plot_id
      left join imdb_image pim on pim.rowid=ti.primary_image_id
      left join vi            on vi.title_id=ti.rowid
      left join im            on im.title_id=ti.rowid
    where se.rowid={series} and ti.series_season_number={season}
    group by ti.rowid
    order by ti.series_episode_number
  |]

deriveDb ''EpisodeData def
