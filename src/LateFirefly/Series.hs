module LateFirefly.Series
  ( seriesWidget
  ) where

import Control.Lens hiding ((#))
import Data.List as L
import Data.Text as T
import Data.UUID.Types as U
import GHC.Records
import LateFirefly.Router
import LateFirefly.DB
import LateFirefly.Series.SeasonItem
import LateFirefly.RPC.TH
import LateFirefly.TheOffice.Schema
import LateFirefly.Widget.Prelude

apiGetSeasons :: (?conn::Connection) => Text -> Eio BackendError [(Season, [Episode])]
apiGetSeasons txt = liftIO do
  seasons <- selectFrom_ @Season [sql|where 1 order by `number`|]
  let seasonIds = T.intercalate ", " $ escText . U.toText . unUUID5 . getField @"uuid" <$> seasons
  episodes <- selectFrom_ @Episode [sql|where season_id in (#{seasonIds}) order by `code`|]
  pure $ seasons <&> \s@Season{uuid} -> (s, L.filter ((==uuid) . getField @"seasonId") episodes)

seriesWidget :: SeriesRoute -> Html (Html ())
seriesWidget r@SeriesRoute{..} = do
  ss <- $(remote 'apiGetSeasons) (coerce series)
  pure do
    let Theme{..} = theme
    header2Widget
    divClass "seasons" do
      button_ do
        "emit error"
        on_ "click" do throwError (FlatError "")
      div_ do
        seasonItemWidget r ss
    [style|
      .seasons
        margin: 0 #{unit * 3}
      .seasons > *
        max-width: #{pageWidth}
        margin: 0 auto
      |]

header2Widget :: Html ()
header2Widget = do
  let Theme{..} = theme
  divClass  "header-2" do
    div_ do
      img_ do
        "className" =: "poster"
        "src" =: "https://m.media-amazon.com/images/M/MV5BMDNkOTE4NDQtMTNmYi00MWE0LWE4ZTktYTc0NzhhNWIzNzJiXkEyXkFqcGdeQXVyMzQ2MDI5NjU@._V1_SY999_CR0,0,665,999_AL_.jpg"
      p_ [ht|The Office is an American mockumentary sitcom television series that depicts the everyday lives of office employees in the Scranton, Pennsylvania, branch of the fictional Dunder Mifflin Paper Company. It aired on NBC from March 24, 2005, to May 16, 2013, lasting a total of nine seasons.[1] It is an adaptation of the 2001-2003 BBC series of the same name, being adapted for American television by Greg Daniels, a veteran writer for Saturday Night Live, King of the Hill, and The Simpsons. It was co-produced by Daniels's Deedle-Dee Productions, and Reveille Productions (later Shine America), in association with Universal Television. The original executive producers were Daniels, Howard Klein, Ben Silverman, Ricky Gervais, and Stephen Merchant, with numerous others being promoted in later seasons.|]
      div_ ("style" =: "clear: both")
  [style|
    .header-2
      width: 100%
      box-sizing: border-box
      padding: #{unit * 3}
      p
        margin-top: 0
      & > *
        max-width: #{pageWidth}
        margin: 0 auto
      .poster
        object-fit: contain
        height: 350px
        float: left
        padding-right: #{unit * 3}
    |]
