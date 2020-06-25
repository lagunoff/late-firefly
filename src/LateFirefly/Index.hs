{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE StaticPointers #-}
module LateFirefly.Index
  ( Model(..)
  , indexWidget
  ) where

import Control.Lens
import Control.Monad.Trans
import Data.List as L
import Data.Maybe
import Data.Text as T
import Data.UUID.Types as U
import GHC.Records
import Language.Javascript.JSaddle
import LateFirefly.DB
import LateFirefly.Index.SeasonItem
import LateFirefly.Index.Season
import LateFirefly.Index.Episode
import LateFirefly.Prelude
import LateFirefly.Router
import LateFirefly.TheOffice.Schema
import LateFirefly.Widget.Prelude
import LateFirefly.RPC.TH

data Model = Model
  { _idx_route :: Route
  } deriving (Show)

makeLenses ''Model

indexWidget :: Html
indexWidget = mdo
  let Theme{..} = theme
  headerWidget
  route <- hashRouter IndexR \r ->
    liftIO $ sync $ modify (idx_route .~ r)
  (model, modify) <- liftIO $ newDyn (Model route)
  divClass "root" do
    div_ do
      let routeDyn = holdUniqDyn (fmap _idx_route model)
      dynHtml $ routeDyn <&> \case
        IndexR       -> indexPage
        AboutR       -> aboutPage
        SeasonR{..}  -> seasonWidget season
        EpisodeR{..} -> episodeWidget episode
  [style|
    html, body, body *
      font-family: Arial, sans-serif
    body
      margin: 0
    body a
      color: #{primaryText}
      text-decoration: none
      &:hover
        color: #{primary}
  |]

headerWidget :: Html
headerWidget = do
  let Theme{..} = theme
  divClass "header" do
    aClass "home-link" do
      "href" =: "#"
      span_ "Telikov."
      span_ do
        "Net"
        "style" =: [st|color: #{showt primary}|]
    ulClass "menu" do
      li_ do a_ do "Seasons"; "href" =: printRoute IndexR
      li_ do a_ do "About"; "href" =: printRoute AboutR
    -- divClass "search" do
    --   input_ do
    --     "placeholder" =: "Search"
    [style|
      .header
        display: flex
        align-items: center
        background: white
        color: #{primaryText}
        height: #{unit * 5}
        width: 100%
        box-shadow: 0 0 1px rgba(0,0,0,0.2)
        z-index: 2
        position: relative
      .home-link
        font-size: 33px
        color: rgba(0,0,0,0.9)
        &:hover
          background: black
          color: white
        display: block
        height: 100%
        text-decoration: none
        display: flex
        align-items: center
        padding: 0 #{unit * 2} 0 #{unit * 2}
      .menu
        margin: 0 #{unit * 2}
        padding: 0
        display: flex
        align-items: center
        li
          list-style: none
          margin: 0
        li + li
          margin-left: #{unit * 2}
        a
          color: #{primaryText}
          text-decoration: none
          text-transform: uppercase
          font-size: 13px
      .search
        padding: 0 #{unit * 2}
        height: 100%
        display: flex
        align-items: center
        &:hover
          background: rgba(0,0,0,0.05)
        input
          border: none
          outline: none
          font-size: 16px
          background: transparent
          font-weight: 600
        |]

header2Widget :: Html
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
      background: rgba(0,0,0,0.05)
    .header-2 > *
      max-width: #{pageWidth}
      margin: 0 auto
    .poster
      object-fit: contain
      height: 350px
      float: left
      padding-right: #{unit * 3}
    |]

getSeasons :: (?conn :: Connection) => Text -> IO [(Season, [Episode])]
getSeasons txt = do
  seasons <- selectFrom_ @Season [sql|where 1 order by `number`|]
  let seasonIds = T.intercalate ", " $ escText . U.toText . unUUID5 . getField @"uuid" <$> seasons
  episodes <- selectFrom_ @Episode [sql|where season_id in (#{seasonIds}) order by `code`|]
  pure $ seasons <&> \s@Season{uuid} -> (s, L.filter ((==uuid) . getField @"seasonId") episodes)

aboutPage :: Html
aboutPage = do
  div_ do
    h1_ do
      "About Page Works!!!"

indexPage :: Html
indexPage = do
  let Theme{..} = theme
  header2Widget
  ss <- $(remote 'getSeasons) ""
  divClass "seasons" do
    div_ do
      seasonItemWidget ss
  [style|
    .seasons
      margin: 0 #{unit * 3}
    .seasons > *
      max-width: #{pageWidth}
      margin: 0 auto
    |]

hashRouter :: forall a m. (HasParser U a, HtmlBase m) => a -> (a -> HtmlT m ()) -> HtmlT m a
hashRouter def hashChange = do
  win <- Element <$> liftJSM (jsg ("window" :: Text))
  let
    parseRoute = do
      hash <- fromJSValUnchecked =<< jsg ("location" :: Text) ! ("hash" :: Text)
      pure $ fromMaybe def $ listToMaybe $ parseUrl @a (T.drop 1 hash)
  route <- liftJSM parseRoute
  onEvent_ win "popstate" do
    liftJSM parseRoute >>= hashChange
  liftJSM parseRoute
