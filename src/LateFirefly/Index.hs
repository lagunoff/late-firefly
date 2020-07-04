module LateFirefly.Index
  ( IndexState(..)
  , indexWidget
  ) where

import Control.Lens hiding ((#))
import Control.Monad.Trans
import Data.Maybe
import Data.Text as T
import Language.Javascript.JSaddle
import LateFirefly.Series.Episode
import LateFirefly.Series.Season
import LateFirefly.Prelude
import LateFirefly.Router
import LateFirefly.Widget.Prelude
import LateFirefly.Disqus
import LateFirefly.Series
import LateFirefly.Icons

data IndexState = IndexState
  { _idx_route :: Route
  } deriving (Show)

makeLenses ''IndexState

indexWidget :: Html
indexWidget = mdo
  let Theme{..} = theme
  headerWidget
  setupDisqus
  route <- htmlRouter SeriesR \r ->
    liftIO $ sync $ modify (idx_route .~ r)
  (model, modify) <- liftIO $ newDyn (IndexState route)
  divClass "root" do
    div_ do
      let routeDyn = holdUniqDyn (fmap _idx_route model)
      dynHtml $ routeDyn <&> \case
        HomeR_       -> homeWidget >>= (<* restoreState)
        SeriesR      -> seriesWidget >>= (<* restoreState)
        SeasonR{..}  -> seasonWidget (coerce season) >>= (<* restoreState)
        EpisodeR{..} -> episodeWidget (coerce episode) >>= (<* restoreState)
    embedDisqus "home" "Telikov.Net — Home"

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

homeWidget :: HtmlM Html
homeWidget = do
  let Theme{..} = theme
  pure do
    divClass "home" do
      divClass "home-wrapper" do
        h1_ do
          "Telikov.Net"
        div_ do
          input_ do
            "type" =: "search"
    [style|
      .home
        padding: 0 #{unit * 2} 0 #{unit * 2}
        box-sizing: border-box
      .home-wrapper
        margin: 0 auto
        max-width: #{pageWidth}
        text-align: center
        h1
          margin-top: 160px
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

headerWidget :: Html
headerWidget = do
  let Theme{..} = theme
  divClass "header" do
    divClass "header-wrapper" do
      divClass "header-left" do
        linkTo HomeR_ do
          "className" =:"home-link"
          span_ "Telikov."
          span_ do
            "Net"
            "style" =: [st|color: #{showt primary}|]
        ulClass "menu" do
          li_ do linkTo SeriesR do div_ "Series"
          li_ do linkTo SeriesR do div_ "Movies"
          li_ do linkTo SeriesR do div_ "Genre"
          li_ do linkTo SeriesR do div_ "Top IMDB"
          li_ do linkTo SeriesR do div_ "A-Z List"
      divClass "search" do
        input_ do
          "placeholder" =: "Search"
        searchIcon ("className" =: "search")
    [style|
      body
        background: rgba(0,0,0,0.04)
      .header
        background: white
        height: #{unit * 7}
        width: 100%
        box-shadow: 0 0 16px rgba(0,0,0,0.1)
        z-index: 2
        position: relative
        padding: 0 #{unit * 3} 0 #{unit * 3}
        box-sizing: border-box
        .header-wrapper
          height: 100%
          display: flex
          align-items: center
          justify-content: space-between
          margin: 0 auto
          max-width: #{pageWidth}
        .header-left
          height: 100%
          display: flex
          align-items: center
      .home-link
        font-size: 33px
        color: rgba(0,0,0,0.9)
        display: block
        height: 100%
        text-decoration: none
        display: flex
        align-items: center
        padding: 0 #{unit} 0 #{unit}
        margin-left: -#{unit}
        &:hover
          background: black
          color: white
      .menu
        display: flex
        align-items: center
        margin: 0 0 0 #{unit * 2}
        padding: 0
        height: 100%
        li
          list-style: none
          margin: 0
          height: 100%
        a
          padding: 0 #{unit}
          display: flex
          align-items: center
          color: #{primaryText}
          text-decoration: none
          text-transform: uppercase
          font-size: 13px
          height: 100%
      .search
        padding: 0 #{unit * 2}
        height: 100%
        display: flex
        align-items: center
        height: #{(unit * 4) - 2}
        box-sizing: border-box
        border-radius: #{unit * 2}
        border: solid 2px transparent
        box-shadow: 0 0 2px rgba(0,0,0,0.4)
        input
          padding-left: #{unit}
          border: none
          outline: none
          font-size: 16px
          background: transparent
          &:focus
            width: 400px
        svg
          opacity: 0.2
        &:hover
          border: solid 2px #{primary}
          box-shadow: none
          svg
            opacity: 1
        |]

htmlRouter :: forall a m. (HasParser U a, HtmlBase m, Show a) => a -> (a -> HtmlT m ()) -> HtmlT m a
htmlRouter def hashChange = do
  win <- Element <$> liftJSM (jsg ("window" :: Text))
  let
    parseRoute = do
      search <- fromJSValUnchecked =<< jsg ("location" :: Text) ! ("search" :: Text)
      pathname <- fromJSValUnchecked =<< jsg ("location" :: Text) ! ("pathname" :: Text)
      pure $ fromMaybe def $ listToMaybe $ parseUrl @a (pathname <> search)
  route <- liftJSM parseRoute
  onEvent_ win "popstate" do
    liftJSM parseRoute >>= hashChange
  liftJSM parseRoute
