module LateFirefly.Index
  -- ( IndexState(..)
  -- , indexWidget
  -- )
where

import Control.Lens hiding ((#))
import Control.Monad.Trans
import Control.Monad.Reader
import Data.Maybe
import Data.Constraint
import Data.Text as T
import Language.Javascript.JSaddle
-- import LateFirefly.Series.Episode
-- import LateFirefly.Series.Season
import LateFirefly.Template
import LateFirefly.Prelude
import LateFirefly.Router
import LateFirefly.Widget.Prelude
import LateFirefly.Disqus
-- import LateFirefly.Series
import LateFirefly.Icons

data IndexState = IndexState
  { _idx_route :: Route
  } deriving (Show)

makeLenses ''IndexState

indexWidget :: [PageDict] -> Html ()
indexWidget = frontendRouter

data HomeR = HomeR
  deriving stock (Eq, Ord, Generic)
  deriving anyclass Flat

instance HasParser U HomeR where
  parser = dimap (const ()) (const HomeR) $ segments []

instance IsPage HomeR where
  type PageData HomeR = ()
  page = Page (RemotePtr (static (SomeBackend (Backend initHome))) 'initHome) (const homeWidget)

initHome :: HomeR -> BackendIO ()
initHome _ = pure ()

homeWidget :: Html ()
homeWidget =
  htmlTemplate do
    let Theme{..} = theme
    divClass "home" do
      divClass "home-wrapper" do
        h1_ do "Telikov.Net"
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

aboutWidget :: Html ()
aboutWidget =
  div_ do
    ul_ do
      li_ "One"
      li_ "Two"
      li_ "Three"
      li_ do
        button_ "B1"
        button_ "B2"
