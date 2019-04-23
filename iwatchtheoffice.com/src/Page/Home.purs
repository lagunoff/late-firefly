-- | The Conduit homepage allows users to explore articles in several ways: in a personalized feed,
-- | by tag, or by viewing all articles.
module Page.Home where

import Prelude

import Capability.Navigate (class Navigate)
import Component.HTML.Utils (css)
import Control.Monad.Reader (class MonadAsk)
import Data.Lens (Traversal')
import Data.Lens.Index (ix)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.Monoid (guard)
import Data.Route (Route(..))
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Ref (Ref)
import Halogen (AttrName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (ButtonType(..))
import Halogen.HTML.Properties as HP
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

type State =
  { page :: Int
  }

data Tab
  = Feed
  | Global
  | Tag String

derive instance eqTab :: Eq Tab

tabIsTag :: Tab -> Boolean
tabIsTag (Tag _) = true
tabIsTag _ = false

data Query a
  = Initialize a

component
  :: forall m
   . MonadAff m
  => Navigate m
  => H.Component HH.HTML Query Unit Void m
component =
  H.lifecycleComponent
    { initialState
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just $ H.action Initialize
    , finalizer: Nothing
    }
  where 
  initialState :: Unit -> State
  initialState _ =
    { page: 1
    }

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    Initialize a -> do
      pure a

  render :: State -> H.ComponentHTML Query
  render state@{ page } =
    HH.div
    [ css "root" ]
    [ header
--    , HH.div [ HP.id_ "disqus_thread" ] []
    ]
    where
      header =
        HH.header_
        [ HH.nav
          [ css "navbar navbar-expand-lg navbar-light bg-light" ]
          [ HH.button
            [ css "navbar-toggler"
            , HP.type_ ButtonButton
            , HP.attr (AttrName "data-toggle") "collapse"
            , HP.attr (AttrName "data-target") "#navbarTogglerDemo01"
            , HP.attr (AttrName "aria-controls") "navbarTogglerDemo01"
            , HP.attr (AttrName "aria-expanded") "false"
            , HP.attr (AttrName "aria-label") "Toggle navigation"
            ] [ HH.span [ css "navbar-toggler-icon" ] [] ]
          , HH.div
            [ css "collapse navbar-collapse", HP.id_ "navbarTogglerDemo01" ]
            [ HH.ul
              [ css "navbar-nav mr-auto mt-2 mt-lg-0" ]
              [ HH.li [ css "nav-item" ] [ HH.a [ css "nav-link", HP.href "" ] [ HH.text "Home"  ] ]
              , HH.li [ css "nav-item" ] [ HH.a [ css "nav-link", HP.href "" ] [ HH.text "Seasons" ] ]
              ]
            ]
          ]
        , HH.img [ HP.src "OFF-09OFF-09-image0.jpg" ]
        ]
