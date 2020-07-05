module LateFirefly.Router
  ( Route(..)
  , printRoute
  , linkTo
  , restoreState
  , breadcrumbs
  , routeTitle
  , breadcrumbsWidget
  , EpisodeRoute(..)
  , SeriesRoute(..)
  , SeasonRoute(..)
  , module X
  ) where

import LateFirefly.Prelude
import LateFirefly.Parser as X
import LateFirefly.Widget as X
import Data.String as S
import Massaraksh.Text as H
import Control.Monad.Reader
import Data.IORef
import Language.Javascript.JSaddle
import Text.Shakespeare.Text

data Route
  = EpisodeR_ EpisodeRoute
  | SeasonR_ SeasonRoute
  | SeriesR_ SeriesRoute
  | HomeR_
  deriving stock (Show, Eq, Generic)
  deriving anyclass (HasParser U)

data EpisodeRoute = EpisodeRoute
  { series  :: Seg Text
  , season  :: Seg Int
  , episode :: Seg Text }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (HasParser U)

data SeasonRoute = SeasonRoute
  { series :: Seg Text
  , season :: Seg Int }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (HasParser U)

data SeriesRoute = SeriesRoute {series :: Seg Text}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (HasParser U)

printRoute :: Route -> Text
printRoute = ("/" <>) . printUrl

linkTo :: (HtmlBase m, HasParser U r) => r -> HtmlT m x -> HtmlT m x
linkTo r attrs = do
  H.a_ do
    "href" =: ("/" <> printUrl r)
    H.on "click" $ H.dId <&> \(pToJSVal -> ev) -> do
      liftJSM $ ev # ("preventDefault" :: Text) $ ()
      liftJSM $ eval ("history.replaceState({ scrollTop: document.scrollingElement.scrollTop }, '')":: Text)
      liftJSM $ (jsg ("history"::Text)) # ("pushState"::JSString) $ (jsUndefined, (""::JSString), "/" <> printUrl r)
      popStateEv <- liftJSM $ new (jsg ("Event"::Text)) $ ("popstate"::Text)
      win <- liftJSM $ jsg ("window" :: Text)
      liftJSM $ win # ("dispatchEvent" :: Text) $ popStateEv
      blank
    attrs

restoreState :: Html
restoreState = do
  pb <- fmap he_post_build ask
  liftIO $ modifyIORef pb $ (:) do
    void $ liftJSM $ eval ("document.scrollingElement.scrollTop = history.state ? history.state.scrollTop : 0;":: Text)

instance S.IsString s => S.IsString (Seg s) where
  fromString = Seg . S.fromString

breadcrumbs :: Route -> [Route]
breadcrumbs = \case
  EpisodeR_ EpisodeRoute{..} -> [SeriesR_ SeriesRoute{..}, SeasonR_ SeasonRoute{..}]
  SeasonR_ SeasonRoute{..} -> [SeriesR_ SeriesRoute{..}]
  _ -> []

routeTitle :: Route -> Text
routeTitle = \case
  EpisodeR_ EpisodeRoute{..} -> let episode'::Text = coerce episode in [st|Episode #{episode'}|]
  SeasonR_ SeasonRoute{..} -> let season'::Int = coerce season in  [st|Season #{showt season'}|]
  SeriesR_ SeriesRoute{..} -> let series'::Text = coerce series in [st|Series #{series'}|]
  HomeR_ -> [st|Home|]

breadcrumbsWidget :: Route -> Html
breadcrumbsWidget route = do
  ulClass "breadcrumbs" do
  for_ (breadcrumbs route) \r ->
    li_ do linkTo r do H.text (routeTitle r)
  [style|
    .breadcrumbs
      padding: 0
      margin: 0
      & > li
        list-style: none
        display: inline-block
      & > li + li:before
          display: inline
          content: "→"

  |]
