module LateFirefly.Router
  ( Route(..)
  , printRoute
  , linkTo
  , restoreState
  , module X
  ) where

import LateFirefly.Prelude
import LateFirefly.Parser as X
import Massaraksh.Text as H

import Language.Javascript.JSaddle

data Route
  = AboutR
  | EpisodeR {season :: Seg Int, episode :: Seg Text}
  | SeasonR {season :: Seg Int}
  | IndexR_
  deriving (Show, Eq, Generic, HasParser U)

printRoute :: Route -> Text
printRoute = ("/" <>) . printUrl

linkTo :: (HtmlBase m, HasParser U r) => r -> HtmlT m x -> HtmlT m x
linkTo r attrs = do
  H.a_ do
    "href" =: ("/" <> printUrl r)
    H.on "mousedown" $ H.dId <&> \(pToJSVal -> ev) -> do
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
  void $ liftJSM $ eval ("setTimeout(function() {document.scrollingElement.scrollTop = history.state ? history.state.scrollTop : 0;}, 100)":: Text)
