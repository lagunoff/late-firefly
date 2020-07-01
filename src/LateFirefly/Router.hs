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
import Control.Monad.Reader
import Data.IORef
import Language.Javascript.JSaddle

data Route
  = EpisodeR {season :: Seg Int, episode :: Seg Text}
  | SeasonR {season :: Seg Int}
  | SeriesR
  | HomeR_
  deriving (Show, Eq, Generic, HasParser U)

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
