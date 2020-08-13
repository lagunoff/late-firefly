module LateFirefly.Router
  ( Route(..)
  , IsPage(..)
  , printRoute
  , linkTo
  , mayLinkTo
  , restoreState
  , routeTitle
  -- , breadcrumbs
  , breadcrumbsWidget
  , EpisodeRoute(..)
  , SeriesRoute(..)
  , SeasonRoute(..)
  , module X
  , pageParser
  , PageDict(..)
  , Page(..)
  ) where

import LateFirefly.Prelude
import LateFirefly.Parser as X
import LateFirefly.Widget as X
import Data.String as S
import Data.Constraint
import Massaraksh as H
import Control.Monad.Reader
import Data.IORef
import GHC.StaticPtr
import Language.Javascript.JSaddle
import Text.Shakespeare.Text
import qualified Data.Dynamic as D
import Control.Lens (dimap)
import qualified Data.Map as M
import GHC.Fingerprint
import Type.Reflection (SomeTypeRep(..))

class (HasParser U r, Typeable r, Typeable (PageData r)) => IsPage r where
  type PageData r
  page :: Page r (PageData r)

data Page r d = Page
  { pgInit   :: RemotePtr r d
  , pgWidget :: Dynamic d -> Html () }

data SomePage = forall r d. SomePage (Page r d)
data PageDict = forall a. PageDict (Dict (IsPage a))

frontendRouter :: [PageDict] -> Html () -> Html ()
frontendRouter p p404 = do
  undefined

pageParser :: [PageDict] -> Parser UrlChunks D.Dynamic (D.Dynamic, PageDict)
pageParser p = Parser (par p) pri where
  mm = fing p
  par :: [PageDict] -> UrlChunks -> [((D.Dynamic, PageDict), UrlChunks)]
  par [] s = []
  par (x:xs) s = (fmap (\(a, b) -> ((a, x), b)) (parse (pd x) s)) <> par xs s
  pri :: D.Dynamic -> _ -> _
  pri d x = let z = D.dynTypeRep d in
    maybe x (\y -> X.print (pd y) d x) $ M.lookup (typeRepFingerprint z) mm
  pd :: PageDict -> UrlParser D.Dynamic
  pd (PageDict d@Dict) = dPar (pp d) where
    pp :: forall a. Dict (IsPage a) -> UrlParser a
    pp Dict = parser @_ @a
  fing :: [PageDict] -> M.Map Fingerprint PageDict
  fing = M.fromList . fmap \x@(PageDict d) -> (pp d, x) where
    pp :: forall a. Dict (IsPage a) -> Fingerprint
    pp Dict = typeRepFingerprint (typeRep (Proxy::Proxy a))
  dPar :: Typeable a => Parser' s a -> Parser' s D.Dynamic
  dPar = dimap g f where
    f = D.toDyn
    g = fromMaybe errorTrace . D.fromDynamic

instance IsPage SeasonRoute where
  type PageData SeasonRoute = (SeasonRoute, [Int])

data Route
  = SeasonR_ SeasonRoute
  | EpisodeR_ EpisodeRoute
  | SeriesR_ SeriesRoute
  | HomeR_
  deriving stock (Show, Eq, Generic)
  deriving anyclass (HasParser U)

data EpisodeRoute = EpisodeRoute
  { series  :: Seg Text
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

linkTo :: (HasParser U r) => r -> Html x -> Html x
linkTo = mayLinkTo . Just

mayLinkTo :: (HasParser U r) => Maybe r -> Html x -> Html x
mayLinkTo mayR attrs = do
  H.a_ do
    "href" =: maybe "javascript:void(0)" (("/" <>) . printUrl) mayR
    for_ mayR \r -> do
      H.on "click" $ H.decodeJSVal <&> \(pToJSVal -> ev) -> do
        liftJSM $ ev # ("preventDefault" :: Text) $ ()
        liftJSM $ eval ("history.replaceState({ scrollTop: document.scrollingElement.scrollTop }, '')":: Text)
        liftJSM $ (jsg ("history"::Text)) # ("pushState"::JSString) $ (jsUndefined, (""::JSString), "/" <> printUrl r)
        popStateEv <- liftJSM $ new (jsg ("Event"::Text)) $ ("popstate"::Text)
        win <- liftJSM $ jsg ("window"::Text)
        liftJSM $ win # ("dispatchEvent"::Text) $ popStateEv
        blank
    attrs

restoreState :: Html ()
restoreState = do
  pb <- fmap htnvPostBuild ask
  liftIO $ modifyIORef pb $ (:) do
    void $ liftJSM $ eval ("document.scrollingElement.scrollTop = history.state ? history.state.scrollTop : 0;":: Text)

instance S.IsString s => S.IsString (Seg s) where
  fromString = Seg . S.fromString

routeTitle :: Route -> Text
routeTitle = \case
  EpisodeR_ EpisodeRoute{..} -> let episode'::Text = coerce episode in [st|Episode #{episode'}|]
  SeasonR_ SeasonRoute{..} -> let season'::Int = coerce season in [st|Season #{showt season'}|]
  SeriesR_ SeriesRoute{..} -> let series'::Text = coerce series in [st|Series #{series'}|]
  HomeR_ -> [st|Home|]

breadcrumbsWidget :: [Route] -> Html ()
breadcrumbsWidget route = do
  ulClass "breadcrumbs" do
  for_ route \r ->
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
