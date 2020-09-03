{-# OPTIONS_GHC -fno-warn-orphans #-}
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
  , SomePage(..)
  , frontendRouter
  ) where

import Control.Lens (dimap)
import Control.Monad.Reader
import Data.Constraint
import Data.IORef
import Flat
import Data.String as S
import GHC.Fingerprint
import Language.Javascript.JSaddle
import LateFirefly.Parser as X
import LateFirefly.Prelude
import {-# SOURCE #-} LateFirefly.Template
import LateFirefly.Widget as X
import LateFirefly.RPC.TH
import Massaraksh as H
import Text.Shakespeare.Text
import qualified Data.Dynamic as D
import qualified Data.Map as M

class (Flat r, Flat (PageData r), HasParser U r, Typeable r, Typeable (PageData r)) => IsPage r where
  type PageData r
  page :: Page r (PageData r)

data Page r d = Page
  { pgInit   :: RemotePtr r d
  , pgWidget :: Dynamic d -> Html () }

data SomePage = forall r d. SomePage (Page r d)
data PageDict = forall a. PageDict (Dict (IsPage a))

data Page404Data = Page404Data
  deriving stock (Show, Eq, Generic)
  deriving anyclass Flat

instance IsPage () where
  type PageData () = Page404Data
  page = Page (RemotePtr (static (SomeBackend (Backend init404))) 'init404) (const page404)

init404 :: () -> BackendIO Page404Data
init404 _ = pure Page404Data

instance HasParser U () where
  parser = pUnit

frontendRouter :: [PageDict] -> Html ()
frontendRouter p = do
  win <- liftJSM (jsg ("window"::Text))
  let
    par = pageParser p
    pag :: forall a. Dict (IsPage a) -> Page a (PageData a)
    pag Dict = page
    dynBk :: forall (a :: *). Dict (IsPage a) -> D.Dynamic -> RemotePtr a (PageData a) -> JSM D.Dynamic
    dynBk Dict d r = D.toDyn <$> xhrRemote r (D.fromDyn @a d (error "invalid dynamic value"))
    init (dn, PageDict dict@Dict) = do
      let ini = pgInit (pag dict)
      liftJSM (dynBk dict dn ini)
    parseRoute = do
      search <- liftJSM $ fromJSValUnchecked =<< jsg ("location"::Text) ! ("search"::Text)
      pathname <- liftJSM $ fromJSValUnchecked =<< jsg ("location"::Text) ! ("pathname"::Text)
      init $ fromMaybe (D.toDyn (), PageDict (Dict @(IsPage ()))) $ listToMaybe $ parseWith par (pathname <> search)
  r <- parseRoute
  (dRoute, mRoute) <- liftIO (newDyn r)
  domEvent_ (JsNode win) "popstate" do
    parseRoute >>= \x -> liftIO $ sync $ mRoute (const x)
  dynPage p dRoute

dynPage :: [PageDict] -> Dynamic D.Dynamic -> Html ()
dynPage p d = do
  let
    mm = mfng p
    ht :: forall a. Dict (IsPage a) -> Dynamic D.Dynamic -> Html ()
    ht Dict dyn = do
      ini <- fromMaybe (error "invalid dynamic value") . D.fromDynamic @(PageData a) <$> liftIO (dnRead dyn)
      let h = pgWidget $ page @a
      dyn1 <- liftIO $ mapMaybeD ini (D.fromDynamic @(PageData a)) dyn
      h dyn1
    ht2 :: D.Dynamic -> Html ()
    ht2 dyn = case fromMaybe (error "invalid dynamic value") $ M.lookup (typeRepFingerprint (D.dynTypeRep dyn)) mm of
      PageDict dict -> ht dict d
    mfng :: [PageDict] -> M.Map Fingerprint PageDict
    mfng = M.fromList . fmap \x@(PageDict d) -> (pp d, x) where
      pp :: forall a. Dict (IsPage a) -> Fingerprint
      pp Dict = typeRepFingerprint (typeRep (Proxy::Proxy (PageData a)))
    dd = holdUniqDynBy \a b -> fing a == fing b where
      fing x = typeRepFingerprint (D.dynTypeRep x)
  d1 <- liftIO (dnRead d)
  dynHtml (fmap ht2 (dd d))

pageParser :: [PageDict] -> Parser UrlChunks D.Dynamic (D.Dynamic, PageDict)
pageParser p = Parser (par p) pri where
  mm = mfng p
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
  mfng :: [PageDict] -> M.Map Fingerprint PageDict
  mfng = M.fromList . fmap \x@(PageDict d) -> (pp d, x) where
    pp :: forall a. Dict (IsPage a) -> Fingerprint
    pp Dict = typeRepFingerprint (typeRep (Proxy::Proxy a))
  dPar :: Typeable a => Parser' s a -> Parser' s D.Dynamic
  dPar = dimap g f where
    f = D.toDyn
    g = fromMaybe errorTrace . D.fromDynamic

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
        liftJSM $ ev # ("preventDefault"::Text) $ ()
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
