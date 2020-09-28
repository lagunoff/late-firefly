module Router where

import Control.Lens
import Data.Constraint
import Data.List as L
import Data.Maybe
import Data.Proxy
import Data.Text as T
import qualified Data.Text.Lazy as LT
import Database.SQLite.Simple
import GHC.TypeLits
import Network.URI
import Text.Read
import qualified Data.Map as M

import "this" Intro
import {-# SOURCE #-} "this" Site.Template

data UrlParts = Url
  { partsPath  :: [Text]         -- ^ Path segments
  , partsQuery :: [(Text, Text)] -- ^ GET parameters
  }
  deriving (Eq, Show, Generic)

data Route a where
  HomeR    :: Route "Home"
  SearchR  :: {search::Text, offset::Int} -> Route "Search"
  EpisodeR :: {series::Text, code::Text} -> Route "Episode"
  TitleR   :: {slug::Text} -> Route "Title"
  GraphQlR :: Route "GraphQl"

data SomeRoute = forall a. KnownSymbol a => SR (Route a)

class KnownSymbol l => IsPage l o | l -> o where
  pageTemplate :: Html () -> Html ()
  pageTemplate = htmlTemplate
  pageInit :: (?conn::Connection) => Route l -> ServerIO o
  pageWidget :: Route l -> o -> Html ()

class KnownSymbol l => IsPage2 l o | l -> o where
  page :: Page (Route l) o

data Page i o = Page
  { pTemplate :: Html () -> Html ()
  , pInit     :: (?conn::Connection) => i -> ServerIO o
  , pWidget   :: i -> o -> Html ()
  , pCss      :: LT.Text }

defPage :: Page i o
defPage = Page htmlTemplate (const undefined) (\_ _ -> pure ()) mempty

unitPage :: Page i ()
unitPage = Page htmlTemplate (const (pure ())) (\_ _ -> pure ()) mempty

data PageDict = forall l a. PageDict (Dict (IsPage l a))

partsToRoute :: Prism' UrlParts SomeRoute
partsToRoute = prism' build match where
  match :: UrlParts -> Maybe SomeRoute
  match = \case
    Url [] q
      | Just search <- lookupNe "s" q
      , offset      <- lookupInt 0 "offset"  q ->
        Just $ SR SearchR{..}
    Url ["graphql"] _      -> Just $ SR GraphQlR
    Url [series,code] _    -> Just $ SR EpisodeR{..}
    Url [slug] _           -> Just $ SR TitleR{..}
    Url [] _               -> Just $ SR HomeR
    _                     -> Nothing
  build :: SomeRoute -> UrlParts
  build = \case
    SR HomeR        -> Url [] []
    SR SearchR{..}  -> Url [] (catMaybes [Just ("s", search), parDef 0 "offset" offset])
    SR EpisodeR{..} -> Url [series, code] []
    SR TitleR{..}   -> Url [slug] []
    SR GraphQlR{}   -> Url ["graphql"] []

  lookupNe n = mfilter (/="") . L.lookup n
  lookupInt d n = fromMaybe d . (readMaybe @Int . T.unpack =<<) . L.lookup n
  parDef d n x = bool (Just (n,showt x)) Nothing (x==d)

urlToParts ::Iso' Text UrlParts
urlToParts = iso apply unapply where
  unapply (Url f q) =
    T.intercalate "?" ([segments] <> bool [] [params] (params/="")) where
      segments = T.intercalate "/" $ fmap enUri f
      params = T.intercalate "&". L.filter (/= "")
        . fmap (\(k, v) -> k <> "=" <> v) . fmap (bimap enUri enUri)
        $ q
  apply txt = Url seg qry where
    (segTxt, qryTxt) = breakOn1 "?" txt
    seg = fmap deUri . L.filter (/="") . T.splitOn "/" $ segTxt
    qry = fmap (breakOn1 "=" . deUri) . L.filter (/="") . T.splitOn "&" $ qryTxt
    breakOn1 s t = T.breakOn s t & _2 %~ T.drop 1

groupPages :: [PageDict] -> M.Map Text PageDict
groupPages ps = M.fromList $ fmap f ps where
  f p = (pageLabel p, p)

pageLabel :: PageDict -> Text
pageLabel (PageDict d) = f d where
  f :: forall l a. Dict (IsPage l a) -> Text
  f Dict = T.pack $ symbolVal (Proxy @l)

routeLabel :: SomeRoute -> Text
routeLabel (SR r) = f r where
  f :: forall l. KnownSymbol l => Route l -> Text
  f _ = T.pack $ symbolVal (Proxy @l)

enUri :: Text -> Text
enUri = T.pack . escapeURIString isAllowed . T.unpack where
  isAllowed c = c `elem` (['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "-_.~:/,")

deUri :: Text -> Text
deUri = T.pack . unEscapeString . T.unpack

fromSome :: forall l. KnownSymbol l => SomeRoute -> Maybe (Route l)
fromSome (SR r) = f r where
  f :: forall l1. KnownSymbol l1 => Route l1 -> Maybe (Route l)
  f r = case sameSymbol (Proxy @l) (Proxy @l1) of
    Just Refl -> Just r
    Nothing   -> Nothing

linkTo :: KnownSymbol l => Route l -> Html () -> Html ()
linkTo = linkMay . Just

linkAtt :: KnownSymbol l => Route l -> [Attribute] -> Html () -> Html ()
linkAtt r att = linkAttMay (Just r) att

linkMay :: KnownSymbol l => Maybe (Route l) -> Html () -> Html ()
linkMay r = linkAttMay r []

linkAttMay :: KnownSymbol l => Maybe (Route l) -> [Attribute] -> Html () -> Html ()
linkAttMay r att = a_
  (maybe att ((:att) . href_ . ("/"<>) . review (urlToParts . partsToRoute) . SR) r)

urlTo :: KnownSymbol l => Route l -> Text
urlTo = ("/"<>) . review (urlToParts . partsToRoute) . SR
