module Router where

import Control.Lens
import Data.Constraint
import Data.List as L
import Data.Proxy
import Data.Text as T
import Database.SQLite.Simple
import GHC.TypeLits
import Network.URI
import qualified Data.Map as M

import "this" Intro

data UrlParts = UP
  { partsPath  :: [Text]         -- ^ Path segments
  , partsQuery :: [(Text, Text)] -- ^ GET parameters
  }
  deriving (Eq, Show, Generic)

data Route a where
  HomeR    :: Route "HomeR"
  EpisodeR :: {code::Text} -> Route "EpisodeR"
  SeriesR  :: {series::Text} -> Route "SeriesR"

data SomeRoute = forall a. KnownSymbol a => SR (Route a)

class KnownSymbol l => IsPage l o | l -> o where
  pageInit :: (?conn::Connection) => Route l -> ServerIO o
  pageWidget :: Route l -> o -> Html ()

data PageDict = forall l a. PageDict (Dict (IsPage l a))

partsToRoute :: Prism' UrlParts SomeRoute
partsToRoute = prism' build match where
  match :: UrlParts -> Maybe SomeRoute
  match = \case
    UP [] _                  -> Just $ SR HomeR
    UP ("episode":code:_) _  -> Just $ SR EpisodeR{..}
    UP ("series":series:_) _ -> Just $ SR SeriesR{..}
    _                        -> Nothing
  build :: SomeRoute -> UrlParts
  build = \case
    SR HomeR        -> UP [] []
    SR EpisodeR{..} -> UP ["episode", code] []
    SR SeriesR{..}  -> UP ["series", series] []

urlToParts ::Iso' Text UrlParts
urlToParts = iso apply unapply where
  unapply (UP f q) =
    T.intercalate "?" ([segments] <> bool [] [params] (params/="")) where
      segments = T.intercalate "/" $ fmap escUri f
      params = T.intercalate "&". L.filter (/= "")
        . fmap (\(k, v) -> k <> "=" <> v) . fmap (bimap escUri escUri)
        $ q
  apply txt = UP seg qry
    where
      (segTxt, qryTxt) = bimap unEscUri unEscUri . breakOn1 "?" $ txt
      seg = T.splitOn "/" segTxt & L.filter (/="")
      qry = T.splitOn "&" qryTxt & L.filter (/="") <&> breakOn1 "="
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

escUri :: Text -> Text
escUri = T.pack . escapeURIString isAllowed . T.unpack where
  isAllowed c = c `elem` (['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "-_.~:/,")

unEscUri :: Text -> Text
unEscUri = T.pack . unEscapeString . T.unpack

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
