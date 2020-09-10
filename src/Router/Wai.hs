module Router.Wai where

import Control.Lens
import Data.Constraint
import Data.Text.Encoding as T
import Lucid
import Network.HTTP.Types
import Network.Wai
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as M

import "this" DB
import "this" Dev
import "this" Intro
import "this" Router
import "this" Site.Template

html5Router :: (?conn::Connection) => [PageDict] -> Application
html5Router ps req resp = maybe nothin just mr where
  mr = mp >>= \r -> (r,) <$> M.lookup (routeLabel r) pm
  pm = groupPages ps
  mp = preview (urlToParts . partsToRoute) uPath
  bPath = encodePath (pathInfo req) (queryString req)
  uPath = T.decodeUtf8 $ BSL.toStrict $ BS.toLazyByteString bPath
  ini :: forall i o. Dict (IsPage i o) -> UnServer (Route i) o
  ini Dict = pageInit @i
  wid :: forall i o. Dict (IsPage i o) -> SomeRoute -> o -> Html ()
  wid Dict sr o = pageWidget @i (fromMaybe errorTrace $ fromSome @i sr) o
  tpl :: forall i o. Dict (IsPage i o) -> Html () -> Html ()
  tpl Dict = pageTemplate @i
  dynBk :: forall i o. Dict (IsPage i o) -> SomeRoute -> UnServer (Route i) o -> ServerIO o
  dynBk Dict sr f = f $ fromMaybe errorTrace $ fromSome @i sr
  just (sr, PageDict dict) = do
    pdata <- unEio (dynBk dict sr (ini dict))
    let b = htmlBuilder . BS.lazyByteString . renderBS . (tpl dict) $ wid dict sr pdata
    resp (responseBuilder ok200 [(hContentType, "text/html")] b)
  nothin = do
    let b = htmlBuilder . BS.lazyByteString . renderBS $ page404
    resp (responseBuilder ok200 [(hContentType, "text/html")] b)

htmlBuilder :: BS.Builder -> BS.Builder
htmlBuilder h =
  "<!doctype html>\n" <>
  "<html lang=\"en\">\n" <>
  "  <head>\n" <>
  "    <meta charset=\"UTF-8\"/>\n" <>
  "    <script type=\"text/javascript\">" <> reloadJs <> "</script>\n" <>
  "  </head>" <>
  "  <body>\n" <>  h <> "\n" <>
  "  </body>\n" <>
  "</html>\n"
