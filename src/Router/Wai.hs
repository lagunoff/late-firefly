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
  pag :: forall i o. Dict (IsPage i o) -> _
  pag Dict = page @i
  lay :: forall i o. Dict (IsPage i o) -> SomeRoute -> o -> Layout
  lay Dict sr o = pLayout (page @i) (fromMaybe errorTrace $ fromSome @i sr) o
  dynBk :: forall i o. Dict (IsPage i o) -> SomeRoute -> UnServer (Route i) o -> ServerIO o
  dynBk Dict sr f = f $ fromMaybe errorTrace $ fromSome @i sr
  just (sr, PageDict dict) = do
    let Page{..} = pag dict
    pdata <- unEio (dynBk dict sr pInit)
    let Layout{..} = lay dict sr pdata
    let b = htmlBuilder . BS.lazyByteString . renderBS $ pTemplate styles script html
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
