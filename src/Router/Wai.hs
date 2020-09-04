module Router.Wai where

import Data.Constraint
import Data.Dynamic
import Data.Text.Encoding as T
import Lucid
import Network.HTTP.Types
import Network.Wai
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as BSL

import "this" DB
import "this" Parser
import "this" Intro
import "this" Router

html5Router :: (?conn::Connection) => [PageDict] -> Application -> Application
html5Router ps next req resp = maybe nothin just mp where
  mp = listToMaybe (parseWith par uPath)
  par = pageParser ps
  bPath = encodePath (pathInfo req) (queryString req)
  uPath = T.decodeUtf8 $ BSL.toStrict $ BS.toLazyByteString bPath
  ini :: forall i o. Dict (IsPage i o) -> UnServer i o
  ini Dict = pageInit @i
  wid :: forall i o. Dict (IsPage i o) -> o -> Html ()
  wid Dict = pageWidget @i
  dynBk :: Dict (IsPage i o) -> Dynamic -> UnServer i o -> ServerIO o
  dynBk Dict d f = f (fromDyn d (error "html5Router: fromDyn error"))
  just (dn, PageDict dict) = do
    pdata <- unEio (dynBk dict dn (ini dict))
    let b = htmlBuilder . BS.lazyByteString $ renderBS (wid dict pdata)
    resp (responseBuilder ok200 [(hContentType, "text/html")] b)
  nothin = do
    let b = htmlBuilder . BS.lazyByteString $ renderBS page404
    resp (responseBuilder ok200 [(hContentType, "text/html")] b)

page404 :: Html ()
page404 = div_ do
  h1_ "Not found"

htmlBuilder :: BS.Builder -> BS.Builder
htmlBuilder h =
  "<!doctype html>\n" <>
  "<html>\n" <>
  "  <head>\n" <>
  "    <script type=\"text/javascript\" src=\"/jsaddle.js\"></script>\n" <>
  "  </head>" <>
  "  <body>\n" <>  h <> "\n" <>
  "  </body>\n" <>
  "</html>\n"
