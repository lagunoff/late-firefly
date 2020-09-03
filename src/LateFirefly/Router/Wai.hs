module LateFirefly.Router.Wai where

import Data.Constraint
import Data.Dynamic
import Data.Text.Encoding as T
import GHC.StaticPtr
import LateFirefly.DB
import LateFirefly.Parser
import LateFirefly.Prelude
import LateFirefly.Router
import LateFirefly.Template
import Massaraksh.Main
import Network.HTTP.Types
import Network.Wai
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Massaraksh as H

html5Router :: (?conn::Connection) => [PageDict] -> Application -> Application
html5Router ps next req resp = maybe nothin just mp where
  mp = listToMaybe (parseWith par uPath)
  par = pageParser ps
  bPath = encodePath (pathInfo req) (queryString req)
  uPath = T.decodeUtf8 $ BSL.toStrict $ BS.toLazyByteString bPath
  pag :: forall a. Dict (IsPage a) -> Page a (PageData a)
  pag Dict = page
  cas :: forall a b. (Typeable b) => Dict (IsPage a) -> b -> Maybe (PageData a)
  cas Dict = cast
  dynBk :: forall a r. Dynamic -> Backend a r -> BackendIO r
  dynBk d (Backend f) = f (fromDyn d (error "html5Router: fromDyn error"))
  somBk p = deRefStaticPtr @SomeBackend $ rptrStaticPtr (pgInit p)
  just (dn, PageDict dict) = case somBk (pag dict) of
    SomeBackend b@Backend{} -> do
      r <- unEio (dynBk dn b)
      let pdata = fromMaybe (error "html5Router: fromDyn error") $ cas dict r
      (dyn, m) <- H.newDyn pdata
      b <- htmlBuilder <$> buildHtml (pgWidget (pag dict) dyn)
      resp (responseBuilder ok200 [] b)
  nothin = do
    b <- htmlBuilder <$> buildHtml page404
    resp (responseBuilder ok200 [] b)

htmlBuilder :: BS.Builder -> BS.Builder
htmlBuilder h =
  "<!doctype html>\n" <>
  "<html>\n" <>
  "  <head>\n" <>
  "    <script type=\"text/javascript\" src=\"/jsaddle.js\"></script>\n" <>
  "  </head>" <>
  "  <body>\n" <> h <> "\n" <>
  "  </body>\n" <>
  "</html>\n"
