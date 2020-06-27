module LateFirefly.Router.Wai where

import Network.Wai
import Data.Text.Encoding as T
import LateFirefly.Router
import LateFirefly.Prelude

import Network.HTTP.Types.URI
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as BSL

html5Router :: Application -> Application
html5Router next req resp = do
  let
    path = encodePath (pathInfo req) (queryString req)
    path' = traceShowId $ T.decodeUtf8 $ BSL.toStrict $ BS.toLazyByteString  path
    req' = req { queryString = [], pathInfo = [] }
    newReq = maybe req (const req') $ listToMaybe $ parseUrl @Route path'
  next newReq resp
