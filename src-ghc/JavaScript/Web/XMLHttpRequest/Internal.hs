{-# LANGUAGE NoOverloadedStrings #-}
module JavaScript.Web.XMLHttpRequest.Internal where

import Control.Monad
import Control.Monad.IO.Class
import GHCJS.Types
import GHCJS.Prim
import GHCJS.Marshal
import Data.JSString.Internal.Type ( JSString(..) )
import Language.Javascript.JSaddle hiding (textFromJSString)
import {-# SOURCE #-} JavaScript.Web.XMLHttpRequest (XHR(..))
import Data.Coerce
import Control.Concurrent.MVar

js_setWithCredentials :: XHR -> JSM ()
js_setWithCredentials (coerce @_ @JSVal -> xhr) = do
  void $ xhr <# "withCredentials" $ True

js_createXHR :: JSM XHR
js_createXHR = do
  v <- new (jsg "XMLHttpRequest") $ ()
  pure (XHR v)

js_setResponseType :: JSString -> XHR -> JSM ()
js_setResponseType _1 (coerce @_ @JSVal -> xhr) = do
  xhr <# "responseType" $ _1

js_abort :: XHR -> JSM ()
js_abort (coerce @_ @JSVal -> xhr) = do
  void $ xhr # "abort" $ ()

js_setRequestHeader :: JSString -> JSString -> XHR -> JSM ()
js_setRequestHeader _1 _2 (coerce @_ @JSVal -> xhr) = do
  void $ xhr # "setRequestHeader" $ (_1, _2)

js_open2 :: JSString -> JSString -> XHR -> JSM ()
js_open2 _1 _2 (coerce @_ @JSVal -> xhr) = do
  void $ xhr # "open" $ (_1, _2)

js_open4 :: JSString -> JSString -> JSString -> JSString -> XHR -> JSM ()
js_open4 _1 _2 _3 _4 (coerce @_ @JSVal -> xhr) = do
  void $ xhr # "open" $ (_1, _2, True, _3, _4)

-- foreign import javascript unsafe
--   "new FormData()"
--   js_createFormData :: JSM JSFormData
-- foreign import javascript unsafe
--   "$3.append($1,$2)"
--   js_appendFormData2 :: JSString -> JSVal -> JSFormData -> JSM ()
-- foreign import javascript unsafe
--   "$4.append($1,$2,$3)"
--   js_appendFormData3 :: JSString -> JSVal -> JSString -> JSFormData -> JSM ()

js_getStatus :: XHR -> JSM Int
js_getStatus (coerce @_ @JSVal -> xhr) = do
  r <- xhr ! "status"
  fromJSValUnchecked r

js_getResponse :: XHR -> JSM JSVal
js_getResponse (coerce @_ @JSVal -> xhr) =
  xhr ! "response"

js_hasResponse :: XHR -> JSM Bool
js_hasResponse (coerce @_ @JSVal -> xhr) = do
  r <- xhr ! "response"
  ghcjsPure $ isTruthy r

js_getAllResponseHeaders :: XHR -> JSM JSString
js_getAllResponseHeaders (coerce @_ @JSVal -> xhr) = do
  r <- xhr # "getAllResponseHeaders" $ ()
  fromJSValUnchecked r

js_getResponseHeader :: JSString -> XHR -> JSM JSVal
js_getResponseHeader d (coerce @_ @JSVal -> xhr) =
  xhr # "getResponseHeader" $ [d]

-- -----------------------------------------------------------------------------

js_send :: Maybe JSVal -> XHR -> JSM Int
js_send body (coerce @_ @JSVal -> xhr) = do
  mvar <- liftIO newEmptyMVar
  errorCb <- function $ fun \_ _ _ -> do
    liftIO (putMVar mvar 2)
  loadCb <- function $ fun \_ _ _ -> do
    liftIO (putMVar mvar 0)
  abortCb <- function $ fun \_ _ _ -> do
    liftIO (putMVar mvar 1)
  xhr # "addEventListener" $ ("error", errorCb)
  xhr # "addEventListener" $ ("load", loadCb)
  xhr # "addEventListener" $ ("abort", abortCb)
  case body of
    Nothing -> xhr # "send" $ ()
    Just b  -> xhr # "send" $ b
  syncPoint
  r <- liftIO (takeMVar mvar)
  syncPoint
  freeFunction errorCb
  freeFunction loadCb
  freeFunction abortCb
  pure r
