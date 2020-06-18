{-# LANGUAGE DeriveDataTypeable #-}
module JavaScript.Web.XMLHttpRequest where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class

import GHCJS.Types
import GHCJS.Marshal
import GHCJS.Internal.Types
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.ByteString.Base64 as B64

import qualified GHCJS.Buffer as Buffer

import GHC.Generics
import Data.Coerce
import Data.Either

import Data.ByteString
import Data.Data
import Data.JSString.Internal.Type ( JSString(..) )

import Data.Text (Text)

import           Data.JSString.Text (textFromJSString)
import qualified Data.JSString as JSS

import JavaScript.TypedArray.Internal.Types (SomeTypedArray(..))
import JavaScript.TypedArray.ArrayBuffer (ArrayBuffer)
import JavaScript.TypedArray.ArrayBuffer.Internal (SomeArrayBuffer(..))
import JavaScript.Web.XMLHttpRequest.Internal
import Language.Javascript.JSaddle hiding (textFromJSString)
import GHCJS.Buffer
import GHCJS.Buffer.Types
import JavaScript.TypedArray.Internal.Types


data Method = GET | POST | PUT | DELETE | Method JSString
  deriving (Show, Eq, Ord)

data XHRError = XHRError String
              | XHRAborted
              deriving (Generic, Data, Typeable, Show, Eq)

instance Exception XHRError

methodJSString :: Method -> JSString
methodJSString GET    = "GET"
methodJSString POST   = "POST"
methodJSString PUT    = "PUT"
methodJSString DELETE = "DELETE"
methodJSString (Method m) = m

type Header = (JSString, JSString)

data FormDataVal = StringVal JSString
                 -- | BlobVal   Blob     (Maybe JSString)
                 -- | FileVal   File     (Maybe JSString)
  deriving (Typeable)

data Request = Request { reqMethod          :: Method
                       , reqURI             :: JSString
                       , reqLogin           :: Maybe (JSString, JSString)
                       , reqHeaders         :: [Header]
                       , reqWithCredentials :: Bool
                       , reqData            :: RequestData
                       }
  deriving (Typeable)

data RequestData = NoData
                 | StringData     JSString
                 | TypedArrayData (forall e. SomeTypedArray e Immutable)
                 | FormData       [(JSString, FormDataVal)]
  deriving (Typeable)

data Response a = Response { contents              :: Maybe a
                           , status                :: Int
                           , getAllResponseHeaders :: JSM JSString
                           , getResponseHeader     :: JSString -> JSM (Maybe JSString)
                           }

instance Functor Response where fmap f r = r { contents = fmap f (contents r) }

class ResponseType a where
    getResponseTypeString :: Proxy a  -> JSString
    wrapResponseType      :: JSVal -> JSM a

instance ResponseType ArrayBuffer where
  getResponseTypeString _ = "arraybuffer"
  wrapResponseType        = pure . SomeArrayBuffer

instance ResponseType ByteString where
  getResponseTypeString _ = "arraybuffer"
  wrapResponseType        = arrayBufferToByteString . coerce

instance m ~ Immutable => ResponseType JSString where
  getResponseTypeString _ = "text"
  wrapResponseType        = fromJSValUnchecked

-- instance ResponseType Blob where
--   getResponseTypeString _ = "blob"
--   wrapResponseType        = SomeBlob

-- instance m ~ Immutable => ResponseType (SomeValue m) where
--   getResponseTypeString _ = "json"
--   wrapResponseType        = SomeValue

newtype JSFormData = JSFormData JSVal deriving (Typeable)

newtype XHR = XHR JSVal deriving (Typeable)

-- -----------------------------------------------------------------------------
-- main entry point

xhr :: forall a. ResponseType a => Request -> JSM (Response a)
xhr req =
  js_createXHR >>= \x ->
  let doRequest = do
        case reqLogin req of
          Nothing           ->
            js_open2 (methodJSString (reqMethod req)) (reqURI req) x
          Just (user, pass) ->
            js_open4 (methodJSString (reqMethod req)) (reqURI req) user pass x
        js_setResponseType
          (getResponseTypeString (Proxy :: Proxy a)) x
        forM_ (reqHeaders req) (\(n,v) -> js_setRequestHeader n v x)

        case reqWithCredentials req of
          True  -> js_setWithCredentials x
          False -> return ()

        r <- case reqData req of
          NoData                            ->
            js_send Nothing x
          StringData str                    -> do
            s <- toJSVal str
            js_send (Just s) x
          TypedArrayData (SomeTypedArray t) ->
            js_send (Just t) x
          -- FormData xs                       -> do
          --   fd@(JSFormData fd') <- js_createFormData
          --   forM_ xs $ \(name, val) -> case val of
          --     StringVal str               ->
          --       js_appendFormData2 name (pToJSVal str) fd
          --     -- BlobVal (SomeBlob b) mbFile ->
          --     --   appendFormData name b mbFile fd
          --     -- FileVal (SomeBlob b) mbFile ->
          --     --   appendFormData name b mbFile fd
          --   js_send1 fd' x
        case r of
          0 -> do
            status <- js_getStatus x
            r      <- do
              hr <- js_hasResponse x
              if hr then fmap Just . wrapResponseType =<< js_getResponse x
                    else pure Nothing
            return $ Response r
                              status
                              (js_getAllResponseHeaders x)
                              (\h -> getResponseHeader' h x)
          1 -> liftIO (throwIO XHRAborted)
          2 -> liftIO $ throwIO (XHRError "network request error")
  in doRequest

-- appendFormData :: JSString -> JSVal
--                -> Maybe JSString -> JSFormData -> JSM ()
-- appendFormData name val Nothing         fd =
--   js_appendFormData2 name val fd
-- appendFormData name val (Just fileName) fd =
--   js_appendFormData3 name val fileName fd

getResponseHeader' :: JSString -> XHR -> JSM (Maybe JSString)
getResponseHeader' name x = do
  h <- js_getResponseHeader name x
  isNullRes <- ghcjsPure (isNull h)
  if isNullRes then pure Nothing else fromJSVal h

-- -----------------------------------------------------------------------------
-- utilities

xhrString :: Request -> JSM (Response String)
xhrString = fmap (fmap JSS.unpack) . xhr

xhrText :: Request -> JSM (Response Text)
xhrText = fmap (fmap textFromJSString) . xhr

xhrByteString :: Request -> JSM (Response ByteString)
xhrByteString req = do
  resp <- xhr req
  c <- forM (contents resp)
    $ ghcjsPure . Buffer.createFromArrayBuffer
    >=> ghcjsPure . Buffer.toByteString 0 Nothing
  pure resp {contents = c}

arrayBufferToByteString :: ArrayBuffer -> JSM ByteString
arrayBufferToByteString ab =
  fromRight (error "Base64 decode error")
    . B64.decode
    . encodeUtf8
    . textFromJSString
    <$> arrayBufferToBase64 ab


arrayBufferToBase64 :: ArrayBuffer -> JSM JSString
arrayBufferToBase64 ab = do
  toB64 <- eval $
    "(function (buffer) {\n" ++
    "  return btoa(String.fromCharCode.apply(null, new Uint8Array(buffer)));" ++
    "})"
  s <- call toB64 valUndefined (coerce @_ @JSVal ab)
  fromJSValUnchecked s
