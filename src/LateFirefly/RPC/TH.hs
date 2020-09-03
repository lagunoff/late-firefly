{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE CPP #-}
module LateFirefly.RPC.TH where

import Control.Error
import Control.Monad
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Catch as Catch
import qualified Control.Monad.Except as E
import Data.ByteString
import Data.Coerce
import Data.IORef
import Data.Typeable
import Data.Text.Encoding (decodeUtf8)
import Flat
import GHC.Fingerprint.Type
import GHC.StaticPtr
import GHC.Int
import JavaScript.Web.XMLHttpRequest as XHR
import Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax
import Language.Javascript.JSaddle
import LateFirefly.DB
import System.IO.Unsafe
import Unsafe.Coerce
import LateFirefly.Backend as This
import qualified Data.ByteString.Base64 as B64
import qualified Data.Map as M
import qualified Data.Set as S
import LateFirefly.Eio

#ifndef ghcjs_HOST_OS
import JavaScript.TypedArray.Internal.Types
#else
import JavaScript.TypedArray
import Unsafe.Coerce
import GHCJS.Buffer as Buffer
#endif

type DynSPT = M.Map Name SomeBackend

data RpcRequest = RpcRequest
  { rpcqKey  :: StaticKey
  , rpcqName :: Name
  , rpcqArg  :: ByteString }
  deriving (Show, Eq, Generic, Flat)

xhrRemoteTH
  :: (Flat a, Flat r) => UnBackend a r -> RemotePtr a r -> a -> JSM r
xhrRemoteTH _ = xhrRemote

xhrRemote :: (Flat a, Flat r) => RemotePtr a r -> a -> JSM r
xhrRemote r a = do
  syncPoint
  let k = staticKey $ rptrStaticPtr r
  let n = rptrName r
  origin::JSString <- fromJSValUnchecked =<< jsg ("window"::JSString)
    ! ("location"::JSString) ! ("origin"::JSString)
  buffer <- byteStringToArrayBuffer $ flat $ RpcRequest k n (flat a)
  let
    dat = TypedArrayData (unsafeCoerce buffer)
    req = Request POST (origin <> "/rpc") Nothing [] False dat
  either (throw . BackendError) id
    . either (throw . FlatError . show) id
    . unflat
    . fromMaybe (throw (BadResponse "responce body empty"))
    . contents
    <$> (xhrByteString req `Catch.catch` (throwM . This.XHRError))

dynSPT :: IORef (S.Set Name)
dynSPT = unsafePerformIO (newIORef S.empty)

remote :: Name -> Q Exp
remote n = do
  liftIO $ modifyIORef dynSPT (S.insert n)
  let keyE = [|RemotePtr (static (someBackend $(varE n))) $(lift n)|]
  [|liftJSM . xhrRemoteTH $(varE n) $keyE |]

runBackend :: (?conn::Connection) => SomeBackend -> ByteString -> IO ByteString
runBackend (SomeBackend b1) arg = rb b1 arg where
  rb :: forall a b. Backend a b -> ByteString -> IO ByteString
  rb (Backend f) arg = flat @(Either PublicBackendError b) <$> E.runExceptT do
    a <- E.ExceptT $ ee (BEFlatError . show) (unflat @a arg)
    E.ExceptT $ ee id =<< runEio (f a)
    where
      ee e = either (fmap Left . logError . e) (fmap Right . pure)
      logError _ = pure $ ErrorCode (coerce (0::Int64))

someBackend
  :: forall a b
  . (Typeable a, Typeable b, Flat a, Flat b)
  => UnBackend a b -> SomeBackend
someBackend f = SomeBackend (Backend f)

readDynSPT :: Q Exp
readDynSPT = do
  eps <- liftIO (readIORef dynSPT)
  epsExp <- forM (S.toAscList eps) \n ->
    pure $ tupE [lift n, varE 'someBackend `appE` varE n]
  [|M.fromList $(listE epsExp)|]

byteStringToArrayBuffer :: ByteString -> JSM Uint8Array
#ifndef ghcjs_HOST_OS
byteStringToArrayBuffer =
  base64ToArrayBuffer . textToJSString . decodeUtf8 . B64.encode

base64ToArrayBuffer :: JSString -> JSM Uint8Array
base64ToArrayBuffer jss = do
  fromB64 <- eval $
    "(function (base64) {\n" ++
    "  return Uint8Array.from(atob(base64), function(c) { return c.charCodeAt(0); });\n" ++
    "})"
  jss' <- toJSVal jss
  coerce <$> call fromB64 valUndefined jss'
#else
byteStringToArrayBuffer =
  pure . (\(x, off, len) -> js_slice (getUint8Array x) off len) . Buffer.fromByteString

foreign import javascript unsafe
  "$1.slice($2, $3)"
  js_slice :: Uint8Array -> Int -> Int -> Uint8Array
#endif

instance Flat StaticKey where
  size (Fingerprint lo hi) n = size lo n + size hi n
  encode (Fingerprint lo hi) = encode lo <> encode hi
  decode = Fingerprint <$> decode <*> decode
