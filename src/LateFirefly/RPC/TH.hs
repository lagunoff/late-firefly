{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI #-}
{-# LANGUAGE CPP #-}
module LateFirefly.RPC.TH where

import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString
import Data.Coerce
import Data.Either
import Data.IORef
import Data.Maybe
import Data.Reflection
import Data.Text.Encoding (decodeUtf8)
import Debug.Trace
import Flat
import GHC.Fingerprint.Type
import GHC.StaticPtr
import JavaScript.Web.XMLHttpRequest
import Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax
import Language.Javascript.JSaddle
import LateFirefly.DB
import System.IO.Unsafe
import Unsafe.Coerce
import qualified Data.ByteString.Base64 as B64
import qualified Data.Map as M
import qualified Data.Set as S

#ifndef ghcjs_HOST_OS
import JavaScript.TypedArray.Internal.Types
#else
import JavaScript.TypedArray
import Unsafe.Coerce
import GHCJS.Buffer as Buffer
#endif

type (:->) a r = Given Connection => a -> IO r

newtype Ep = Ep {unEp :: ByteString :-> ByteString}

type DynSPT = M.Map Name Ep

data RpcRequest = RpcRequest
  { rr_key  :: StaticKey
  , rr_name :: Name
  , rr_arg  :: ByteString }
  deriving (Show, Eq, Generic, Flat)

sendRpcXhr :: (Flat a, Flat r) => Name -> StaticKey -> (a :-> r) -> a -> JSM r
sendRpcXhr n k _ a = do
  origin :: JSString <- fromJSValUnchecked =<< jsg ("window" :: JSString)
    ! ("location" :: JSString) ! ("origin" :: JSString)
  buffer <- byteStringToArrayBuffer $ flat $ RpcRequest k n (flat a)
  let
    dat = TypedArrayData (unsafeCoerce buffer)
    req = Request POST (origin <> "/rpc") Nothing [] False dat
  either (error . show) id
    . unflat
    . fromMaybe (error "responce body empty")
    . contents
    <$> xhrByteString req

dynSPT :: IORef (S.Set Name)
dynSPT = unsafePerformIO (newIORef S.empty)

remote :: Name -> Q Exp
remote n = do
  liftIO $ modifyIORef dynSPT (S.insert n)
  [| liftJSM . sendRpcXhr $(lift n) (staticKey $(staticE ([|toEp|] `appE` varE n))) $(varE n) |]

toEp :: forall a b. (Flat a, Flat b, Show b) => (a :-> b) -> Ep
toEp f = Ep (fmap (flat @b) . f . fromRight (error "unflat error") . (unflat @a))

readDynSPT :: Q Exp
readDynSPT = do
  eps <- liftIO (readIORef dynSPT)
  epsExp <- forM (S.toAscList eps) \n ->
    pure $ tupE [lift n, varE 'toEp `appE` varE n]
  [| M.fromList $(listE epsExp) |]

instance Flat Name
instance Flat OccName
instance Flat NameFlavour
instance Flat ModName
instance Flat NameSpace
instance Flat PkgName

byteStringToArrayBuffer :: ByteString -> JSM Uint8Array
#ifndef ghcjs_HOST_OS
byteStringToArrayBuffer =
  base64ToArrayBuffer . textToJSString . decodeUtf8 . B64.encode

base64ToArrayBuffer :: JSString -> JSM Uint8Array
base64ToArrayBuffer jss = do
  fromB64 <- eval $
    "(function (base64) {\n" ++
    "  return Uint8Array.from(atob(base64), c => c.charCodeAt(0));" ++
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
