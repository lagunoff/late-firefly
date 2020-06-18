{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI #-}
{-# LANGUAGE CPP #-}
module LateFirefly.RPC.TH where

import Flat
import Data.ByteString
import Data.IORef
import Data.Maybe
import Data.Coerce
import Control.Monad.IO.Class
import Control.Monad
import qualified Data.Set as S
import qualified Data.Map as M
import System.IO.Unsafe
import Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax
import Data.Either
import LateFirefly.DB
import Language.Javascript.JSaddle
import JavaScript.Web.XMLHttpRequest
import Data.Reflection
import Debug.Trace
import qualified Data.ByteString.Base64 as B64
import GHC.StaticPtr
import Data.Text.Encoding (decodeUtf8)
import GHC.Fingerprint.Type
import Unsafe.Coerce

#ifndef ghcjs_HOST_OS
import JavaScript.TypedArray.Internal.Types
#else
import JavaScript.TypedArray
import Unsafe.Coerce
import GHCJS.Buffer as Buffer
#endif

reifyName :: String -> Q Name
reifyName s = recover assumeLocal
  $ TH.reify (mkName s) >>= \case
    VarI n _ _ -> pure n
  where
    assumeLocal = do
      loc <- location
      pure $ Name (OccName s) (NameG VarName (PkgName (loc_package loc)) (ModName (loc_module loc)))

type (:->) a r = Given Connection => a -> IO r

newtype Ep = Ep {unEp :: ByteString :-> ByteString}

type EpMap = M.Map Name Ep

data RpcRequest = RpcRequest
  { rr_key  :: StaticKey
  , rr_name :: Name
  , rr_arg  :: ByteString }
  deriving (Show, Eq, Generic, Flat)

sendRpc_client :: (Flat a, Flat r) => Name -> StaticKey -> (a :-> r) -> a -> JSM r
sendRpc_client n k _ a = do
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

epRef :: IORef (S.Set Name)
epRef = unsafePerformIO (newIORef S.empty)

sendRpc :: Name -> Q Exp
sendRpc n = do
#ifdef ghcjs_HOST_OS
  [| liftJSM . sendRpc_client $(lift n) (staticKey $(staticE ([|toEp|] `appE` varE n))) $(varE n) |]
#else
  liftIO $ modifyIORef epRef (S.insert n)
  [| liftJSM . sendRpc_client $(lift n) (staticKey $(staticE ([|toEp|] `appE` varE n))) $(varE n) |]
#endif

toEp :: forall a b. (Flat a, Flat b, Show b) => (a :-> b) -> Ep
toEp f = Ep (fmap (flat @b) . f . fromRight (error "unflat error") . (unflat @a))

rpcEps :: Q Exp
rpcEps = do
  eps <- liftIO (readIORef epRef)
  epsE <- forM (S.toAscList eps) \n -> do
    pure $ tupE [lift n, varE 'toEp `appE` varE n]
  [| M.fromList $(listE epsE) |]

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
