{-# LANGUAGE IncoherentInstances #-}
module LateFirefly.RPC (
  module LateFirefly.RPC.TH,
  mkApplication
) where

import LateFirefly.RPC.TH
import Network.Wai
import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Types.Status
import Data.ByteString as BS
import qualified Data.Map as M
import Flat
import GHC.StaticPtr
import Control.Applicative
import LateFirefly.Prelude
import LateFirefly.DB

mkApplication :: Given Connection => DynSPT -> Application
mkApplication dynSpt req response = do
  RpcRequest{..} <- either (error . show) id <$> parseFlatRequest
  staticEp :: Maybe Ep <- fmap deRefStaticPtr <$> unsafeLookupStaticPtr rr_key
  let
    dynEp = M.lookup rr_name dynSpt
    ep = fromMaybe (error $ "unknown method: " ++ show rr_name) $ staticEp <|> dynEp
  r <- unEp ep rr_arg
  response $ responseLBS ok200 [] (LBS.fromStrict r)
  where
    parseFlatRequest = do
      requestBody :: ByteString <- getRequestBody BS.empty
      pure (unflat @RpcRequest requestBody)

    getRequestBody head = do
      chunk <- getRequestBodyChunk req
      if chunk == BS.empty
        then pure (head <> chunk)
        else getRequestBody (head <> chunk)
