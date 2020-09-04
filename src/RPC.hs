{-# LANGUAGE IncoherentInstances #-}
module RPC (
  module RPC.TH,
  mkApplication
) where

import Control.Applicative
import Data.ByteString as BS
import Flat
import GHC.StaticPtr
import "this" DB
import "this" Intro
import "this" RPC.TH
import Network.HTTP.Types.Status
import Network.Wai
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as M

mkApplication :: (?conn::Connection) => DynSPT -> Application
mkApplication dynSpt req response = do
  RpcRequest{..} <- either (error . show) id <$> parseFlatRequest
  staticEp :: Maybe SomeBackend <- fmap deRefStaticPtr <$> unsafeLookupStaticPtr rpcqKey
  let
    dynEp = M.lookup rpcqName dynSpt
    ep = fromMaybe (error $ "unknown method: " ++ show rpcqName) $ staticEp <|> dynEp
  r <- runBackend ep rpcqArg
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
