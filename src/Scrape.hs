module Scrape where

import Data.Aeson as AE
import Control.Lens hiding (As)
import Network.HTTP.Client
import Network.HTTP.Types
import qualified Network.Wreq as Wreq
import qualified Data.ByteString.Lazy as BSL

import "this" Eio

httpGet :: forall e. (As HttpException e) => String -> Eio e (Response BSL.ByteString)
httpGet u = liftIOWith (review (_S @HttpException @e)) $ putStrLn (u <> "...") *> Wreq.getWith opts u
  where
    opts = Wreq.defaults & Wreq.headers .~ [(hUserAgent, ua), (hAcceptLanguage, lang)]
    ua = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/81.0.4044.122 Safari/537.36"
    lang = "en-US;q=0.9,en;q=0.8"

getJSON
  :: forall a e. (FromJSON a, As HttpException e)
  => String -> Eio e a
getJSON u = do
  either error id . AE.eitherDecode' @a . (^. Wreq.responseBody) <$> httpGet u
