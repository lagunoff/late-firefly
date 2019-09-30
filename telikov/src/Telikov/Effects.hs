{-# LANGUAGE TemplateHaskell #-}
module Telikov.Effects
  ( module Network.Wreq
  , module Polysemy
  , module Telikov.Database
  , UTCTime(..), Query(..)
  , CurrentTime(..), currentTime, time2IO, Http(..), httpGet, http2JSM, http2IO
  , io2jsm
  , emit, RPC(..), remoteRequest, mapMessages, Eval, Init, Exists(..), evaluateMessages
  ) where 

import Polysemy
import Polysemy.State
import Data.Time.Clock.POSIX (getCurrentTime)
import Data.Time.Clock (UTCTime(..))
import Network.Wreq (responseBody)
import qualified Network.Wreq as Wreq
import qualified Data.ByteString.Lazy as L
import Language.Javascript.JSaddle (JSM)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value)
import Haste.App.Protocol (Endpoint(..), Nonce)
import Telikov.Database

data Http m a where
  HttpGet :: String -> Http m (Wreq.Response L.ByteString)
makeSem ''Http

http2IO :: Member (Embed IO) r => Sem (Http ': r) a -> Sem r a
http2IO = interpret \case
  HttpGet url -> embed (putStrLn $ "GET " <> url) *> embed (Wreq.get url)

http2JSM :: Member (Embed JSM) r => Sem (Http ': r) a -> Sem r a
http2JSM = interpret \case
  HttpGet url -> embed (liftIO $ Wreq.get url)

io2jsm :: Member (Embed JSM) r => Sem (Embed IO ': r) a -> Sem r a
io2jsm = interpret $ embed . liftIO . unEmbed

data Query msg m a where
  Emit :: msg a -> Query msg m a
makeSem ''Query

mapMessages :: forall msg1 msg2 r a. (Member (Query msg2) r) => (forall b. msg1 b -> msg2 b) -> Sem (Query msg1 ': r) a -> Sem r a
mapMessages t = interpret \case
  Emit msg1 -> emit (t msg1)

evaluateMessages :: forall msg r a. (forall b. msg b -> Sem (Query msg ': r) b) -> Sem (Query msg ': r) a -> Sem r a
evaluateMessages eval = interpret \case
  Emit msg -> evaluateMessages eval (eval msg)

data RPC m a where
  RemoteRequest :: Endpoint -> String -> Nonce -> RPC m Value
makeSem ''RPC

type Eval model msg a = forall r. Members '[State model, Query msg, Http, RPC, Embed IO] r => Sem r a
type Init model = forall r. Members '[Http, RPC, Embed IO] r => Sem r model

data Exists f = forall a. Exists { runExist :: f a }
