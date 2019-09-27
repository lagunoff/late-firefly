{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
module Telikov.Effects
  ( module Network.Wreq
  , module Polysemy
  , UTCTime(..), RowID(..)
  , SQL(..), query, query_, execute, execute_, lastInsertRowId, sql2IO
  , CurrentTime(..), currentTime, time2IO, Http(..), httpGet, http2JSM, http2IO
  , io2jsm
  , Query, emit, RPC(..), remoteRequest, mapMessages, Eval, Init, Exists(..), evaluateMessages
  ) where 

import Database.SQLite.Simple (Connection, FromRow, ToRow)
import Data.Int (Int64)
import Polysemy
import Polysemy.State
import qualified Database.SQLite.Simple as SQLite
import Data.Time.Clock.POSIX (getCurrentTime)
import Data.Time.Clock (UTCTime(..))
import Network.Wreq (responseBody)
import qualified Network.Wreq as Wreq
import qualified Data.ByteString.Lazy as L
import Language.Javascript.JSaddle (JSM)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON, Value)
import Haste.App.Protocol (Endpoint(..), Nonce)
import GHC.TypeLits
import GHC.Generics
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField

newtype RowID t = RowID { unRowID :: Int64 }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
  deriving (ToField, FromField) via Int64

data SQL m a where
  Query :: (ToRow q, FromRow row) => SQLite.Query -> q -> SQL m [row]
  Query_ :: (FromRow row) => SQLite.Query -> SQL m [row]
  Execute :: (ToRow q) => SQLite.Query -> q -> SQL m ()
  Execute_ :: SQLite.Query -> SQL m ()
  LastInsertRowId :: KnownSymbol t => SQL m (RowID t)
makeSem_ ''SQL

query :: forall row q eff. (FromRow row, ToRow q, Member SQL eff) => SQLite.Query -> q -> Sem eff [row]
query_ :: forall row eff. (FromRow row, Member SQL eff) => SQLite.Query -> Sem eff [row]
execute :: forall q eff. (ToRow q, Member SQL eff) => SQLite.Query -> q -> Sem eff ()
execute_ :: forall eff. (Member SQL eff) => SQLite.Query -> Sem eff ()
lastInsertRowId :: forall eff t. (KnownSymbol t, Member SQL eff) => Sem eff (RowID t)

sql2IO :: Member (Embed IO) r => Connection -> Sem (SQL ': r) a -> Sem r a
sql2IO conn = interpret \case
  Query q p       -> embed $ SQLite.query conn q p
  Query_ q        -> embed $ SQLite.query_ conn q
  Execute q p     -> embed $ SQLite.execute conn q p
  Execute_ q      -> embed $ SQLite.execute_ conn q
  LastInsertRowId -> embed $ RowID <$> SQLite.lastInsertRowId conn

data CurrentTime m a where
  CurrentTime :: CurrentTime m UTCTime
makeSem ''CurrentTime

time2IO :: Member (Embed IO) r => Sem (CurrentTime ': r) a -> Sem r a
time2IO = interpret \case
  CurrentTime -> embed getCurrentTime

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
