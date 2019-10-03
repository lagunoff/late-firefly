{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE UndecidableInstances, QuantifiedConstraints, MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Telikov.Database where

import Database.SQLite.Simple (ToRow, FromRow, Connection, SQLData(..), field)
import Database.SQLite.Simple.Ok (Ok(..))
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.FromRow (RowParser)
import Database.SQLite.Simple.Internal (Field(..))
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import GHC.TypeLits
import Data.Int (Int64)
import GHC.Exts
import Control.Applicative
import Control.Lens ((&))
import Data.Aeson (withText, FromJSON(..), ToJSON(..))
import Data.Time.Clock (getCurrentTime, UTCTime(..))
import Polysemy
import Polysemy (Embed)
import Polysemy.Reader
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Data.UUID.V5
import Data.Typeable
import Data.Word (Word8)

-- Type class for obtaining sql table name
class GHasTable f where
  gtableName :: Text

class HasUUID5 a where
  uuid5Object :: a -> [Word8]
  
-- Type class for default implementation of key-value record serialization
class GPersistent f where
  gklist :: [Text]
  gkvlist :: f p -> [(Text, SQLData)]

class Persistent a where
  tableName :: Text
  default tableName :: (Generic a, GHasTable (Rep a)) => Text
  tableName = gtableName @(Rep a)
  
  klist :: [Text]
  default klist :: (Generic a, GPersistent (Rep a)) => [Text]
  klist = gklist @(Rep a)
  
  kvlist :: a -> [(Text, SQLData)]
  default kvlist :: (Generic a, GPersistent (Rep a)) => a -> [(Text, SQLData)]
  kvlist = gkvlist . from

-- instance Persistent fields => Persistent (Row fields) where
--   tableName = tableName @fields
--   klist = ["id", "version", "deleted"] <> klist @fields
--   kvlist (Row _id version deleted fields) = [("id", toField _id), ("version", toField version), ("deleted", toField deleted)] <> kvlist fields

newtype RowID t = RowID { unRowID :: Int64 }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
  deriving (ToField, FromField) via Int64

newtype UUID5 t = UUID5 { unUUID5 :: UUID }
  deriving (Show, Eq, Typeable)

data SQL m a where
  Query :: (ToRow q, FromRow row) => SQLite.Query -> q -> SQL m [row]
  Query_ :: (FromRow row) => SQLite.Query -> SQL m [row]
  Execute :: (ToRow q) => SQLite.Query -> q -> SQL m ()
  Execute_ :: SQLite.Query -> SQL m ()
  LastInsertRowId :: SQL m (RowID t)
makeSem_ ''SQL

query :: forall row q eff. (FromRow row, ToRow q, Member SQL eff) => SQLite.Query -> q -> Sem eff [row]
query_ :: forall row eff. (FromRow row, Member SQL eff) => SQLite.Query -> Sem eff [row]
execute :: forall q eff. (ToRow q, Member SQL eff) => SQLite.Query -> q -> Sem eff ()
execute_ :: forall eff. (Member SQL eff) => SQLite.Query -> Sem eff ()
lastInsertRowId :: forall eff t. (Member SQL eff) => Sem eff (RowID t)

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

withNewTransaction
  :: forall a r
   . (Members '[SQL, CurrentTime] r)
  => Sem (Reader (RowID Transaction) ': r) a
  -> Sem r a
withNewTransaction block = do
  transaction <- flip Transaction Nothing <$> currentTime
  version <- insertRow transaction
  runReader version block <* do
    finishedAt <- currentTime
    execute "update \"transaction\" set finished_at=(?) where rowid=(?)" (finishedAt, version)

data Transaction = Transaction
  { started_at  :: UTCTime
  , finished_at :: Maybe UTCTime
  } deriving (Show, Eq, Generic, ToJSON, FromJSON, Persistent)

instance (KnownSymbol name) => GHasTable (M1 c ('MetaData name m p n) f) where
  gtableName = T.toLower $ T.pack $ symbolVal' (proxy# :: Proxy# name)

-- Type class for default implementation of FromRow using generics
class GFromRow f where
  gFromRow :: RowParser (f p)

instance GFromRow f => GFromRow (M1 c i f) where
  gFromRow = M1 <$> gFromRow

instance (GFromRow f, GFromRow g) => GFromRow (f :*: g) where
  gFromRow = liftA2 (:*:) gFromRow gFromRow

instance FromField a => GFromRow (K1 R a) where
  gFromRow = K1 <$> field

instance GFromRow U1 where
  gFromRow = pure U1
      
-- Type class for default implementation of ToRow generics
class GToRow f where
  gToRow :: f p -> [SQLData]

instance GToRow f => GToRow (M1 c i f) where
  gToRow (M1 x) = gToRow x

instance (GToRow f, GToRow g) => GToRow (f :*: g) where
  gToRow (f :*: g) = gToRow f ++ gToRow g

instance (ToField a) => GToRow (K1 R a) where
  gToRow (K1 a) = [toField a]

instance GToRow U1 where
  gToRow _ = []

instance {-# OVERLAPS #-} GPersistent f => GPersistent (M1 c i f) where
  gklist = gklist @f
  gkvlist (M1 x) = gkvlist x

instance (GPersistent f, KnownSymbol sym) => GPersistent (M1 c ('MetaSel ('Just sym) m1 m2 m3) f) where
  gklist = [T.pack $ symbolVal' (proxy# :: Proxy# sym)]
  gkvlist (M1 x) = gkvlist x & fmap \(_, v) -> (T.pack $ symbolVal' (proxy# :: Proxy# sym), v)
      
instance (GPersistent f, GPersistent g) => GPersistent (f :*: g) where
  gklist = gklist @f ++ gklist @g
  gkvlist (f :*: g) = gkvlist f ++ gkvlist g

instance (ToField a) => GPersistent (K1 R a) where
  gklist = []
  gkvlist (K1 a) = [("", toField a)]

instance GPersistent U1 where
  gklist = []
  gkvlist _ = []

uuid5 :: HasUUID5 a => a -> UUID5 a
uuid5 = UUID5 . generateNamed namespaceURL . (uuid5Object $)

instance (Typeable (UUID5 a)) => FromField (UUID5 a) where
  fromField f@(Field (SQLText t) _) = case UUID.fromText t of
    Just uuid -> Ok $ UUID5 uuid
    Nothing   -> returnError ConversionFailed f "Invalid UUID value"
  fromField f                     = returnError ConversionFailed f "expecting SQLText column type"

instance ToField (UUID5 a) where
  toField = toField . UUID.toText . unUUID5

instance FromJSON (UUID5 a) where
  parseJSON = withText "UUID" \s -> case UUID.fromText s of
    Just uuid -> pure $ UUID5 uuid
    _ -> fail "Invalid UUID"

instance ToJSON (UUID5 a) where
  toJSON = toJSON . UUID.toText . unUUID5

insert :: forall a r. (HasUUID5 a, Persistent a, Member SQL r, Member (Reader (RowID Transaction)) r) => a -> Sem r (UUID5 a)
insert a = do
  version <- SQLInteger . unRowID <$> ask @(RowID Transaction)
  let uuid = SQLText . UUID.toText . unUUID5 $ uuid5 a
  let columns = klist @a & T.intercalate ","
  let questions = klist @a & fmap (const "?") & T.intercalate "," 
  let values = kvlist a & fmap snd & ([uuid, version] <>)
  let insertQuery = "insert into \"" <> tableName @a <> "\" (uuid, version, " <> columns <> ") values (?, ?, " <> questions <> ")"
  execute (SQLite.Query insertQuery) values
  pure (uuid5 a)

insertRow :: forall a r. (Persistent a, Member SQL r) => a -> Sem r (RowID a)
insertRow a = do
  let columns = klist @a & T.intercalate ","
  let questions = klist @a & fmap (const "?") & T.intercalate "," 
  let values = kvlist a & fmap snd
  let insertQuery = "insert into \"" <> tableName @a <> "\" (" <> columns <> ") values (" <> questions <> ")"
  execute (SQLite.Query insertQuery) values *> lastInsertRowId

select :: forall a r p. (Persistent a, FromRow a, ToRow p, Member SQL r) => SQLite.Query -> p -> Sem r [a]
select (SQLite.Query txt) params = do
  let columns = klist @a & T.intercalate ","
  query (SQLite.Query $ "select " <> columns <> " from \"" <> tableName @a <> "_latest\" " <> txt) params

select_ :: forall a r. (Persistent a, FromRow a, Member SQL r) => SQLite.Query -> Sem r [a]
select_ (SQLite.Query txt) = do
  let columns = klist @a & T.intercalate ","
  query_ (SQLite.Query $ "select " <> columns <> " from \"" <> tableName @a <> "_latest\" " <> txt)



