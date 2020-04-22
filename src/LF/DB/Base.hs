{-# OPTIONS_GHC -fno-warn-orphans #-}
module LF.DB.Base
  ( Id(..)
  , UUID5(..)
  , ColumnInfo(..)
  , TableInfo(..)
  , DbTable(..)
  , DbField(..)
  , PKInfo(..)
  , createTableStmt
  , execute
  , execute_
  , query
  , query_
  , upsert
  , selectFrom
  , selectFrom_
  , selectFromNamed
  , JsonField(..)
  , ReadShowField(..)
  , uuid5FromBS
  , DeriveUUID(..)
  , fixUUID
  ) where

import Control.Lens
import Data.Aeson as AE
import Data.Aeson.Text as AE
import Data.ByteString as BS
import Data.ByteString.Builder as B
import Data.ByteString.Internal as BS
import Data.Function
import Data.Generics.Product
import Data.Text as T
import Data.Text.Encoding as T
import Data.Text.Lazy as T (toStrict)
import Data.Time.Clock
import Data.UUID (UUID)
import Data.UUID.Types as U
import Data.UUID.V5
import Database.SQLite.Simple (Connection, Query(..), FromRow(..), ToRow(..), NamedParam)
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.Internal
import Database.SQLite.Simple.Ok
import Database.SQLite.Simple.ToField
import Database.SQLite3 (ColumnType(..))
import Flat.Rpc
import GHC.Exception
import GHC.Int
import LF.Prelude
import LF.DB.QQ
import Text.Read
import qualified Database.SQLite.Simple as S
import qualified GHC.Records as G

newtype Id t = Id {unId :: Int64}
  deriving stock (Eq, Ord, Show)
  deriving newtype Flat

newtype UUID5 t = UUID5 {unUUID5 :: UUID}
  deriving stock (Show, Read, Eq, Typeable, Generic)
  deriving newtype Flat

data ColumnInfo = ColumnInfo
  { colType    :: ColumnType
  , nullable   :: Bool
  , uniqueKey  :: Bool
  , foreignKey :: Maybe (Text, [Text])
  -- ^ Tuple contains table name and the field name
  } deriving (Eq, Show, Generic)

data TableInfo = TableInfo
  { name    :: Text
  , primary :: [Text]
  , columns :: [(Text, ColumnInfo)]
  , prio    :: Int
  } deriving (Eq, Show, Generic)

esc :: Text -> Text
esc t = "`" <> t <> "`"

isVersioned :: forall t. DbTable t => Bool
isVersioned = case pkInfo @t of
  HasVersion -> True
  _          -> False

createTableStmt :: forall t. DbTable t => [Query]
createTableStmt = let
  TableInfo{..} = tableInfo @t
  createColumns = columns <&> \(k, t) -> esc k <> " " <> ppColumnDesc t
  tableName = bool name (name <> "_versions") (isVersioned @t)
  primaryContrains = ["primary key(" <> T.intercalate ", " (fmap esc primary) <> ")"]
  createTable = [sql|CREATE TABLE IF NOT EXISTS #{esc tableName} (
    #{T.intercalate ",\n  " (createColumns <> primaryContrains)}
  )|]
  selectColumns = T.intercalate ", " $ fmap (esc . fst) columns
  createView = [sql|CREATE VIEW IF NOT EXISTS #{name} AS
    SELECT #{selectColumns} from
      (SELECT x.* FROM #{tableName} x
        LEFT JOIN `transaction` t ON t.rowid = x.version
        WHERE
          t.rowid <= (SELECT MAX(rowid) FROM `transaction` WHERE active=1)
          AND t.finished_at NOT NULL
        ORDER BY version DESC)
     GROUP BY uuid HAVING deleted=0|]
  in [createTable] <> bool mempty [createView] (isVersioned @t)

ppColumnDesc :: ColumnInfo -> Text
ppColumnDesc ColumnInfo{..} =
  [Just (ppColumnType colType), bool (Just "NOT NULL") Nothing nullable]
  & catMaybes & T.intercalate " "

ppColumnType :: ColumnType -> Text
ppColumnType = \case
  IntegerColumn -> "INTEGER"
  FloatColumn   -> "DOUBLE"
  TextColumn    -> "TEXT"
  BlobColumn    -> "BLOB"
  NullColumn    -> "NULL"

uuid5FromBS :: ByteString -> UUID5 a
uuid5FromBS = UUID5 . generateNamed namespaceURL . BS.unpackBytes

execute :: (Given Connection, ToRow p) => Query -> p -> IO ()
execute = S.execute given

execute_ :: Given Connection => Query -> IO ()
execute_ = S.execute_ given

query :: (FromRow r, ToRow p, Given Connection) => Query -> p -> IO [r]
query = S.query given

query_ :: (FromRow r, Given Connection) => Query -> IO [r]
query_ = S.query_ given

upsert :: forall t. (Given Connection, DbTable t) => t -> IO t
upsert t = do
  let
    TableInfo{..} = tableInfo @t
    tableName = bool name (name <> "_versions") (isVersioned @t)
    cols = T.intercalate ", " $ fmap (esc . fst) columns
    vals = T.intercalate ", " $ fmap (const "?") columns
    sets = T.intercalate ", " $ fmap (\(c, _) -> esc c <> " = ?") columns
  case pkInfo @t of
    NoVersion -> do
      let q = "INSERT INTO " <> esc tableName <> " (" <> cols <> ") VALUES (" <> vals <> ") ON CONFLICT(rowid) DO UPDATE SET " <> sets
      S.execute given (Query q) (toRow t <> toRow t)
      if getField @"rowid" t /= def then pure t else
        S.lastInsertRowId given <&> \idInt -> setField @"rowid" (Id idInt) t
    HasVersion -> do
      -- TODO: Check for duplicates with the previous version
      let q = "INSERT INTO " <> esc tableName <> " (" <> cols <> ") VALUES (" <> vals <> ")"
      t <$ S.execute given (Query q) (toRow t)

selectFrom
  :: forall t p
  . (Given Connection, DbTable t, ToRow p) => Query -> p -> IO [t]
selectFrom qTail p = do
  let
    TableInfo{..} = tableInfo @t
    cols = T.intercalate ", " $ fmap (esc . fst) columns
    q = "SELECT " <> cols <> " FROM " <> esc name <> " " <> fromQuery qTail
  S.query given (Query q) p

selectFrom_ :: forall t. (Given Connection, DbTable t) => Query -> IO [t]
selectFrom_ qTail = do
  let
    TableInfo{..} = tableInfo @t
    cols = T.intercalate ", " $ fmap (esc . fst) columns
    q = "SELECT " <> cols <> " FROM " <> esc name <> " " <> fromQuery qTail
  S.query_ given (Query q)

selectFromNamed
  :: forall t. (Given Connection, DbTable t) => Query -> [NamedParam] -> IO [t]
selectFromNamed qTail p = do
  let
    TableInfo{..} = tableInfo @t
    cols = T.intercalate ", " $ fmap (esc . fst) columns
    q = "SELECT " <> cols <> " FROM " <> esc name <> " " <> fromQuery qTail
  S.queryNamed given (Query q) p

newtype JsonField a = JsonField {unJsonField :: a}

instance ToJSON a => ToField (JsonField a) where
  toField = S.SQLText . T.toStrict . AE.encodeToLazyText . unJsonField

instance (FromJSON a, Typeable a) => FromField (JsonField a) where
  fromField  = textFieldParser $
    fmap JsonField . AE.eitherDecode @a . B.toLazyByteString .
    T.encodeUtf8Builder

newtype ReadShowField a = ReadShowField {unReadShowField :: a}

instance Show a => ToField (ReadShowField a) where
  toField = S.SQLText . T.pack . show . unReadShowField

instance (Read a, Typeable a) => FromField (ReadShowField a) where
  fromField = textFieldParser $
    fmap ReadShowField . note "read failed" . readMaybe @a . T.unpack

textFieldParser
  :: forall a. Typeable a => (Text -> Either String a) -> FieldParser a
textFieldParser f fld =
  let
    sqlTy = T.unpack (decodeUtf8 (gettypename (G.getField @"result" fld)))
    haskTy = tyConName (typeRepTyCon (typeRep (Proxy @a)))
  in case fld of
    Field (S.SQLText txt) _ -> either
      (Errors . pure . SomeException . ConversionFailed sqlTy haskTy) Ok $ f txt
    Field _ _               ->
      Errors [SomeException (Incompatible sqlTy haskTy "")]

data PKInfo t (v :: Bool) where
  NoVersion :: HasField' "rowid" t (Id t) => PKInfo t 'False
  HasVersion ::
    ( HasField' "uuid" t (UUID5 t)
    , HasField' "version" t (Id tr) )
    => PKInfo t 'True

class DeriveUUID a where
  uuidSalt :: a -> ByteString

fixUUID :: (DeriveUUID a, HasField' "uuid" a (UUID5 a)) => (UUID5 a -> a) -> a
fixUUID f = fix (f . uuid5FromBS . uuidSalt)

class (FromRow a, ToRow a) => DbTable a where
  type HasVersion a :: Bool
  pkInfo :: PKInfo a (HasVersion a)
  tableInfo :: TableInfo

class (FromField a, ToField a) => DbField a where
  columnInfo :: Proxy a -> ColumnInfo

-- | By convention uninitialized id's have -1 values inside them
instance Default (Id t) where def = Id (-1)

instance Typeable (Id t) => FromField (Id t) where
  fromField = \case
    Field (S.SQLInteger i) _ -> Ok $ Id i
    Field S.SQLNull _        -> Ok $ Id (-1)
    f                        -> returnError ConversionFailed f "need an int"

instance ToField (Id t) where
  toField = \case
    Id (-1) -> S.SQLNull
    Id i    -> S.SQLInteger i

instance DbField Int where
  columnInfo _ = ColumnInfo IntegerColumn False False Nothing

instance DbField Text where
  columnInfo _ = ColumnInfo TextColumn False False Nothing

instance DbField a => DbField (Maybe a) where
  columnInfo _ = (columnInfo (Proxy @a)) {nullable = True}

instance (DbTable t, Typeable (Id t)) => DbField (Id t) where
  columnInfo _ = ColumnInfo IntegerColumn False False $ Just (tName, fName)
    where
      fName = getField @"primary" (tableInfo @t)
      tName = getField @"name" (tableInfo @t)

instance (DbTable t, Typeable t) => DbField (UUID5 t) where
  columnInfo _ = ColumnInfo TextColumn False False $ Just (tName, fName)
    where
      fName = getField @"primary" (tableInfo @t)
      tName = getField @"name" (tableInfo @t)

instance DbField UTCTime where
  columnInfo _ = ColumnInfo TextColumn False False Nothing

instance (Typeable a, FromJSON a, ToJSON a) => DbField [a] where
  columnInfo _ = ColumnInfo TextColumn False False Nothing

instance ToField (UUID5 t) where
  toField = S.SQLText . U.toText . unUUID5

instance Typeable (UUID5 t) => FromField (UUID5 t) where
  fromField = textFieldParser (fmap UUID5 . note "Cannot read UUID" . U.fromText)

instance DbField Bool where
  columnInfo _ = ColumnInfo IntegerColumn False False Nothing

deriving via JsonField [a] instance (FromJSON a, Typeable a) => FromField [a]
deriving via JsonField [a] instance ToJSON a => ToField [a]
