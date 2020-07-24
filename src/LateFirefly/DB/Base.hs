{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module LateFirefly.DB.Base
  ( ColumnInfo(..)
  , TableInfo(..)
  , DbTable(..)
  , DbField(..)
  , createTableStmt
  , execute
  , query
  , upsert
  , upsertVersion
  , upsertVersionConflict
  -- , upsert'
  -- , upsertWith
  , selectFrom
  , JsonField(..)
  , ReadShowField(..)
  , uuid5FromBS
  , DeriveUUID(..)
  , fixUUID
  , escText
  ) where

import Control.Lens
import Data.ByteString as BS
import Data.ByteString.Builder as B
import Data.ByteString.Internal as BS
import Data.Function
import Data.List as L
import Data.Generics.Product
import Data.Text as T
import qualified Data.Map as M
import Data.Text.Encoding as T
import Data.Text.Lazy as T (toStrict)
import Data.Time.Clock
import Data.UUID (UUID)
import Data.UUID.Types as U
import Data.UUID.V5
import Database.SQLite.Simple (Connection, Query(..), FromRow(..), ToRow(..), NamedParam, Only(..), SQLData)
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.Internal
import Database.SQLite.Simple.Ok
import Database.SQLite.Simple.ToField
import Database.SQLite3 (ColumnType(..))
import Flat
import GHC.Exception
import GHC.Int
import LateFirefly.Prelude
import LateFirefly.DB.QQ
import Text.Read
import qualified Database.SQLite.Simple as S
import qualified GHC.Records as G
#ifndef __GHCJS__
import Data.Aeson as AE
import Data.Aeson.Text as AE
#endif

data ColumnInfo = ColumnInfo
  { colType    :: ColumnType
  , nullable   :: Bool
  , uniqueKey  :: Bool
  , foreignKey :: Maybe (Text, [Text])
  -- ^ Tuple contains table name and the field name
  } deriving (Eq, Show, Generic)

data TableInfo = TableInfo
  { name    :: Text
  , columns :: [(Text, ColumnInfo)]
  , prio    :: Int }
  deriving stock (Eq, Show, Generic)

escText :: Text -> Text
escText t = "'" <> t <> "'"

isVersioned :: forall t. DbTable t => Bool
isVersioned = isJust $ L.lookup "version" $ getField @"columns" (tableInfo @t)

createTableStmt :: forall t. DbTable t => [Sql]
createTableStmt = let
  TableInfo{..} = tableInfo @t
  createColumns = columns <&> \(k, t) -> esc k <> " " <> ppColumnDesc t
  tableName = bool name (name <> "_versions") (isVersioned @t)
  pkeys = ["rowid"] <> bool [] ["version"] (isVersioned @t)
  primaryContrains = ["primary key(" <> T.intercalate ", " (fmap esc pkeys) <> ")"]
  createTable = [sql|create table if not exists {{tableName}} (
    #{T.intercalate ",\n  " (createColumns <> primaryContrains)}
  )|]
  selectColumns = T.intercalate ", " $ fmap (esc . fst) columns
  createView = [sql|create view if not exists {{name}} as
    select #{selectColumns} from
      (select x.* FROM #{tableName} x
        left join `transaction` t on t.rowid = x.version
        where
          t.rowid <= (select max(rowid) from `transaction` where active=1)
          and t.finished_at not null
        order by version desc)
     group by rowid having deleted=0|]
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

execute :: (?conn::Connection) => Sql -> IO ()
execute (Sql q [] _) = S.execute_ ?conn (Query q)
execute (Sql q p _) = S.execute ?conn (Query q) p

data UpsertResult
  = New | Updated | Cached | Rewritten
  deriving (Eq, Show, Generic)

upsert :: forall t g. (?conn::Connection, DbTable t, HasField' "rowid" t (Id g)) => t -> IO t
upsert t = do
  let
    TableInfo{..} = tableInfo @t
    tableName = bool name (name <> "_versions") (isVersioned @t)
    cols = T.intercalate ", " $ fmap (esc . fst) columns
    vals = T.intercalate ", " $ fmap (const "?") columns
    sets = T.intercalate ", " $ fmap (\(c, _) -> esc c <> " = ?") columns
  let q = "INSERT INTO " <> esc tableName <> " (" <> cols <> ") VALUES (" <> vals <> ") ON CONFLICT(rowid) DO UPDATE SET " <> sets
  S.execute ?conn (Query q) (toRow t <> toRow t)
  if getField @"rowid" t /= def then pure t else
     S.lastInsertRowId ?conn <&> \idInt -> setField @"rowid" (Id idInt) t

upsertVersion
  :: forall t. (?conn::Connection, Eq t, DbTable t) => t -> IO (t, UpsertResult)
upsertVersion = upsertVersionOpts Nothing

upsertVersionConflict
  :: forall t. (?conn::Connection, Eq t, DbTable t)
  => (t -> t -> t) -> t -> IO (t, UpsertResult)
upsertVersionConflict f = upsertVersionOpts (Just f)

upsertVersionOpts
  :: forall t. (?conn::Connection, Eq t, DbTable t)
  => Maybe (t -> t -> t) -> t -> IO (t, UpsertResult)
upsertVersionOpts mf t = do
  let
    TableInfo{..} = tableInfo @t
    tableName = bool name (name <> "_versions") (isVersioned @t)
    cols = T.intercalate ", " $ fmap (esc . fst) columns
    vals = T.intercalate ", " $ fmap (const "?") columns
    sets = T.intercalate ", " $ fmap (\(c, _) -> esc c <> " = ?") columns
    -- FIXME: Implementation implies that HasVersion only supports uuid PKs
  let row = toRow t
  let versionIdx = L.findIndex ((=="version") . fst) columns
  let kvs = L.zip (fmap fst columns) row
  let pkVersion = liftA2 (,) (L.lookup "rowid" kvs) (L.lookup "version" kvs)
  let existingQ = liftA2 (curry fst) pkVersion mf <&> \(ri, vr) -> [sql|select #{cols} from {{tableName}} where rowid={ri} AND version={vr}|]
  let lastQ = pkVersion <&> \(pk, _) -> [sql|select #{cols} from {{name}} where rowid={pk}|]
  let Sql q _ _ = [sql|insert into {{tableName}} (#{cols}) values (#{vals}) on conflict(rowid, version) do update set #{sets}|]
  mayExist::Maybe t <- join <$> for existingQ \q -> fmap fst . L.uncons <$> doQuery q
  case liftA2 (,) mayExist mf of
    Just (x, f) -> (t, Rewritten) <$ S.execute ?conn (Query q) (toRow (f x t) <> toRow (f x t))
    Nothing -> do
      mayLast::Maybe [SQLData] <- join <$> for lastQ \q -> fmap fst . L.uncons <$> doQuery q
      case (flip rowsEq row <$> versionIdx <*> mayLast) of
        (Just True)  -> pure (t, Cached)
        (Just False) -> (t, Updated) <$ S.execute ?conn (Query q) (row <> row)
        Nothing      -> (t, New) <$ S.execute ?conn (Query q) (row <> row)
      where
        rowsEq :: Eq a => Int -> [a] -> [a] -> Bool
        rowsEq _ [] [] = True
        rowsEq _ _ []  = False
        rowsEq _ [] _  = False
        rowsEq 0 (_:xs) (_:ys) = rowsEq (-1) xs ys
        rowsEq n (x:xs) (y:ys) = x == y && rowsEq (max (-1) (n - 1)) xs ys

selectFrom
  :: forall t p
  . (?conn::Connection, DbTable t) => Sql -> IO [t]
selectFrom (Sql qTail p _) = do
  let
    TableInfo{..} = tableInfo @t
    cols = T.intercalate ", " $ fmap (esc . fst) columns
    q = "SELECT " <> cols <> " FROM " <> esc name <> " " <> qTail
  S.query ?conn (Query q) p

newtype JsonField a = JsonField {unJsonField :: a}

#ifndef __GHCJS__
instance ToJSON a => ToField (JsonField a) where
  toField = S.SQLText . T.toStrict . AE.encodeToLazyText . unJsonField

instance (FromJSON a, Typeable a) => FromField (JsonField a) where
  fromField  = textFieldParser $
    fmap JsonField . AE.eitherDecode @a . B.toLazyByteString .
    T.encodeUtf8Builder
#else
instance ToField (JsonField a) where toField = error "Unimplemented"
instance (Typeable a) => FromField (JsonField a) where fromField = error "Unimplemented"
#endif

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

class DeriveUUID a where
  uuidSalt :: a -> ByteString

fixUUID :: (DeriveUUID a, HasField' "uuid" a (UUID5 a)) => (UUID5 a -> a) -> a
fixUUID f = fix (f . uuid5FromBS . uuidSalt)

class (FromRow a, ToRow a) => DbTable a where
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

instance DbField ByteString where
  columnInfo _ = ColumnInfo BlobColumn False False Nothing

instance (DbTable t, Typeable (Id t)) => DbField (Id t) where
  columnInfo _ = let TableInfo{..} = tableInfo @t in
    ColumnInfo IntegerColumn False False (Just (name, ["rowid"]))

instance (DbTable t, Typeable t) => DbField (UUID5 t) where
  columnInfo _ = let TableInfo{..} = tableInfo @t in
    ColumnInfo IntegerColumn False False (Just (name, ["rowid"]))

instance DbField UTCTime where
  columnInfo _ = ColumnInfo TextColumn False False Nothing

#ifndef __GHCJS__
instance (Typeable a, FromJSON a, ToJSON a) => DbField [a] where
  columnInfo _ = ColumnInfo TextColumn False False Nothing
instance (FromJSON (M.Map k v), ToJSON (M.Map k v), Typeable k, Typeable v) => DbField (M.Map k v) where
  columnInfo _ = ColumnInfo TextColumn False False Nothing
#else
instance (Typeable a) => DbField [a] where columnInfo _ = error "Unimplemented"
instance (Typeable k, Typeable v) => DbField (M.Map k v) where columnInfo _ = error "Unimplemented"
#endif

instance ToField (UUID5 t) where
  toField = S.SQLText . U.toText . unUUID5

instance Typeable (UUID5 t) => FromField (UUID5 t) where
  fromField = textFieldParser (fmap UUID5 . note "Cannot read UUID" . U.fromText)

instance DbField Bool where
  columnInfo _ = ColumnInfo IntegerColumn False False Nothing

deriving newtype instance DbField (Tid t)
deriving newtype instance ToField (Tid t)
deriving newtype instance FromField (Tid t)

#ifndef __GHCJS__
deriving via JsonField [a] instance (FromJSON a, Typeable a) => FromField [a]
deriving via JsonField [a] instance ToJSON a => ToField [a]
deriving via JsonField (M.Map k v) instance (FromJSON (M.Map k v), Typeable k, Typeable v) => FromField (M.Map k v)
deriving via JsonField (M.Map k v) instance ToJSON (M.Map k v) => ToField (M.Map k v)
deriving newtype instance FromJSON (Tid t)
deriving newtype instance ToJSON (Tid t)
#else
deriving via JsonField [a] instance (Typeable a) => FromField [a]
deriving via JsonField [a] instance ToField [a]
deriving via JsonField (M.Map k v) instance (Typeable k, Typeable v) => FromField (M.Map k v)
deriving via JsonField (M.Map k v) instance ToField (M.Map k v)
#endif
