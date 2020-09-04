{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module DB.Base
  ( ColumnInfo(..)
  , TableInfo(..)
  , DbTable(..)
  , DbColumns(..)
  , DbField(..)
  , createTableStmt
  , execute
  , query
  , select
  , upsert
  , upsertInc
  , upsertVersion
  , upsertVersionConflict
  , JsonField(..)
  , ReadShowField(..)
  , uuid5FromBS
  , DeriveUUID(..)
  , fixUUID
  , escText
  , SqlIO
  , S.SQLError
  ) where

import Control.Lens hiding (As)
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Aeson as AE
import Data.Aeson.Text as AE
import Data.ByteString as BS
import Data.ByteString.Builder as B
import Data.ByteString.Internal as BS
import Data.Generics.Product
import Data.List as L
import Data.Text as T
import Data.Text.Encoding as T
import Data.Text.Lazy as T (toStrict)
import Data.Time.Clock
import Data.UUID.Types as U
import Data.UUID.V5
import Database.SQLite.Simple (Connection, Query(..), FromRow(..), ToRow(..), SQLData(..))
import Database.SQLite.Simple.FromField (FromField(..), FieldParser)
import Database.SQLite.Simple.Internal (Field(..), RowParser(..))
import Database.SQLite.Simple.Ok (Ok(..))
import Database.SQLite.Simple.ToField (ToField(..))
import Database.SQLite3 (ColumnType(..))
import Flat
import GHC.Exception
import "this" DB.QQ
import "this" Intro
import Text.Read
import qualified Data.Map as M
import qualified Database.SQLite.Simple as S
import qualified Database.SQLite.Simple.FromField as S
import qualified Database.SQLite.Simple.Internal as S
import qualified GHC.Records as G

type SqlIO = Eio S.SQLError

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
  , pkeys   :: [Text]
  , ukeys   :: [[Text]]
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
  pks = bool pkeys ["rowid"] (pkeys == []) <> bool [] ["version"] (isVersioned @t)
  primaryContrains = ["primary key(" <> T.intercalate ", " (fmap esc pks) <> ")"]
  fks = catMaybes $ columns <&> \case
    (k, ColumnInfo {foreignKey=Just (t, tks)})
      | k /= "rowid" -> Just $ "foreign key (" <> esc k <> ") references "
        <> esc t <> " (" <> T.intercalate ", " (fmap esc tks) <> ")"
    _ -> Nothing
  uks = ukeys <&> \ks -> "unique (" <> T.intercalate ", " (fmap esc ks) <> ")"
  createTable = [sql|create table if not exists {{tableName}} (
    #{T.intercalate ",\n  " (createColumns <> primaryContrains <> fks <> uks)}
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

execute :: (?conn::Connection, As S.SQLError e) => Sql -> Eio e ()
execute (Sql q [] _) = liftEio $ Eio @S.SQLError $ S.execute_ ?conn (Query q)
execute (Sql q p _) = liftEio $ Eio @S.SQLError $ S.execute ?conn (Query q) p

query :: (?conn::Connection, As S.SQLError e, FromRow r) => Sql -> Eio e [r]
query (Sql t [] _) = liftEio $ Eio @S.SQLError $ S.query_ ?conn (Query t)
query (Sql t p _)  = liftEio $ Eio @S.SQLError $ S.query ?conn (Query t) p

select :: forall t e.
  (?conn::Connection, As S.SQLError e, DbTable t) => Sql -> Eio e [t]
select (Sql qTail p _) = do
  let
    TableInfo{..} = tableInfo @t
    cols = T.intercalate ", " $ fmap (esc . fst) columns
    q = "SELECT " <> cols <> " FROM " <> esc name <> " " <> qTail
  liftEio $ Eio @S.SQLError $ S.query ?conn (Query q) p

data UpsertResult
  = New | Updated | Cached | Rewritten
  deriving (Eq, Show, Generic)

upsertInc :: forall t e.
  (?conn::Connection, As S.SQLError e, DbTable t, HasField' "rowid" t (Id t))
  => t -> Eio e t
upsertInc t = do
  let
    TableInfo{..} = tableInfo @t
    tableName = bool name (name <> "_versions") (isVersioned @t)
    cols = T.intercalate ", " $ fmap (esc . fst) columns
    vals = T.intercalate ", " $ fmap (const "?") columns
    Sql q _ _ = [sql|replace into {{tableName}} (#{cols}) values (#{vals})|]
  liftEio $ Eio @S.SQLError $ S.execute ?conn (Query q) (toRow t)
  if getField @"rowid" t /= def then pure t else
     liftEio $ Eio @S.SQLError $ S.lastInsertRowId ?conn <&> \idInt -> setField @"rowid" (Id idInt) t

upsert :: forall t e.
  (?conn::Connection, As S.SQLError e, DbTable t) => t -> Eio e ()
upsert t = do
  let
    TableInfo{..} = tableInfo @t
    tableName = bool name (name <> "_versions") (isVersioned @t)
    cols = T.intercalate ", " $ fmap (esc . fst) columns
    vals = T.intercalate ", " $ fmap (const "?") columns
    Sql q _ _ = [sql|replace into {{tableName}} (#{cols}) values (#{vals})|]
  liftEio $ Eio @S.SQLError $ S.execute ?conn (Query q) (toRow t)

upsertVersion :: forall t e.
  (?conn::Connection, Eq t, DbTable t, As S.SQLError e)
  => t -> Eio e (t, UpsertResult)
upsertVersion = upsertVersionOpts Nothing

upsertVersionConflict
  :: forall t e. (?conn::Connection, Eq t, DbTable t, As S.SQLError e)
  => (t -> t -> t) -> t -> Eio e (t, UpsertResult)
upsertVersionConflict f = upsertVersionOpts (Just f)

upsertVersionOpts
  :: forall t e. (?conn::Connection, Eq t, DbTable t, As S.SQLError e)
  => Maybe (t -> t -> t) -> t -> Eio e (t, UpsertResult)
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
  mayExist::Maybe t <- join <$> for existingQ \q -> fmap fst . L.uncons <$> query q
  case liftA2 (,) mayExist mf of
    Just (x, f) -> Eio $ (t, Rewritten) <$ S.execute ?conn (Query q) (toRow (f x t) <> toRow (f x t))
    Nothing -> do
      mayLast::Maybe [SQLData] <- join <$> for lastQ \q -> fmap fst . L.uncons <$> query q
      Eio $ case (flip rowsEq row <$> versionIdx <*> mayLast) of
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

newtype JsonField a = JsonField {unJsonField :: a}

instance ToJSON a => ToField (JsonField a) where
  toField = S.SQLText . T.toStrict . AE.encodeToLazyText . unJsonField

instance (FromJSON a, Typeable a) => FromField (JsonField a) where
  fromField  = textFieldParser $
    fmap JsonField . AE.eitherDecode @a . B.toLazyByteString .
    T.encodeUtf8Builder

instance (ToJSON a, FromJSON a, Typeable a) => DbField (JsonField a) where
  columnInfo _ = ColumnInfo TextColumn False False Nothing

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
    sqlTy = T.unpack (decodeUtf8 (S.gettypename (G.getField @"result" fld)))
    haskTy = tyConName (typeRepTyCon (typeRep (Proxy @a)))
  in case fld of
    Field (S.SQLText txt) _ -> either
      (Errors . pure . SomeException . S.ConversionFailed sqlTy haskTy) Ok $ f txt
    Field _ _               ->
      Errors [SomeException (S.Incompatible sqlTy haskTy "")]

class DeriveUUID a where
  uuidSalt :: a -> ByteString

fixUUID :: (DeriveUUID a, HasField' "uuid" a (UUID5 a)) => (UUID5 a -> a) -> a
fixUUID f = fix (f . uuid5FromBS . uuidSalt)

class (FromRow a, ToRow a) => DbTable a where
  tableInfo :: TableInfo

class DbColumns a where
  columnsInfo :: [(Text, ColumnInfo)]

class (FromField a, ToField a) => DbField a where
  columnInfo :: Proxy a -> ColumnInfo

-- | By convention uninitialized id's have -1 values inside them
instance Default (Id t) where def = Id (-1)

instance Typeable (Id t) => FromField (Id t) where
  fromField = \case
    Field (S.SQLInteger i) _ -> Ok $ Id i
    Field S.SQLNull _        -> Ok $ Id (-1)
    f                        -> S.returnError S.ConversionFailed f "need an int"

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

instance (DbTable t, Typeable (Tid t)) => DbField (Tid t) where
  columnInfo _ = let TableInfo{..} = tableInfo @t in
    ColumnInfo TextColumn False False (Just (name, ["rowid"]))

instance (DbTable t, Typeable t) => DbField (UUID5 t) where
  columnInfo _ = let TableInfo{..} = tableInfo @t in
    ColumnInfo IntegerColumn False False (Just (name, ["rowid"]))

instance DbField UTCTime where
  columnInfo _ = ColumnInfo TextColumn False False Nothing

instance (Typeable a, FromJSON a, ToJSON a) => DbField [a] where
  columnInfo _ = ColumnInfo TextColumn False False Nothing
instance (FromJSON (M.Map k v), ToJSON (M.Map k v), Typeable k, Typeable v) => DbField (M.Map k v) where
  columnInfo _ = ColumnInfo TextColumn False False Nothing

instance ToField (UUID5 t) where
  toField = S.SQLText . U.toText . unUUID5

instance Typeable (UUID5 t) => FromField (UUID5 t) where
  fromField = textFieldParser (fmap UUID5 . note "Cannot read UUID" . U.fromText)

instance DbField Bool where
  columnInfo _ = ColumnInfo IntegerColumn False False Nothing

deriving newtype instance ToField (Tid t)
deriving newtype instance FromField (Tid t)

deriving via JsonField [a] instance (FromJSON a, Typeable a) => FromField [a]
deriving via JsonField [a] instance ToJSON a => ToField [a]
deriving via JsonField (M.Map k v) instance (FromJSON (M.Map k v), Typeable k, Typeable v) => FromField (M.Map k v)
deriving via JsonField (M.Map k v) instance ToJSON (M.Map k v) => ToField (M.Map k v)
deriving newtype instance FromJSON (Tid t)
deriving newtype instance ToJSON (Tid t)

-- | Count the number of fields in a record
class GCountFields f where
  countFields :: Proxy (f p) -> Int

instance GCountFields f => GCountFields (D1 c f) where
  countFields _ = countFields (Proxy::Proxy(f x))

instance Constructor c => GCountFields (C1 c U1) where
  countFields _ = 0

instance (GCountFields f, Constructor c) => GCountFields (C1 c f) where
  countFields _ = countFields (Proxy::Proxy(f x))

instance Selector s => GCountFields (S1 s f) where
  countFields _ = 1

instance (GCountFields f, GCountFields g) => GCountFields ((:*:) f g) where
  countFields _ = countFields (Proxy::Proxy(f x)) + countFields (Proxy::Proxy(g x))

instance (ToRow a, GCountFields (Rep a)) => ToRow (Maybe a) where
  toRow Nothing = L.take (countFields (Proxy::Proxy(Rep a _))) $ L.repeat S.SQLNull
  toRow (Just x) = toRow x

instance (FromRow a, GCountFields (Rep a)) => FromRow (Maybe a) where
  fromRow = RP $ ReaderT \r -> StateT \s@(n, cols) ->
    let flds = countFields (Proxy::Proxy(Rep a _)) in
    case flip runStateT s $ flip runReaderT r $ unRP $ fromRow @a of
      Errors _  -> Ok (Nothing, (n - flds, L.drop flds cols))
      Ok (a, s) -> Ok (Just a, s)
