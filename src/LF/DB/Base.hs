module LF.DB.Base
  ( Id(..)
  , UUID5(..)
  , ColumnDesc(..)
  , TableDesc(..)
  , DbTable(..)
  , DbField(..)
  , createTableStmt
  , withConnection
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
  ) where

import Control.Error.Util
import Control.Lens
import Data.Aeson as AE
import Data.Aeson.Text as AE
import Data.ByteString.Builder as B
import Data.Generics.Product
import Data.List as L
import Data.Text as T
import Data.Text.Encoding as T
import Data.Text.Lazy as T (toStrict)
import Data.Time.Clock
import Data.UUID (UUID)
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
import Text.Read
import qualified Database.SQLite.Simple as S
import qualified GHC.Records as G

newtype Id t = Id {unId :: Int64}
  deriving stock (Eq, Ord, Show)
  deriving newtype Flat

newtype UUID5 t = UUID5 {unUUID5 :: UUID}
  deriving stock (Show, Read, Eq, Typeable, Generic)
  deriving newtype Flat
  deriving (FromField, ToField) via ReadShowField (UUID5 t)

data ColumnDesc = ColumnDesc
  { colType    :: ColumnType
  , nullable   :: Bool
  , uniqueKey  :: Bool
  , foreignKey :: Maybe (Text, [Text])
  -- ^ Tuple contains table name and the field name
  } deriving (Eq, Show, Generic)

data TableDesc = TableDesc
  { name    :: Text
  , primary :: [Text]
  , columns :: [(Text, ColumnDesc)]
  } deriving (Eq, Show, Generic)

esc :: Text -> Text
esc t = "`" <> t <> "`"

createTableStmt :: forall t. DbTable t => [Query]
createTableStmt = let
  cs = getField @"columns" $ (tableDesc @t)
  name = getField @"name" (tableDesc @t)
  primaryKey = getField @"primary" (tableDesc @t)
  columns = cs <&> \(k, t) -> esc k <> " " <> ppColumnDesc t
  foreignContrains = cs & mapMaybe \case
    (k, ColumnDesc{foreignKey=Just(ft, ff), ..}) ->
      if L.all (k /=) primaryKey then
        Just $ "foreign key(" <> esc k <> ") references " <> esc ft <> "(" <> T.intercalate ", " (fmap esc (L.filter (/="version") ff)) <> ")"
      else Nothing
    _ -> Nothing
  primaryContrains = ["primary key(" <> T.intercalate ", " (fmap esc primaryKey) <> ")"]
  in [Query $ "CREATE TABLE IF NOT EXISTS " <> esc name <> " (\n  "
  <> T.intercalate ",\n  " (columns <> foreignContrains <> primaryContrains)
  <> "\n)"]

ppColumnDesc :: ColumnDesc -> Text
ppColumnDesc ColumnDesc{..} =
  [ Just (ppColumnType colType)
  , bool (Just "NOT NULL") Nothing nullable ]
  & catMaybes & T.intercalate " "

ppColumnType :: ColumnType -> Text
ppColumnType = \case
  IntegerColumn -> "INTEGER"
  FloatColumn   -> "DOUBLE"
  TextColumn    -> "TEXT"
  BlobColumn    -> "BLOB"
  NullColumn    -> "NULL"

withConnection :: FilePath -> (Given Connection => IO a) -> IO a
withConnection path act = S.withConnection path \conn -> do
  mapM_ (S.execute_ conn)
    ["PRAGMA foreign_keys = ON", "PRAGMA journal_mode=WAL"]
  give conn act

execute :: (Given Connection, ToRow p) => Query -> p -> IO ()
execute = S.execute given

execute_ :: Given Connection => Query -> IO ()
execute_ = S.execute_ given

query :: (FromRow r, ToRow p, Given Connection) => Query -> p -> IO [r]
query = S.query given

query_ :: (FromRow r, Given Connection) => Query -> IO [r]
query_ = S.query_ given

upsert
  :: forall t n
   . Given Connection
  => DbTable t
  => HasField' "rowid" t (Id n)
  => t -> IO t
upsert t = do
  let
    name = getField @"name" (tableDesc @t)
    columns = getField @"columns" (tableDesc @t)
    cols = T.intercalate ", " $ fmap fst columns
    vals = T.intercalate ", " $ fmap (const "?") columns
    sets = T.intercalate ", " $ fmap (\(c, _) -> esc c <> " = ?") columns
    q = "INSERT INTO " <> esc name <> " (" <> cols <> ") VALUES (" <> vals <> ") ON CONFLICT(rowid) DO UPDATE SET " <> sets
  S.execute given (Query q) (toRow t <> toRow t)
  if getField @"rowid" t /= def then pure t else
    S.lastInsertRowId given <&> \idInt -> setField @"rowid" (Id idInt) t

selectFrom
  :: forall t p
  . (Given Connection, DbTable t, ToRow p) => Query -> p -> IO [t]
selectFrom qTail p = do
  let
    name = getField @"name" (tableDesc @t)
    q = "SELECT rowid, * FROM " <> esc name <> " " <> fromQuery qTail
  S.query given (Query q) p

selectFrom_ :: forall t. (Given Connection, DbTable t) => Query -> IO [t]
selectFrom_ qTail = do
  let
    name = getField @"name" (tableDesc @t)
    q = "SELECT rowid, * FROM " <> esc name <> " " <> fromQuery qTail
  S.query_ given (Query q)

selectFromNamed
  :: forall t. (Given Connection, DbTable t) => Query -> [NamedParam] -> IO [t]
selectFromNamed qTail p = do
  let
    name = getField @"name" (tableDesc @t)
    q = "SELECT rowid, * FROM " <> esc name <> " " <> fromQuery qTail
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

class (FromRow a, ToRow a) => DbTable a where
  tableDesc :: TableDesc

class (FromField a, ToField a) => DbField a where
  fieldDesc :: Proxy a -> ColumnDesc

-- | By convention uninitialized id's have -1 values inside them
instance Default (Id t) where def = Id (-1)

instance Typeable (Id t) => FromField (Id t) where
  fromField = \case
    Field (S.SQLInteger i) _ -> Ok $ Id i
    Field S.SQLNull _        -> Ok $ Id (-1)
    f                      -> returnError ConversionFailed f "need an int"

instance ToField (Id t) where
  toField = \case
    Id (-1) -> S.SQLNull
    Id i    -> S.SQLInteger i

instance DbField Int where
  fieldDesc _ = ColumnDesc IntegerColumn False False Nothing

instance DbField Text where
  fieldDesc _ = ColumnDesc TextColumn False False Nothing

instance DbField a => DbField (Maybe a) where
  fieldDesc _ = (fieldDesc (Proxy @a)) { nullable = True }

instance (DbTable t, Typeable (Id t)) => DbField (Id t) where
  fieldDesc _ = ColumnDesc IntegerColumn False False $ Just (tName, fName)
    where
      fName = getField @"primary" (tableDesc @t)
      tName = getField @"name" (tableDesc @t)

instance (DbTable t, Typeable t) => DbField (UUID5 t) where
  fieldDesc _ = ColumnDesc TextColumn False False $ Just (tName, fName)
    where
      fName = getField @"primary" (tableDesc @t)
      tName = getField @"name" (tableDesc @t)

instance DbField UTCTime where
  fieldDesc _ = ColumnDesc IntegerColumn False False Nothing

instance (Typeable a, FromJSON a, ToJSON a) => DbField [a] where
  fieldDesc _ = ColumnDesc TextColumn False False Nothing
