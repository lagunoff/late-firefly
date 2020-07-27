{-# LANGUAGE CPP #-}
module LateFirefly.DB.Transaction where

import Data.Generics.Product
import Data.Time
import Data.Text as T
import Data.List as L
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Flat
import LateFirefly.DB.Base as DB
import LateFirefly.DB.QQ
import LateFirefly.DB.TH
import LateFirefly.Prelude
import qualified Database.SQLite.Simple as S
import Database.SQLite3 (ColumnType(..))
import Control.Exception as E
#ifndef __GHCJS__
import Data.Aeson
#endif

data CaughtException e
  = KnownException e
  | UnknownException String
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Flat)
  deriving (FromField, ToField) via JsonField (CaughtException e)
#ifndef __GHCJS__
  deriving anyclass (FromJSON, ToJSON)
#endif

instance
  ( Typeable e
#ifndef __GHCJS__
  , FromJSON e, ToJSON e
#endif
  ) =>
  DbField (CaughtException e) where
    columnInfo _ = ColumnInfo TextColumn False False Nothing

data Transaction = Transaction
  { rowid       :: Id Transaction
  , active      :: Bool
  , started_at  :: UTCTime
  , finished_at :: Maybe UTCTime
  , exception   :: Maybe Text }
  deriving stock (Eq, Show, Generic)
  deriving anyclass Flat

deriveDbDef ''Transaction

newVersion :: (?conn::Connection) => ((?version::NewVersion) => IO a) -> IO a
newVersion = newVersionOrContinue False

newVersionOrContinue :: (?conn::Connection) => Bool -> ((?version::NewVersion) => IO a) -> IO a
newVersionOrContinue continue act = do
  let
    takeTransaction = \case
      False -> do
        startedAt <- getCurrentTime
        upsertInc (Transaction def True startedAt Nothing Nothing)
      True -> selectFrom
        [sql|where rowid=(select max(rowid) from `transaction`) and finished_at is null|]
        >>= maybe (takeTransaction False) pure . fmap fst . L.uncons
  t <- takeTransaction continue
  let newTr = getField @"rowid" t
  a <- let ?version = newTr in fmap Right act
    `catchSync` (pure . Left)
  currTime <- getCurrentTime
  let finished_at = bool Nothing (Just currTime) $ isRight a
  upsertInc (t {finished_at, exception = either (Just . T.pack . displayException) (const Nothing) a})
  DB.execute
    [sql|update `transaction` set active=0 where active=1 and rowid <> {newTr}|]
  either throwIO pure a

withConnection :: FilePath -> ((?conn::Connection) => IO a) -> IO a
withConnection path act = S.withConnection path \conn -> do
  mapM_ (S.execute_ conn) ["PRAGMA journal_mode=WAL"]
  let ?conn = conn in act

type NewVersion = Id Transaction
