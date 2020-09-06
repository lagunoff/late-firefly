{-# LANGUAGE CPP, IncoherentInstances #-}
module DB.Transaction where

import Control.Lens hiding (As)
import Control.Monad.Catch
import Data.Aeson
import Data.Generics.Product
import Data.List as L
import Data.Text as T
import Data.Time
import Database.SQLite.Simple (Connection)
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Database.SQLite3 (ColumnType(..))
import System.Environment
import qualified Database.SQLite.Simple as S

import "this" DB.Base as DB
import "this" DB.QQ
import "this" DB.TH
import "this" Intro

data CaughtException e
  = KnownException e
  | UnknownException String
  deriving stock (Eq, Show, Generic)
  deriving (FromField, ToField) via JsonField (CaughtException e)
  deriving anyclass (FromJSON, ToJSON)

instance
  ( Typeable e
  , FromJSON e, ToJSON e
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

deriveDbDef ''Transaction

newVersion :: (?conn::Connection, (<:) S.SQLError e, Exception e) => ((?version::NewVersion) => Eio e a) -> Eio e a
newVersion = newVersionOrContinue False

newVersionOrContinue
  :: (?conn::Connection, As S.SQLError e)
  => Bool -> ((?version::NewVersion) => Eio e a) -> Eio e a
newVersionOrContinue continue act = do
  let
    takeTransaction = \case
      False -> do
        startedAt <- liftIO $ getCurrentTime
        upsertInc (Transaction def True startedAt Nothing Nothing)
      True -> select
        [sql|where rowid=(select max(rowid) from `transaction`) and finished_at is null|]
        >>= maybe (takeTransaction False) pure . fmap fst . L.uncons
  t <- takeTransaction continue
  let newTr = getField @"rowid" t
  a <- Eio $ let ?version = newTr in fmap Right (unEio act)
    `catchSync` (pure . Left)
  currTime <- liftIO getCurrentTime
  let finished_at = bool Nothing (Just currTime) $ isRight a
  upsertInc (t {finished_at, exception = either (Just . T.pack . displayException) (const Nothing) a})
  DB.execute
    [sql|update `transaction` set active=0 where active=1 and rowid <> {newTr}|]
  either (Eio . throwM) pure a

withConnection
  :: (As S.SQLError e)
  => FilePath -> ((?conn::Connection) => Eio e a) -> Eio e a
withConnection path act = Eio $ S.withConnection path \conn -> unEio do
  withEio (review _S) $ Eio @S.SQLError $ mapM_ (S.execute_ conn) ["PRAGMA journal_mode=WAL"]
  let ?conn = conn in act

withConnectionEnv
  :: (As SQLError e) => ((?conn::Connection) => Eio e a) -> Eio e a
withConnectionEnv f = do
  e <- liftIO $ lookupEnv "DB"
  withConnection (e ?: "late-firefly.sqlite") f

type NewVersion = Id Transaction

transaction :: (?conn::Connection, As SQLError e) => Eio e a -> Eio e a
transaction act = begin *> act <* commit `catch` rollback where
  begin = execute [sql|begin transaction;|]
  commit = execute [sql|commit;|]
  rollback e = execute [sql|rollback;|] *> throwM (e::SomeException)
