{-# LANGUAGE NamedFieldPuns, CPP, StaticPointers, TypeFamilies, FlexibleInstances, OverloadedStrings #-}
module Main
  ( greet
  , main
  ) where
import Haste.App
import Data.IORef
import GHC.StaticPtr
import Control.Monad (when, forM_)
import IWatchTheOffice.Db
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.QQ
import qualified TheOffice.Home as Home
import TheOffice.Utils (liftJSM)

type MyS = EnvServer (IORef Int)

instance Node MyS where
  type Env MyS = IORef Int
  init _ = liftIO $ newIORef 0


greet :: RemotePtr (String -> MyS [Season])
greet = static (native $ remote $ \s -> do
  liftIO $ withConnection "./test.db" $ \conn -> do
    [Only updateId] <- query_ conn "select max(rowid) from updates where finished_at not null" :: IO [Only Int]
    seasons <- query conn "select code, thumbnail, href from iwatchtheoffice_seasons where update_id=?" (Only updateId) :: IO [Season]
    pure seasons
  )

main = runApp [start (Proxy :: Proxy MyS)] $ do
  seasons <- dispatch greet ""
  liftIO $ liftJSM $ Home.main seasons
