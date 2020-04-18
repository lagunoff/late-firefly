{-# LANGUAGE CPP #-}
module Main where

import Control.Lens
import Control.Monad.IO.Unlift
import Data.Text as T
import LF.DB
import LF.Prelude
import Options.Generic
import Data.Generics.Product
import Flat.Rpc.Main
import LF.Backend
import LF.Index
import Massaraksh
import Massaraksh
import qualified Database.SQLite.Simple as S

#ifndef ghcjs_HOST_OS
import System.Environment
import System.IO
import LF.TheOffice.Scrape as TheOffice

-- | Command line options
data Opts
  = TheOffice {dbpath :: Maybe Text}
  | Start {dbpath :: Maybe Text, webPort :: Maybe Int}
  deriving (Show, Generic, ParseRecord)

mainWith :: Opts -> IO ()
mainWith = \case
  TheOffice{dbpath=mayDb} -> do
    let
      defDb = T.unpack $ getField @"dbPath" (def @WebOpts)
      dbpath = maybe defDb T.unpack mayDb
    withConnection dbpath do
      for_ $(mkDatabaseSetup) execute_
      TheOffice.scrapeSite
  Start{..} -> do
    let
      defDb = T.unpack $ getField @"dbPath" (def @WebOpts)
      opts = def & field @"webPort" %~ (maybe id const webPort)
        & field @"dbPath" %~ (maybe id const dbpath)
    S.withConnection defDb \conn -> webServer ($ conn) opts

update :: IO ()
update = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  let defDb = T.unpack $ getField @"dbPath" (def @WebOpts)
  webServer (S.withConnection defDb) (def {webPort = 7900})

main = do
  args <- getArgs
  withArgs (case args of "--":rest -> rest; xs -> xs) $
    getRecord "Web site with tons of free videos" >>= mainWith
#else
main = webServer undefined def
#endif

type ConnectionPool = forall r. (Connection -> IO r) -> IO r

webServer :: ConnectionPool -> WebOpts -> IO ()
webServer wConn WebOpts{..} = runFlat $
  MainConfig @Backend (Just webPort) (wConn . flip runBackend) \runCli -> do
    un <- askUnliftIO
    void $ attachToBody (unliftIO un . runCli) indexWidget
