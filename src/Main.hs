{-# LANGUAGE CPP #-}
module Main where

import Control.Lens
import Control.Monad.IO.Unlift
import Data.Generics.Product
import Data.Text as T
import Data.Text.IO as T
import Flat.Rpc.Main
import LateFirefly.Backend
import LateFirefly.DB
import LateFirefly.Index
import LateFirefly.Prelude
import Massaraksh
import Options.Generic
import qualified Database.SQLite.Simple as S
import GHC.StaticPtr

#ifndef ghcjs_HOST_OS
import Flat.Rpc.Wai
import LateFirefly.TheOffice.Scrape as TheOffice
import Network.Wai
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Gzip
import System.Environment
import System.IO

-- | Command line options
data Opts
  = TheOffice {dbpath :: Maybe Text}
  | Start {dbpath :: Maybe Text, port :: Maybe Int, docroot :: Maybe Text}
  | Migrate {dbpath :: Maybe Text}
  | PrintSchema
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
  Start{dbpath=mayDb, docroot=mayDR, port=mayPort, ..} -> do
    print =<< staticPtrKeys
    let
      port = getField @"webPort" opts
      docroot = fromMaybe "./" mayDR
      opts = def & field @"webPort" %~ (maybe id const mayPort)
        & field @"dbPath" %~ (maybe id const mayDb)
      dbpath = getField @"dbPath" opts
    S.withConnection (T.unpack dbpath) \conn -> let
      cfg = webServer ($ conn) opts
      sApp = staticApp $ defaultFileServerSettings (T.unpack docroot)
      fApp = flatRpcApplication @Backend (mcfEvalServer cfg)
      withGzip = gzip def {gzipFiles=GzipPreCompressed GzipIgnore, gzipCheckMime=const True}
      in Warp.run port \case
        req@(pathInfo -> "rpc":_) -> fApp req
        req                       -> withGzip sApp req
  Migrate{dbpath=mayDb,..} -> do
    let
      defDb = T.unpack $ getField @"dbPath" (def @WebOpts)
      dbpath = maybe defDb T.unpack mayDb
    withConnection dbpath do for_ $(mkDatabaseSetup) execute_
  PrintSchema -> do
    let qSchema = T.intercalate ";\n\n" $ fmap fromQuery $(mkDatabaseSetup)
    T.putStrLn qSchema

update :: IO ()
update = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  let defDb = T.unpack $ getField @"dbPath" (def @WebOpts)
  runFlat $ webServer (S.withConnection defDb) def {webPort = 7900}

main = do
  args <- getArgs
  withArgs (case args of "--":rest -> rest; xs -> xs) $
    getRecord "Web site with tons of free videos" >>= mainWith
#else
main = do
  print =<< staticPtrKeys
  runFlat (webServer undefined def {webPort = 7900})
#endif

type ConnectionPool = forall r. (Connection -> IO r) -> IO r

webServer :: ConnectionPool -> WebOpts -> MainConfig _ _
webServer wConn WebOpts{..} =
  MainConfig @Backend (Just webPort) (wConn . flip runBackend) \runCli -> do
    un <- askUnliftIO
    void $ attachToBody (unliftIO un . runCli) indexWidget
