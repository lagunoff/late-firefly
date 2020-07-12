{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}
module Main where

import Control.Lens
import Data.Generics.Product
import Data.Text as T
import Data.Text.IO as T
import Flat
import LateFirefly.Backend
import LateFirefly.DB
import LateFirefly.Index
import LateFirefly.Prelude
import Massaraksh
import Options.Generic
import qualified Database.SQLite.Simple as S

#ifndef ghcjs_HOST_OS
import Language.Javascript.JSaddle.WebSockets as Warp
import LateFirefly.IMDB.Scrape as IMDB
import LateFirefly.RPC
import LateFirefly.Router.Wai
import LateFirefly.TheOffice.Scrape as TheOffice
import Network.Wai
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Gzip
import System.Environment
import System.IO
import qualified Network.HTTP.Types as H

-- | Command line options
data Opts
  = TheOffice {dbpath :: Maybe Text}
  | IMDB {dbpath :: Maybe Text}
  | Start {dbpath :: Maybe Text, port :: Maybe Int, docroot :: Maybe Text}
  | Migrate {dbpath :: Maybe Text}
  | PrintSchema
  deriving (Show, Generic, ParseRecord)

mainWith :: Opts -> IO ()
mainWith = \case
  TheOffice{dbpath=mayDb} -> do
    let defDb = T.unpack $ getField @"dbPath" (def @WebOpts)
    let dbpath = maybe defDb T.unpack mayDb
    withConnection dbpath do
      for_ $(mkDatabaseSetup) execute_
      TheOffice.scrapeSite
  IMDB{dbpath=mayDb} -> do
    let defDb = T.unpack $ getField @"dbPath" (def @WebOpts)
    let dbpath = maybe defDb T.unpack mayDb
    withConnection dbpath do
      for_ $(mkDatabaseSetup) execute_
      IMDB.scrapeSite
  Start{dbpath=mayDb, docroot=mayDR, port=mayPort, ..} -> do
    let
      port = getField @"webPort" opts
      docroot = fromMaybe "./" mayDR
      opts = (def :: WebOpts) & field @"webPort" %~ (maybe id const mayPort)
        & field @"dbPath" %~ (maybe id const mayDb)
      dbpath = getField @"dbPath" opts
    S.withConnection (T.unpack dbpath) \conn -> let
      rpcApp = let ?conn = conn in mkApplication $readDynSPT
      withGzip = gzip def
        { gzipFiles=GzipPreCompressed GzipIgnore
        , gzipCheckMime=const True }
      ss404Handler = Just $ html5Router $ withGzip $ staticApp
        $ defaultFileServerSettings (T.unpack docroot)
      staticApp' = withGzip $ staticApp
        $ (defaultFileServerSettings (T.unpack docroot)) {ss404Handler=ss404Handler}
      sett = setServerName "" . setPort port $ defaultSettings
      in Warp.runSettings sett \case
        req@(pathInfo -> "rpc":_) -> rpcApp req
        req                       -> staticApp' req
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
  let dbPath = T.unpack $ getField @"dbPath" (def :: WebOpts)
  conn <- S.open dbPath
  let rpcApp = let ?conn = conn in mkApplication $readDynSPT
  Warp.debugOr 7900 (void $ attachToBody indexWidget) \case
    req@(pathInfo -> "rpc":_) -> rpcApp req
    _                         ->
      ($ responseLBS H.status404 [("Content-Type", "text/plain")] "Not found")

main = do
  args <- getArgs
  withArgs (case args of "--":rest -> rest; xs -> xs) $
    getRecord "Web site with tons of free videos" >>= mainWith
#else
main = do
  void $ attachToBody indexWidget
#endif

type ConnectionPool = forall r. (Connection -> IO r) -> IO r
