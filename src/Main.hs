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
import LateFirefly.TheOffice.Scrape as TheOffice
import Network.Wai
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Gzip
import System.Environment
import System.IO
import Language.Javascript.JSaddle.WebSockets as Warp
import qualified Network.HTTP.Types as H
import LateFirefly.RPC
import LateFirefly.Router.Wai

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
    let
      port = getField @"webPort" opts
      docroot = fromMaybe "./" mayDR
      opts = (def :: WebOpts) & field @"webPort" %~ (maybe id const mayPort)
        & field @"dbPath" %~ (maybe id const mayDb)
      dbpath = getField @"dbPath" opts
    S.withConnection (T.unpack dbpath) \conn -> let
      rpcApp = let ?conn = conn in mkApplication $readDynSPT
      withGzip = gzip def {gzipFiles=GzipPreCompressed GzipIgnore, gzipCheckMime=const True}
      sApp = html5Router $ withGzip $ staticApp $ defaultFileServerSettings (T.unpack docroot)
      in Warp.run port \case
        req@(pathInfo -> "rpc":_) -> rpcApp req
        req                       -> sApp req
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
  Warp.debugOr 7900 (void $ attachToBodySimple indexWidget) \case
    req@(pathInfo -> "rpc":_) -> rpcApp req
    _                         ->
      ($ responseLBS H.status404 [("Content-Type", "text/plain")] "Not found")

main = do
  args <- getArgs
  withArgs (case args of "--":rest -> rest; xs -> xs) $
    getRecord "Web site with tons of free videos" >>= mainWith
#else
main = do
  void $ attachToBodySimple indexWidget
#endif

type ConnectionPool = forall r. (Connection -> IO r) -> IO r
