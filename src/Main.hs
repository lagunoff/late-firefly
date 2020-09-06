{-# LANGUAGE CPP #-}
module Main where

import Control.Lens
import Data.Constraint
import Data.Generics.Product
import Data.Text as T
import Data.Text.IO as T
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Gzip
import Options.Generic
import System.Environment
import System.IO
import qualified Database.SQLite.Simple as S

import "this" DB
import "this" Dev
import "this" Index ()
import "this" Intro
import "this" Router
import "this" Router.Wai
import "this" Series ()
import "this" Series.Episode ()

pages =
  [ PageDict (Dict @(IsPage "HomeR" _)), PageDict (Dict @(IsPage "SeriesR" _))
  , PageDict (Dict @(IsPage "EpisodeR" _)) ]

data WebOpts = WebOpts
  { webPort :: Int
  , dbPath  :: Text }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ParseRecord)

instance Default WebOpts where
  def = WebOpts 8080 "./late-firefly.sqlite"

-- | Command line options
data Opts
  = Start        {dbpath::Maybe Text, port::Maybe Int, docroot::Maybe Text}
  | Migrate      {dbpath::Maybe Text}
  | PrintSchema
  deriving (Show, Generic, ParseRecord)

mainOpts :: Opts -> IO ()
mainOpts = \case
  Start{dbpath=mayDb, docroot=mayDR, port=mayPort, ..} -> do
    let
      port = getField @"webPort" opts
      docroot = fromMaybe "./" mayDR
      opts = (def::WebOpts) & field @"webPort" %~ (maybe id const mayPort)
        & field @"dbPath" %~ (maybe id const mayDb)
      dbpath = getField @"dbPath" opts
    S.withConnection (T.unpack dbpath) \conn -> let ?conn = conn in let
      withGzip = gzip def
        { gzipFiles=GzipPreCompressed GzipIgnore
        , gzipCheckMime=const True }
      ss404Handler = Just $ html5Router pages
      staticApp' = withGzip $ staticApp
        $ (defaultFileServerSettings (T.unpack docroot)) {ss404Handler=ss404Handler}
      sett = setServerName "" . setPort port $ defaultSettings
      in Warp.runSettings sett staticApp'
  Migrate{dbpath=mayDb,..} -> do
    let
      defDb = T.unpack $ getField @"dbPath" (def @WebOpts)
      dbpath = maybe defDb T.unpack mayDb
    unEio @_ @SQLError $ withConnection dbpath do for_ $mkDatabaseSetup execute
  PrintSchema -> do
    let qSchema = T.intercalate ";\n\n" $ fmap (\(Sql x _ _) -> x) $mkDatabaseSetup
    T.putStrLn qSchema

update :: IO ()
update = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  let dbPath = T.unpack $ getField @"dbPath" (def :: WebOpts)
  conn <- S.open dbPath
  let site = let ?conn = conn in html5Router pages
  runOr 7900 site

main = do
  args <- getArgs
  withArgs (case args of "--":rest -> rest; xs -> xs) $
    getRecord "Web site with tons of free videos" >>= mainOpts
