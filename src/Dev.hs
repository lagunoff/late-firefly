module Dev where

import Control.Lens
import Control.Concurrent
import Data.ByteString.Builder as BS
import Data.IORef
import Data.Monoid ((<>))
import Foreign.Store
import Network.HTTP.Types as H
import Network.Wai
import Network.Wai.Handler.Warp
import System.Environment
import qualified Network.Wai as W

import "this" Intro

reloadJs :: BS.Builder
reloadJs =
  "var xhr = new XMLHttpRequest();\n" <>
  "xhr.open('POST', '/reload/', true);\n" <>
  "xhr.onreadystatechange = function() {\n" <>
  "  if(xhr.readyState === XMLHttpRequest.DONE && xhr.status === 200)\n" <>
  "  window.location.reload();\n" <>
  "};\n" <>
  "xhr.send();\n"

refreshMiddleware :: ((Response -> IO ResponseReceived) -> IO ResponseReceived) -> Middleware
refreshMiddleware refresh otherApp = \case
  req@(W.pathInfo -> "reload":_) -> refresh
  req                            -> otherApp req

runDebug :: Int -> Application -> IO ()
runDebug port application = do
  reloadMVar <- newEmptyMVar
  let
    storeId = 355
    middleware = refreshMiddleware \send -> do
      tryTakeMVar reloadMVar
      readMVar reloadMVar
      send $ W.responseLBS H.status200
        [("Content-Type", "application/json")] "reload"
    start = do
      applicationRef <- newIORef (application, reloadMVar)
      forkIO do
        runSettings (setPort port defaultSettings) \req resp -> do
          app <- fmap middleware (fmap fst (readIORef applicationRef))
          app req resp
      pure applicationRef
  lookupStore storeId >>= \case
    Nothing -> do
      applicationRef <- start
      void $ writeStore (Store storeId) applicationRef
    Just applicationStore -> do
      applicationRef :: IORef (Application, MVar ()) <- readStore applicationStore
      reloadMVar <- atomicModifyIORef applicationRef
        \(_, mvar) -> ((application, mvar), mvar)
      void $ tryPutMVar reloadMVar ()

runOr :: Int -> Application -> IO ()
runOr port app = do
  progName <- getProgName
  putStrLn $ "<a href=\"http://localhost:" <> show port <> "\">run</a>"
  app & if progName == "<interactive>"
    then runDebug port
    else runSettings (setPort port defaultSettings)
