{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StaticPointers    #-}
{-# LANGUAGE TypeFamilies      #-}
module Main
  ( mainClient
  , main
  ) where

import           Control.Monad                  (when)
import           Data.IORef
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow
import           Database.SQLite.Simple.QQ
import           GHC.StaticPtr
import           Haste.App
import           IWatchTheOffice.Db
import qualified TheOffice.Home                 as Home
import           TheOffice.Utils                (liftJSM)
import           TheOffice.RPC (greet, mainRPC)
import GHCJS.DOM.Types (JSM)

mainClient :: JSM ()
mainClient = do
  seasons <- dispatch greet ""
  Home.main seasons

main :: IO ()
main = mainRPC
