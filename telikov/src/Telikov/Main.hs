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

import GHCJS.DOM.Types (JSM)
import Telikov.RPC (mainRPC)
import qualified Telikov.Home as Home

mainClient :: JSM ()
mainClient = do
  Home.main =<< Home.init

main :: IO ()
#ifndef __GHCJS__
main = mainRPC
#else
main = mainClient
#endif
